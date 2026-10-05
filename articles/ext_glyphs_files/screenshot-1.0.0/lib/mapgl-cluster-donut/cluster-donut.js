/* Donut cluster images, drawn on demand.
 *
 * Donut cluster layers (cluster_options(donut_column = ...)) are symbol
 * layers whose icon-image expression evaluates to a name like
 *
 *   mapgl-donut|<fingerprint>|<radius>|<u0>,<u1>,...|<layer id>
 *
 * where each u is a category's rounded share in resolution units. No
 * image by that name exists, so the map fires `styleimagemissing`; this
 * handler draws the donut on a canvas and registers it with addImage.
 *
 * The handler is stateless: slice colors and ring styling come from the
 * layer's metadata["mapgl:donut"], so proxy-added layers, style reloads,
 * and compare maps need no registry. The fingerprint in the name must
 * match the layer's current spec, so a request left over from a replaced
 * layer draws nothing instead of caching an image with the wrong colors.
 *
 * Both Mapbox GL JS and MapLibre GL JS fire styleimagemissing
 * synchronously and check for the image right after the listeners run,
 * so drawing and addImage must happen before the handler returns.
 *
 * MapLibre GL JS v6 replaces this event with setMissingStyleImageResolver.
 */
(function () {
  "use strict";

  const PREFIX = "mapgl-donut|";

  function parseName(name) {
    // The layer id is everything after the fourth "|", so ids may contain "|"
    const parts = name.split("|");
    if (parts.length < 5) return null;
    const radius = Number(parts[2]);
    const units = parts[3].split(",").map(Number);
    if (!(radius > 0) || units.some((u) => !Number.isFinite(u) || u < 0)) {
      return null;
    }
    return {
      fingerprint: parts[1],
      radius: radius,
      units: units,
      layerId: parts.slice(4).join("|"),
    };
  }

  function rgba(hex, alpha) {
    const value = parseInt(hex.slice(1), 16);
    return (
      "rgba(" +
      ((value >> 16) & 255) +
      "," +
      ((value >> 8) & 255) +
      "," +
      (value & 255) +
      "," +
      (alpha == null ? 1 : alpha) +
      ")"
    );
  }

  function drawDonut(parsed, spec, pixelRatio) {
    const radius = parsed.radius;
    const strokeWidth = spec.stroke_width || 0;
    const outer = radius + strokeWidth;
    const size = Math.ceil(outer * 2 * pixelRatio);
    const canvas = document.createElement("canvas");
    canvas.width = size;
    canvas.height = size;
    // willReadFrequently keeps the canvas on the CPU; without it,
    // getImageData forces a GPU readback that can stall for hundreds of ms
    const ctx = canvas.getContext("2d", { willReadFrequently: true });
    ctx.scale(pixelRatio, pixelRatio);

    const center = size / pixelRatio / 2;
    const inner = radius * (1 - spec.width);
    const total = parsed.units.reduce((a, b) => a + b, 0);

    if (total > 0) {
      let start = -Math.PI / 2;
      parsed.units.forEach((unit, i) => {
        if (!unit || !spec.colors[i]) return;
        const end = start + (unit / total) * 2 * Math.PI;
        ctx.beginPath();
        ctx.arc(center, center, radius, start, end);
        ctx.arc(center, center, inner, end, start, true);
        ctx.closePath();
        ctx.fillStyle = rgba(spec.colors[i], spec.alphas[i]);
        ctx.fill();
        start = end;
      });
    }

    if (spec.fill) {
      ctx.beginPath();
      ctx.arc(center, center, inner, 0, 2 * Math.PI);
      ctx.fillStyle = rgba(spec.fill, spec.fill_alpha);
      ctx.fill();
    }

    if (strokeWidth > 0 && spec.stroke) {
      ctx.beginPath();
      ctx.arc(center, center, radius + strokeWidth / 2, 0, 2 * Math.PI);
      ctx.lineWidth = strokeWidth;
      ctx.strokeStyle = rgba(spec.stroke, spec.stroke_alpha);
      ctx.stroke();
    }

    const image = ctx.getImageData(0, 0, size, size);
    return {
      width: size,
      height: size,
      data: new Uint8Array(image.data.buffer),
    };
  }

  function layerSpec(map, layerId) {
    const layer = map.getLayer(layerId);
    const metadata = layer && layer.metadata;
    return metadata ? metadata["mapgl:donut"] : null;
  }

  function attach(map) {
    if (!map || map._mapglClusterDonutAttached) return;
    map._mapglClusterDonutAttached = true;

    // Memory accounting for diagnostics. `liveBytes` is a running total
    // kept O(1) per image; it's reconciled against hasImage() only on
    // style reloads (which can drop images) and when memory() is read.
    const stats = {
      names: new Map(),
      liveBytes: 0,
      peakBytes: 0,
      createdCount: 0,
      cumulativeBytes: 0,
    };
    map._mapglClusterDonutStats = stats;
    const warned = new Set();

    map.on("style.load", function () {
      reconcile(map, stats);
    });

    map.on("styleimagemissing", function (e) {
      const name = e && e.id;
      if (typeof name !== "string" || name.indexOf(PREFIX) !== 0) return;
      if (map.hasImage(name)) return;

      const parsed = parseName(name);
      if (!parsed) return;
      const spec = layerSpec(map, parsed.layerId);
      if (!spec) {
        if (!warned.has(parsed.layerId)) {
          warned.add(parsed.layerId);
          console.warn(
            "mapgl: no donut settings found for layer '" +
              parsed.layerId +
              "'",
          );
        }
        return;
      }
      // Stale request from a replaced layer definition
      if (spec.fp !== parsed.fingerprint) return;

      try {
        const pixelRatio = window.devicePixelRatio || 1;
        const image = drawDonut(parsed, spec, pixelRatio);
        map.addImage(name, image, { pixelRatio: pixelRatio });
        const bytes = image.data.byteLength;
        stats.names.set(name, bytes);
        stats.createdCount++;
        stats.cumulativeBytes += bytes;
        stats.liveBytes += bytes;
        if (stats.liveBytes > stats.peakBytes) stats.peakBytes = stats.liveBytes;
      } catch (err) {
        console.warn("mapgl: failed to draw donut cluster image", err);
      }
    });
  }

  // Drop names the map no longer holds and recompute the running total
  function reconcile(map, stats) {
    let bytes = 0;
    stats.names.forEach(function (size, name) {
      if (map.hasImage(name)) {
        bytes += size;
      } else {
        stats.names.delete(name);
      }
    });
    stats.liveBytes = bytes;
    return bytes;
  }

  // Snapshot of image memory for a map: images and bytes currently held,
  // peak bytes held, and images and bytes generated over the map's lifetime.
  function memory(map) {
    const stats = map && map._mapglClusterDonutStats;
    if (!stats) return null;
    const retainedBytes = reconcile(map, stats);
    return {
      retainedCount: stats.names.size,
      retainedBytes: retainedBytes,
      peakBytes: stats.peakBytes,
      createdCount: stats.createdCount,
      cumulativeBytes: stats.cumulativeBytes,
    };
  }

  window._mapglClusterDonut = { attach: attach, memory: memory };
})();
