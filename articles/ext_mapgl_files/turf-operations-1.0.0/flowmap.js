window.MapGLFlowmapPlugin = (function () {
  const FLOWMAP_ATTRIBUTION_SELECTOR =
    ".mapboxgl-ctrl-attrib-inner, .maplibregl-ctrl-attrib-inner";
  const FLOWMAP_ATTRIBUTION_LINK_SELECTOR =
    'a[data-mapgl-flowmap-attribution="true"]';
  const FLOWMAP_ATTRIBUTION_SEPARATOR_SELECTOR =
    '[data-mapgl-flowmap-attribution-separator="true"]';
  const DEFAULT_LOCATION_TOOLTIP =
    "<strong>{name}</strong><br>Incoming trips: {totals.incomingCount}<br>Outgoing trips: {totals.outgoingCount}<br>Internal or round trips: {totals.internalCount}";
  const DEFAULT_FLOW_TOOLTIP = "<strong>{count}</strong> trips";

  function dataframeToRows(data, HTMLWidgets) {
    if (!data || Array.isArray(data) || typeof data !== "object") {
      return data;
    }

    if (HTMLWidgets && typeof HTMLWidgets.dataframeToD3 === "function") {
      return HTMLWidgets.dataframeToD3(data);
    }

    return data;
  }

  function attributionNodeHasContent(node) {
    if (!node) {
      return false;
    }

    if (
      node.nodeType === 1 &&
      (node.matches(FLOWMAP_ATTRIBUTION_LINK_SELECTOR) ||
        node.matches(FLOWMAP_ATTRIBUTION_SEPARATOR_SELECTOR))
    ) {
      return false;
    }

    return node.textContent && node.textContent.trim() !== "";
  }

  function hasNativeAttributionContent(attributionInner) {
    for (var i = 0; i < attributionInner.childNodes.length; i++) {
      if (attributionNodeHasContent(attributionInner.childNodes[i])) {
        return true;
      }
    }

    return false;
  }

  function makeFlowmapAttributionLink() {
    const link = document.createElement("a");
    link.href = "https://flowmap.gl/";
    link.target = "_blank";
    link.rel = "noopener noreferrer";
    link.setAttribute("data-mapgl-flowmap-attribution", "true");
    link.textContent = "Flowmap.gl";
    return link;
  }

  function normalizeFlowmapAttributionLink(link) {
    link.href = "https://flowmap.gl/";
    link.target = "_blank";
    link.rel = "noopener noreferrer";
    link.setAttribute("data-mapgl-flowmap-attribution", "true");
    link.textContent = "Flowmap.gl";
  }

  function ensureFlowmapAttribution(map) {
    if (!map || typeof map.getContainer !== "function") {
      return;
    }

    const container = map.getContainer();
    if (!container || typeof container.querySelector !== "function") {
      return;
    }

    const attributionInner = container.querySelector(
      FLOWMAP_ATTRIBUTION_SELECTOR
    );
    if (!attributionInner) {
      return;
    }

    const existingLinks = attributionInner.querySelectorAll(
      FLOWMAP_ATTRIBUTION_LINK_SELECTOR
    );
    var link = existingLinks[0] || makeFlowmapAttributionLink();

    normalizeFlowmapAttributionLink(link);

    for (var i = 1; i < existingLinks.length; i++) {
      existingLinks[i].remove();
    }

    const separators = attributionInner.querySelectorAll(
      FLOWMAP_ATTRIBUTION_SEPARATOR_SELECTOR
    );
    for (var j = 0; j < separators.length; j++) {
      separators[j].remove();
    }

    if (attributionInner.firstChild !== link) {
      attributionInner.insertBefore(link, attributionInner.firstChild);
    }

    if (hasNativeAttributionContent(attributionInner)) {
      const separator = document.createElement("span");
      separator.setAttribute(
        "data-mapgl-flowmap-attribution-separator",
        "true"
      );
      separator.textContent = " | ";
      attributionInner.insertBefore(separator, link.nextSibling);
    }
  }

  function installFlowmapAttributionRefresh(map) {
    if (!map || map._mapglFlowmapAttributionRefreshInstalled) {
      return;
    }

    var timer = null;
    const refresh = function () {
      if (timer) {
        clearTimeout(timer);
      }
      timer = setTimeout(function () {
        timer = null;
        ensureFlowmapAttribution(map);
      }, 50);
    };

    map._mapglFlowmapAttributionRefreshInstalled = true;
    map._mapglFlowmapAttributionRefresh = refresh;

    map.on("styledata", refresh);
    map.on("sourcedata", refresh);
    map.on("idle", refresh);
    map.once("remove", function () {
      if (timer) {
        clearTimeout(timer);
        timer = null;
      }
      map.off("styledata", refresh);
      map.off("sourcedata", refresh);
      map.off("idle", refresh);
      map._mapglFlowmapAttributionRefreshInstalled = false;
      map._mapglFlowmapAttributionRefresh = null;
    });

    ensureFlowmapAttribution(map);
  }

  function getTooltipStore(map) {
    if (!map._mapglFlowmapTooltips) {
      map._mapglFlowmapTooltips = {};
    }
    return map._mapglFlowmapTooltips;
  }

  function hideFlowmapTooltip(map, id) {
    const store = getTooltipStore(map);
    const tooltip = store[id];
    if (!tooltip) {
      return;
    }
    if (tooltip.popup) {
      tooltip.popup.remove();
    }
    if (tooltip.element) {
      tooltip.element.style.display = "none";
    }
  }

  function hideAllFlowmapTooltips(map) {
    const store = getTooltipStore(map);
    Object.keys(store).forEach(function (id) {
      hideFlowmapTooltip(map, id);
    });
  }

  function hideOtherFlowmapTooltips(map, activeId) {
    const store = getTooltipStore(map);
    Object.keys(store).forEach(function (id) {
      if (id !== activeId) {
        hideFlowmapTooltip(map, id);
      }
    });
  }

  function getPopupConstructor() {
    if (typeof mapboxgl !== "undefined" && mapboxgl.Popup) {
      return mapboxgl.Popup;
    }
    if (typeof maplibregl !== "undefined" && maplibregl.Popup) {
      return maplibregl.Popup;
    }
    return null;
  }

  function escapeHTML(value) {
    return String(value == null ? "" : value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  // Mapbox-style expression evaluator (concat / number-format / get). flowmap
  // is a separate IIFE and cannot reach the bindings' copy, so it carries its
  // own — keep in sync with evaluateExpression in mapboxgl.js / maplibregl.js.
  function evaluateExpression(expression, properties) {
    if (!Array.isArray(expression)) {
      return expression;
    }
    const operator = expression[0];
    switch (operator) {
      case "get":
        return properties[expression[1]];
      case "concat":
        return expression
          .slice(1)
          .map(function (item) {
            return evaluateExpression(item, properties);
          })
          .join("");
      case "to-string":
        return String(evaluateExpression(expression[1], properties));
      case "to-number":
        return Number(evaluateExpression(expression[1], properties));
      case "number-format": {
        const value = evaluateExpression(expression[1], properties);
        const options = expression[2] || {};
        const locale = options.locale || "en-US";
        const formatOptions = {};
        if (options.style) formatOptions.style = options.style;
        if (options.currency) formatOptions.currency = options.currency;
        if (options.unit) formatOptions.unit = options.unit;
        if (options.hasOwnProperty("min-fraction-digits")) {
          formatOptions.minimumFractionDigits = options["min-fraction-digits"];
        }
        if (options.hasOwnProperty("max-fraction-digits")) {
          formatOptions.maximumFractionDigits = options["max-fraction-digits"];
        }
        if (options.hasOwnProperty("min-integer-digits")) {
          formatOptions.minimumIntegerDigits = options["min-integer-digits"];
        }
        if (options.notation) formatOptions.notation = options.notation;
        if (options.compactDisplay) {
          formatOptions.compactDisplay = options.compactDisplay;
        }
        if (options.hasOwnProperty("useGrouping")) {
          formatOptions.useGrouping = options.useGrouping;
        }
        return new Intl.NumberFormat(locale, formatOptions).format(value);
      }
      default:
        return expression;
    }
  }

  function getPathValue(object, path) {
    if (!object || !path) {
      return undefined;
    }

    return path.split(".").reduce(function (value, key) {
      if (value == null) {
        return undefined;
      }
      return value[key];
    }, object);
  }

  // Render a {brace} template against a properties object — shared package-wide
  // tooltip syntax (mirrors renderTemplate in the bindings). Dotted paths work
  // because the flat property bag below also keeps the nested object.
  function renderTemplate(template, properties) {
    if (typeof template !== "string") {
      return template;
    }
    return template.replace(/\{([^}]+)\}/g, function (match, path) {
      const value = getPathValue(properties, path.trim());
      return value == null ? "" : escapeHTML(value);
    });
  }

  // Resolve a tooltip/popup content spec: array -> expression, "{..}" -> brace
  // template, string -> column-name lookup on the property bag.
  function resolveTooltipContent(spec, properties) {
    if (Array.isArray(spec)) {
      return evaluateExpression(spec, properties);
    }
    if (typeof spec === "string" && spec.indexOf("{") !== -1) {
      return renderTemplate(spec, properties);
    }
    return properties[spec];
  }

  // Build a flat property bag for a picked location/flow so templates and
  // expressions can use simple keys (origin_id, dest_name, incoming, ...). The
  // original nested object is preserved so dotted paths ({origin.id},
  // {totals.incomingCount}) keep resolving too.
  function flowmapProps(object, objectType) {
    const props = Object.assign({}, object);
    if (objectType === "location") {
      const totals = object.totals || {};
      props.incoming = totals.incomingCount;
      props.outgoing = totals.outgoingCount;
      props.internal = totals.internalCount;
    } else if (objectType === "flow") {
      const origin = object.origin || {};
      const dest = object.dest || {};
      props.origin_id = origin.id;
      props.origin_name = origin.name;
      props.dest_id = dest.id;
      props.dest_name = dest.name;
    }
    return props;
  }

  function getTooltipHTML(config, info, behavior) {
    const interaction = config[behavior] || {};
    const object = info && info.object;
    if (!object) {
      return null;
    }

    // Determine object type if missing
    let objectType = object.type;
    if (!objectType) {
      if (object.lat != null && object.lon != null) {
        objectType = "location";
      } else if (object.origin != null && object.dest != null) {
        objectType = "flow";
      }
    }

    if (!objectType) {
      return null;
    }

    // The per-type content spec is one of the package-wide tooltip forms:
    //   true   -> the default {brace} template for this object type
    //   false  -> disabled
    //   string -> a column name or {brace} template
    //   array  -> a concat()/number_format() expression
    const spec = interaction[objectType];
    if (spec == null || spec === false) {
      return null;
    }

    const props = flowmapProps(object, objectType);

    let html;
    if (spec === true) {
      const template =
        objectType === "location"
          ? DEFAULT_LOCATION_TOOLTIP
          : DEFAULT_FLOW_TOOLTIP;
      html = renderTemplate(template, props);
    } else {
      const resolved = resolveTooltipContent(spec, props);
      html = resolved == null ? "" : String(resolved);
    }

    return html || null;
  }

  function getTooltipLngLat(map, info) {
    if (info && info.lngLat) {
      return info.lngLat;
    }
    if (info && Array.isArray(info.coordinate)) {
      return info.coordinate;
    }

    const object = info && info.object;
    if (!object) {
      return null;
    }

    if (object.type === "location" && object.location) {
      const location = object.location;
      if (location.lon != null && location.lat != null) {
        return [location.lon, location.lat];
      }
    }

    if (object.type === "flow" && object.origin && object.dest) {
      const origin = object.origin;
      const dest = object.dest;
      if (
        origin.lon != null &&
        origin.lat != null &&
        dest.lon != null &&
        dest.lat != null
      ) {
        return [(origin.lon + dest.lon) / 2, (origin.lat + dest.lat) / 2];
      }
    }

    return null;
  }

  function getTooltipPoint(map, info) {
    if (info && Number.isFinite(info.x) && Number.isFinite(info.y)) {
      return { x: info.x, y: info.y };
    }

    const lngLat = getTooltipLngLat(map, info);
    if (lngLat && map && typeof map.project === "function") {
      return map.project(lngLat);
    }

    return null;
  }

  function mergeClassName(base, extra) {
    return extra ? base + " " + extra : base;
  }

  function generateHash(str) {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = (hash << 5) - hash + char;
      hash |= 0;
    }
    return Math.abs(hash).toString(36);
  }

  function ensureCustomStyle(css, className) {
    const selector = className || "mapgl-custom-" + generateHash(css);
    const styleId = "style-" + selector;
    if (!document.getElementById(styleId)) {
      const style = document.createElement("style");
      style.id = styleId;
      style.textContent = "." + selector + " { " + css + " }";
      document.head.appendChild(style);
    }
    return selector;
  }

  // ---- tooltip/popup theming (mapgl_tooltip_style spec) -------------------
  // Mirrors tooltipStyleToClass in the bindings; flowmap additionally needs an
  // inline form because its "floating" tooltip is a bare <div>, not a Popup.
  function tooltipHexToRgba(color, alpha) {
    if (typeof color !== "string" || color.charAt(0) !== "#") return color;
    let h = color.slice(1);
    if (h.length === 3) {
      h = h.split("").map(function (c) { return c + c; }).join("");
    }
    const r = parseInt(h.slice(0, 2), 16);
    const g = parseInt(h.slice(2, 4), 16);
    const b = parseInt(h.slice(4, 6), 16);
    return "rgba(" + r + ", " + g + ", " + b + ", " + alpha + ")";
  }

  // Returns { decls: "background:...;color:...;", background: <bg or null> }.
  function tooltipStyleDecls(spec) {
    let bg = spec.background_color;
    if (bg != null && spec.background_opacity != null) {
      bg = tooltipHexToRgba(bg, spec.background_opacity);
    }
    const d = [];
    if (bg != null) d.push("background:" + bg + ";");
    if (spec.text_color != null) d.push("color:" + spec.text_color + ";");
    if (spec.border_color != null || spec.border_width != null) {
      const bw = spec.border_width == null ? 1 : spec.border_width;
      const bc = spec.border_color == null ? "transparent" : spec.border_color;
      d.push("border:" + bw + "px solid " + bc + ";");
    }
    if (spec.border_radius != null) {
      d.push("border-radius:" + spec.border_radius + "px;");
    }
    if (spec.font_family != null) d.push("font-family:" + spec.font_family + ";");
    if (spec.font_size != null) d.push("font-size:" + spec.font_size + "px;");
    if (spec.font_weight != null) d.push("font-weight:" + spec.font_weight + ";");
    if (spec.padding != null) d.push("padding:" + spec.padding + "px;");
    if (spec.max_width != null) {
      const mw =
        typeof spec.max_width === "number" ? spec.max_width + "px" : spec.max_width;
      d.push("max-width:" + mw + ";");
    }
    if (spec.shadow) {
      const ss = spec.shadow_size == null ? 8 : spec.shadow_size;
      const sc = spec.shadow_color == null ? "rgba(0, 0, 0, 0.2)" : spec.shadow_color;
      d.push("box-shadow:0 2px " + ss + "px " + sc + ";");
    }
    return { decls: d.join(""), background: bg };
  }

  // Scoped class for an anchored Popup (targets .popup-content + tip).
  function tooltipStyleToClass(spec) {
    if (!spec || typeof spec !== "object") return null;
    if (!window._mapglTooltipStyleClasses) window._mapglTooltipStyleClasses = {};
    const cache = window._mapglTooltipStyleClasses;
    const key = JSON.stringify(spec);
    if (cache[key]) return cache[key];
    const cls = "mapgl-tooltip-style-" + (Object.keys(cache).length + 1);
    const built = tooltipStyleDecls(spec);
    let css =
      "." + cls + " .mapboxgl-popup-content, ." + cls +
      " .maplibregl-popup-content {" + built.decls + "}";
    if (built.background != null) {
      css +=
        "." + cls + " .mapboxgl-popup-tip, ." + cls +
        " .maplibregl-popup-tip {border-top-color:" + built.background +
        ";border-bottom-color:" + built.background + ";}";
    }
    const styleEl = document.createElement("style");
    styleEl.textContent = css;
    document.head.appendChild(styleEl);
    cache[key] = cls;
    return cls;
  }

  // Inline declarations for a floating tooltip <div>.
  function tooltipStyleInline(spec) {
    if (!spec || typeof spec !== "object") return "";
    return tooltipStyleDecls(spec).decls;
  }

  function tooltipStyleOffset(spec) {
    const offset = spec && spec.offset;
    if (Array.isArray(offset) && offset.length >= 2) {
      return [Number(offset[0]) || 0, Number(offset[1]) || 0];
    }
    if (typeof offset === "number") return [offset, offset];
    return [10, 10];
  }

  function showInteractiveUI(map, config, info, behavior) {
    const interaction = config[behavior];
    if (!interaction || !interaction.enabled) return;

    const html = getTooltipHTML(config, info, behavior);
    if (!html) {
      if (behavior === "tooltip") hideFlowmapTooltip(map, config.id);
      return;
    }

    const lngLat = getTooltipLngLat(map, info);
    if (!lngLat) {
      if (behavior === "tooltip") hideFlowmapTooltip(map, config.id);
      return;
    }

    // Dismiss existing interactions of other types if needed
    if (behavior === "popup") {
      hideFlowmapTooltip(map, config.id);
    }

    // interaction.style is a mapgl_tooltip_style spec (appearance + position).
    const styleSpec = interaction.style || {};
    const position = styleSpec.position || "floating";

    if (position === "anchored") {
      const Popup = getPopupConstructor();
      if (!Popup) return;

      const className = mergeClassName(
        "mapgl-flowmap-tooltip",
        tooltipStyleToClass(styleSpec),
      );

      const options = {
        closeButton: behavior === "popup",
        closeOnClick: behavior === "popup" ? false : true,
        maxWidth: "400px",
        className: className,
      };

      const store = getTooltipStore(map);
      if (!store[config.id]) store[config.id] = {};

      const storeKey = behavior === "popup" ? "clickPopup" : "popup";

      if (store[config.id][storeKey]) {
        store[config.id][storeKey].remove();
      }

      store[config.id][storeKey] = new Popup(options)
        .setLngLat(lngLat)
        .setHTML(html)
        .addTo(map);
    } else {
      // Floating mode: a bare <div> styled inline from the spec.
      const point = getTooltipPoint(map, info);
      if (!point || !map || typeof map.getContainer !== "function") return;

      const store = getTooltipStore(map);
      if (!store[config.id]) store[config.id] = {};

      const storeKey = behavior === "popup" ? "clickElement" : "element";

      if (!store[config.id][storeKey]) {
        const element = document.createElement("div");
        element.style.position = "absolute";
        element.style.zIndex = "1000";
        map.getContainer().appendChild(element);
        store[config.id][storeKey] = element;
      }

      const element = store[config.id][storeKey];

      // Base class supplies layout/positioning; the spec supplies the theme
      // (background/border/etc.) as inline declarations.
      element.className = "mapgl-flowmap-example-tooltip flowmap-tooltip";
      element.style.cssText =
        "position:absolute;z-index:1000;" + tooltipStyleInline(styleSpec);
      element.innerHTML = html;

      const offset = tooltipStyleOffset(styleSpec);
      element.style.left = point.x + offset[0] + "px";
      element.style.top = point.y + offset[1] + "px";
      element.style.display = "block";
      element.style.pointerEvents = behavior === "popup" ? "auto" : "none";

      if (behavior === "popup") {
        // Delayed listener to avoid immediate dismissal from bubbling
        setTimeout(() => {
          const closeHandler = (e) => {
            // Don't close if we clicked inside the popup element itself
            if (element.contains(e.target)) return;
            element.style.display = "none";
            map.off("click", closeHandler);
          };
          map.on("click", closeHandler);
        }, 100);
      }
    }
  }

  function getElementOffset(options) {
    const offset = options && options.offset;
    if (Array.isArray(offset) && offset.length >= 2) {
      return [Number(offset[0]) || 0, Number(offset[1]) || 0];
    }
    if (typeof offset === "number") {
      return [offset, offset];
    }
    return [10, 10];
  }

  function ensureOverlay(map, interleaved, elId, settings) {
    if (typeof FlowmapGL === "undefined") {
      console.error("FlowmapGL is not loaded. Cannot add flowmap layers.");
      return null;
    }

    if (interleaved) {
      // Interleaved mode (MapboxOverlay) for layer ordering (beforeId/slot)
      if (
        map._mapglFlowmapOverlay &&
        map._mapglFlowmapOverlayInterleaved === true
      ) {
        return map._mapglFlowmapOverlay;
      }

      // Cleanup standalone Deck if any
      if (map._deckgl) {
        try {
          map._deckgl.finalize();
        } catch (e) {}
        map._deckgl = null;
      }
      if (map._deckContainer) {
        try {
          map._deckContainer.remove();
        } catch (e) {}
        map._deckContainer = null;
      }

      if (map._mapglFlowmapOverlay) {
        try {
          map.removeControl(map._mapglFlowmapOverlay);
        } catch (e) {}
      }

      const overlay = new FlowmapGL.MapboxOverlay({
        interleaved: true,
        layers: [],
      });

      map.addControl(overlay);
      map._mapglFlowmapOverlay = overlay;
      map._mapglFlowmapOverlayInterleaved = true;

      map.once("remove", function () {
        map._mapglFlowmapOverlay = null;
        map._mapglFlowmapLayers = [];
      });

      return overlay;
    } else {
      // Standalone mode: Create standalone Deck container to bypass nested control stacking context.
      // This is crucial because CSS mix-blend-mode will not blend with sibling map canvas elements
      // if nested deep inside Mapbox/MapLibre's .mapboxgl-control-container hierarchy.
      if (
        map._deckgl &&
        map._mapglFlowmapOverlayInterleaved === false
      ) {
        // Update blending styles on the existing canvas during widget updates
        // NOTE: blend must go on the canvas, not the container, to avoid
        // stacking-context isolation that prevents cross-element blending.
        const deckCanvas = map._deckCanvas;
        if (deckCanvas) {
          if (settings && settings.flowBlend) {
            if (typeof settings.flowBlend === "string") {
              deckCanvas.style.mixBlendMode = settings.flowBlend;
            } else {
              deckCanvas.style.mixBlendMode = settings.darkMode ? "screen" : "multiply";
            }
          } else {
            deckCanvas.style.mixBlendMode = "";
          }
        }
        return map._deckgl;
      }

      // Cleanup MapboxOverlay if any
      if (map._mapglFlowmapOverlay) {
        try {
          map.removeControl(map._mapglFlowmapOverlay);
        } catch (e) {}
        map._mapglFlowmapOverlay = null;
      }
      if (map._deckgl) {
        try {
          map._deckgl.finalize();
        } catch (e) {}
        map._deckgl = null;
      }
      if (map._deckContainer) {
        try {
          map._deckContainer.remove();
        } catch (e) {}
        map._deckContainer = null;
      }

      const container = map.getContainer();

      // Create overlay container div directly under map container (sibling of canvas container)
      const deckContainer = document.createElement("div");
      deckContainer.id = "deck-container-" + elId;
      deckContainer.style.cssText = "position:absolute;top:0;left:0;width:100%;height:100%;pointer-events:none;";
      container.appendChild(deckContainer);

      // Create standalone canvas
      const deckCanvas = document.createElement("canvas");
      deckCanvas.id = "deck-canvas-" + elId;
      deckCanvas.style.cssText = "width:100%;height:100%;";
      deckContainer.appendChild(deckCanvas);

      map._deckContainer = deckContainer;
      map._deckCanvas = deckCanvas;

      const center = map.getCenter();
      const initialViewState = {
        longitude: center.lng,
        latitude: center.lat,
        zoom: map.getZoom(),
        pitch: map.getPitch(),
        bearing: map.getBearing()
      };

      const deckInstance = new FlowmapGL.Deck({
        canvas: deckCanvas,
        controller: false, // map controls the camera
        _useDevicePixels: true,
        initialViewState: initialViewState,
        layers: [],
        getTooltip: null,
        pickingRadius: 5
      });

      // Synchronize standalone Deck viewport state with map moves
      const syncViewState = () => {
        const center = map.getCenter();
        deckInstance.setProps({
          viewState: {
            longitude: center.lng,
            latitude: center.lat,
            zoom: map.getZoom(),
            pitch: map.getPitch(),
            bearing: map.getBearing()
          }
        });
      };

      map.on("move", syncViewState);
      map.on("moveend", syncViewState);
      syncViewState();

      // Forward mouse events from Mapbox to standalone Deck for picking / hover tooltips.
      // We manually delegate hover and click events to the deck.gl sublayers, which
      // automatically propagates them up to the composite FlowmapLayer. This ensures
      // that the composite layer's internal highlight state is updated synchronously,
      // and its async picking info enrichment is executed before calling our custom handlers.
      const onMapMouseMove = (e) => {
        if (!deckInstance || !map._mapglFlowmapLayers || map._mapglFlowmapLayers.length === 0) return;
        const { x, y } = e.point;
        try {
          const info = deckInstance.pickObject({ x, y, radius: 2 });
          map.getCanvas().style.cursor = info ? "pointer" : "";

          if (info && info.layer) {
            const event = { srcEvent: e.originalEvent || e };
            info.x = x;
            info.y = y;
            info.lngLat = e.lngLat;
            if (e.lngLat) {
              info.coordinate = [e.lngLat.lng, e.lngLat.lat];
            }
            if (typeof info.layer.onHover === "function") {
              info.layer.onHover(info, event);
            } else if (info.layer.props && typeof info.layer.props.onHover === "function") {
              info.layer.props.onHover(info, event);
            }
          } else {
            hideAllFlowmapTooltips(map);
            // Also notify active flowmap layers that hover has ended, to clear highlights
            map._mapglFlowmapLayers.forEach((layer) => {
              if (layer.props && typeof layer.props.onHover === "function") {
                layer.props.onHover({ index: -1 }, {});
              }
            });
          }
        } catch (err) {
          // Ignore deck.gl picking errors during init / rapid movement
        }
      };

      const onMapClick = (e) => {
        if (!deckInstance || !map._mapglFlowmapLayers || map._mapglFlowmapLayers.length === 0) return;
        const { x, y } = e.point;
        try {
          const info = deckInstance.pickObject({ x, y, radius: 5 });
          if (info && info.layer) {
            const event = { srcEvent: e.originalEvent || e };
            info.x = x;
            info.y = y;
            info.lngLat = e.lngLat;
            if (e.lngLat) {
              info.coordinate = [e.lngLat.lng, e.lngLat.lat];
            }
            if (typeof info.layer.onClick === "function") {
              info.layer.onClick(info, event);
            } else if (info.layer.props && typeof info.layer.props.onClick === "function") {
              info.layer.props.onClick(info, event);
            }
          }
        } catch (err) {
          // Ignore deck.gl picking errors
        }
      };

      if (!map._hasDeckMoveListener) {
        map.on("mousemove", onMapMouseMove);
        map.on("click", onMapClick);
        map.on("mouseout", function () {
          hideAllFlowmapTooltips(map);
          // Clear hover highlights on mouseout — directly update state to avoid async delay
          let needsRedraw = false;
          if (map._flowmapHoverTimers) {
            Object.keys(map._flowmapHoverTimers).forEach(function(id) {
              if (map._flowmapHoverTimers[id]) {
                clearTimeout(map._flowmapHoverTimers[id]);
                map._flowmapHoverTimers[id] = null;
              }
            });
          }
          if (map._flowmapHoveredLocation) {
            Object.keys(map._flowmapHoveredLocation).forEach(function(id) {
              if (map._flowmapHoveredLocation[id] !== null) {
                map._flowmapHoveredLocation[id] = null;
                needsRedraw = true;
              }
            });
          }
          if (needsRedraw) {
            redrawFlowmaps(map, map._flowmapHTMLWidgets);
          }
          // Also notify via layer.props for any other listeners
          if (map._mapglFlowmapLayers) {
            map._mapglFlowmapLayers.forEach((layer) => {
              if (layer.props && typeof layer.props.onHover === "function") {
                layer.props.onHover({ index: -1 }, {});
              }
            });
          }
        });
        map._hasDeckMoveListener = true;
      }

      map._deckgl = deckInstance;
      map._mapglFlowmapOverlayInterleaved = false;

      // Apply CSS Blending directly to the canvas element.
      // IMPORTANT: The container must NOT have a z-index (other than auto)
      // because that creates a stacking context which isolates the canvas
      // and prevents mix-blend-mode from blending with the underlying map.
      if (settings && settings.flowBlend) {
        if (typeof settings.flowBlend === "string") {
          deckCanvas.style.mixBlendMode = settings.flowBlend;
        } else {
          deckCanvas.style.mixBlendMode = settings.darkMode ? "screen" : "multiply";
        }
      } else {
        deckCanvas.style.mixBlendMode = "";
      }

      map.once("remove", function () {
        if (map._deckgl) {
          try {
            map._deckgl.finalize();
          } catch (e) {}
          map._deckgl = null;
        }
        if (map._deckContainer) {
          try {
            map._deckContainer.remove();
          } catch (e) {}
          map._deckContainer = null;
        }
        map._mapglFlowmapLayers = [];
        hideAllFlowmapTooltips(map);
      });

      return deckInstance;
    }
  }

  function makeLayer(config, HTMLWidgets, map, customLocations, customFlows, customPickable) {
    const settings = config.settings || {};

    // Cache converted rows on the original config's data object (for the canonical id)
    const canonicalId = config._canonicalId || config.id;
    const canonical = map._flowmapsConfig && map._flowmapsConfig.find(function(c) { return c.id === canonicalId; });
    const dataTarget = canonical ? canonical.data : config.data;

    if (!dataTarget._locationsRows) {
      dataTarget._locationsRows = dataframeToRows(dataTarget.locations, HTMLWidgets);
    }
    if (!dataTarget._flowsRows) {
      dataTarget._flowsRows = dataframeToRows(dataTarget.flows, HTMLWidgets);
    }

    const locations = customLocations !== undefined ? customLocations : dataTarget._locationsRows;
    const flows = customFlows !== undefined ? customFlows : dataTarget._flowsRows;
    const isPickable = customPickable !== undefined ? customPickable : true;
    // When custom flows are provided we've already done the location filtering externally
    const skipLocationFilter = customFlows !== undefined;

    const layerProps = {
      id: config.id,
      data: {
        locations: locations,
        flows: flows,
      },
      beforeId: config.beforeId || undefined,
      slot: config.slot || undefined,
      pickable: isPickable,
      visible: config.visibility !== "none",
      opacity: settings.opacity == null ? 1 : settings.opacity,
      colorScheme: settings.colorScheme,
      darkMode: settings.darkMode,
      fadeAmount: settings.fadeAmount,
      highlightColor: settings.highlightColor,
      locationsEnabled: settings.locationsEnabled,
      locationTotalsEnabled: settings.locationTotalsEnabled,
      locationLabelsEnabled: settings.locationLabelsEnabled,
      flowLinesRenderingMode: settings.flowLinesRenderingMode,
      flowLineThicknessScale: settings.flowLineThicknessScale == null ? 1 : settings.flowLineThicknessScale,
      flowLineCurviness: settings.flowLineCurviness == null ? 1 : settings.flowLineCurviness,
      clusteringEnabled: settings.clusteringEnabled,
      clusteringAuto: settings.clusteringAuto,
      clusteringLevel: settings.clusteringLevel === null ? undefined : settings.clusteringLevel,
      fadeEnabled: settings.fadeEnabled,
      fadeOpacityEnabled: settings.fadeOpacityEnabled,
      adaptiveScalesEnabled: settings.adaptiveScalesEnabled,
      temporalScaleDomain: settings.temporalScaleDomain || "selected",
      maxTopFlowsDisplayNum: settings.maxTopFlowsDisplayNum,
      flowEndpointsInViewportMode: settings.flowEndpointsInViewportMode,
      getLocationId: function (location) {
        return location.id;
      },
      getLocationLat: function (location) {
        return location.lat;
      },
      getLocationLon: function (location) {
        return location.lon;
      },
      getLocationName: function (location) {
        return location.name || location.id;
      },
      getFlowOriginId: function (flow) {
        return flow.origin;
      },
      getFlowDestId: function (flow) {
        return flow.dest;
      },
      getFlowMagnitude: function (flow) {
        return flow.count;
      },
    };

    if (settings.timeColumn) {
      layerProps.getFlowTime = function (flow) {
        return flow.time ? new Date(flow.time) : undefined;
      };
    }

    if (settings.selectedTimeRange) {
      layerProps.filter = {
        ...layerProps.filter,
        selectedTimeRange: [
          new Date(settings.selectedTimeRange[0]),
          new Date(settings.selectedTimeRange[1])
        ]
      };
    }

    if (settings.selectedTimeRanges) {
      layerProps.filter = {
        ...layerProps.filter,
        selectedTimeRange: null,
        selectedTimeRanges: normalizeTimeRanges(settings.selectedTimeRanges)
      };
    }

    // Only apply location filter on the FlowmapLayer when we haven't pre-filtered externally.
    // When customFlows is provided, the caller already filtered by selected locations.
    if (!skipLocationFilter) {
      let selectedLocs = settings.selectedLocations;
      if (settings.clickToFilter && map._flowmapSelectedLocations && map._flowmapSelectedLocations[canonicalId]) {
        selectedLocs = map._flowmapSelectedLocations[canonicalId];
      }
      if (selectedLocs && selectedLocs.length > 0) {
        layerProps.filter = {
          ...layerProps.filter,
          selectedLocations: selectedLocs
        };
      }
    }

    if (settings.locationFilterMode) {
      layerProps.filter = {
        ...layerProps.filter,
        locationFilterMode: settings.locationFilterMode
      };
    }

    // Only attach interactive handlers on the canonical (pickable) layer.
    // The originalConfig is the root config from _flowmapsConfig for closures.
    const originalConfig = (map._flowmapsConfig && map._flowmapsConfig.find(function(c) { return c.id === canonicalId; })) || config;

    const hasHoverHandler = (originalConfig.tooltip && originalConfig.tooltip.enabled) || settings.hoverToHighlight;
    const hasClickHandler = (originalConfig.popup && originalConfig.popup.enabled) || settings.clickToFilter;

    if (isPickable && hasHoverHandler) {
      layerProps.onHover = function (info) {
        if (originalConfig.tooltip && originalConfig.tooltip.enabled) {
          showInteractiveUI(map, originalConfig, info, "tooltip");
        }
        if (settings.hoverToHighlight) {
          handleHoverInteraction(map, originalConfig, info);
        }
      };
    }

    if (isPickable && hasClickHandler) {
      layerProps.onClick = function (info) {
        if (originalConfig.popup && originalConfig.popup.enabled) {
          showInteractiveUI(map, originalConfig, info, "popup");
        }
        if (settings.clickToFilter) {
          handleClickInteraction(map, originalConfig, info);
        }
      };
    }

    return makeFlowmapLayer(layerProps);
  }

  function makeFlowmapLayer(layerProps) {
    const layer = new FlowmapGL.FlowmapLayer(layerProps);
    layer._mapglOnHover = layerProps.onHover;
    layer._mapglOnClick = layerProps.onClick;
    return layer;
  }

  function cloneFlowmapLayer(layer, props) {
    const cloneProps = Object.assign({}, props);

    if (Object.prototype.hasOwnProperty.call(layer, "_mapglOnHover")) {
      cloneProps.onHover = layer._mapglOnHover;
    }
    if (Object.prototype.hasOwnProperty.call(layer, "_mapglOnClick")) {
      cloneProps.onClick = layer._mapglOnClick;
    }

    const cloned = layer.clone(cloneProps);
    cloned._mapglOnHover = layer._mapglOnHover;
    cloned._mapglOnClick = layer._mapglOnClick;
    return cloned;
  }

  function init(map, x, el, HTMLWidgets) {
    if (!x.flowmaps || x.flowmaps.length === 0) {
      return;
    }

    if (typeof FlowmapGL === "undefined" || !FlowmapGL.FlowmapLayer) {
      console.error("FlowmapGL is not loaded. Cannot add flowmap layers.");
      return;
    }

    installFlowmapAttributionRefresh(map);

    const interleaved = x.flowmaps.some(function (flowmap) {
      return Boolean(flowmap.beforeId || flowmap.slot);
    });

    var firstFlowmap = x.flowmaps[0];
    var settings = firstFlowmap.settings || {};

    const overlay = ensureOverlay(map, interleaved, el.id, settings);
    if (!overlay) {
      return;
    }

    // Initialize interaction state maps
    if (!map._flowmapSelectedLocations) {
      map._flowmapSelectedLocations = {};
    }
    if (!map._flowmapHoveredLocation) {
      map._flowmapHoveredLocation = {};
    }
    if (!map._flowmapHoverTimers) {
      map._flowmapHoverTimers = {};
    }
    map._flowmapsConfig = x.flowmaps;
    map._flowmapHTMLWidgets = HTMLWidgets;

    // A set_style() wipes the basemap (and, in interleaved mode, the deck
    // layers) so the flowmap must be rebuilt once the new style loads. The
    // persisted _flowmapsConfig carries the current settings — including any
    // slider-written selectedTimeRange — so redrawFlowmaps() restores the
    // flowmap and its temporal filter by construction. Installed once per map
    // and debounced because a single set_style() can emit style.load more than
    // once.
    if (!map._flowmapStyleReloadInstalled) {
      map._flowmapStyleReloadInstalled = true;
      var flowmapStyleReloadTimer = null;
      map.on("style.load", function () {
        if (!map._flowmapsConfig || map._flowmapsConfig.length === 0) {
          return;
        }
        if (flowmapStyleReloadTimer) {
          clearTimeout(flowmapStyleReloadTimer);
        }
        flowmapStyleReloadTimer = setTimeout(function () {
          flowmapStyleReloadTimer = null;
          if (!map._flowmapsConfig || map._flowmapsConfig.length === 0) {
            return;
          }
          redrawFlowmaps(map, map._flowmapHTMLWidgets);
          // Let listeners (e.g. a slider driving the temporal window) re-emit
          // their current state after the flowmap has been rebuilt.
          if (typeof map.fire === "function") {
            map.fire("mapgl:flowmap-reinit");
          }
        }, 50);
      });
    }

    x.flowmaps.forEach(function (config) {
      const id = config.id;
      const cSettings = config.settings || {};
      if (map._flowmapSelectedLocations[id] === undefined) {
        const initialLocs = cSettings.selectedLocations;
        map._flowmapSelectedLocations[id] = Array.isArray(initialLocs)
          ? [...initialLocs]
          : (initialLocs ? [initialLocs] : []);
      }
      if (map._flowmapHoveredLocation[id] === undefined) {
        map._flowmapHoveredLocation[id] = null;
      }
      if (map._flowmapHoverTimers[id] === undefined) {
        map._flowmapHoverTimers[id] = null;
      }
    });

    redrawFlowmaps(map, HTMLWidgets);
  }

  function redrawFlowmaps(map, HTMLWidgets) {
    if (!map._flowmapsConfig) return;
    const HTMLWidgetsInstance = HTMLWidgets || map._flowmapHTMLWidgets;
    const layers = [];

    map._flowmapsConfig.forEach(function (config) {
      const id = config.id;
      const settings = config.settings || {};
      const isVisible = config.visibility !== "none";

      if (!isVisible) {
        return;
      }

      const hoverActive = settings.hoverToHighlight && map._flowmapHoveredLocation[id];

      if (!config.data._locationsRows) {
        config.data._locationsRows = dataframeToRows(config.data.locations, HTMLWidgetsInstance);
      }
      if (!config.data._flowsRows) {
        config.data._flowsRows = dataframeToRows(config.data.flows, HTMLWidgetsInstance);
      }

      let baseLocations = config.data._locationsRows;
      let baseFlows = config.data._flowsRows;

      const selected = map._flowmapSelectedLocations[id] || [];
      if (settings.clickToFilter && selected.length > 0) {
        const selectedSet = new Set(selected.map(String));
        baseFlows = baseFlows.filter(function (flow) {
          return selectedSet.has(String(flow.origin)) || selectedSet.has(String(flow.dest));
        });
      }

      if (hoverActive) {
        const hovered = map._flowmapHoveredLocation[id];

        // Build the full normal layer first, then clone it with dimmed opacity.
        // Cloning reuses the FlowmapLayer's internal state and preserves handlers.
        const layer = makeLayer(config, HTMLWidgetsInstance, map, baseLocations, baseFlows);
        const dimOpacity = (settings.opacity == null ? 1.0 : settings.opacity) * 0.15;
        const baseLayer = cloneFlowmapLayer(layer, {
          id: id + "-dim",
          opacity: dimOpacity,
          pickable: true  // Keep pickable so hover events pass through to handlers
        });
        layers.push(baseLayer);

        let highlightFlows = [];
        let highlightLocations = [];
        const connectedLocationIds = new Set();

        if (hovered.type === "location") {
          const hoveredId = String(hovered.id);
          connectedLocationIds.add(hoveredId);
          highlightFlows = baseFlows.filter(function (flow) {
            const isConnected = String(flow.origin) === hoveredId || String(flow.dest) === hoveredId;
            if (isConnected) {
              connectedLocationIds.add(String(flow.origin));
              connectedLocationIds.add(String(flow.dest));
            }
            return isConnected;
          });
        } else if (hovered.type === "flow") {
          // For flow type, origin/dest IDs are in hovered.origin.id and hovered.dest.id
          // (the picking info object has {type:'flow', flow:<raw>, origin:<loc>, dest:<loc>, count})
          const originId = hovered.origin ? String(hovered.origin.id) : null;
          const destId = hovered.dest ? String(hovered.dest.id) : null;
          if (originId && destId) {
            connectedLocationIds.add(originId);
            connectedLocationIds.add(destId);
            highlightFlows = baseFlows.filter(function (flow) {
              return String(flow.origin) === originId && String(flow.dest) === destId;
            });
          }
        }

        highlightLocations = baseLocations.filter(function (loc) {
          return connectedLocationIds.has(String(loc.id));
        });

        // Highlight overlay — not pickable so cursor passes through to dim base layer.
        const hlLayerSettings = Object.assign({}, settings, {
          opacity: settings.opacity == null ? 1.0 : settings.opacity,
          clusteringEnabled: false  // disable clustering so small highlight sets render correctly
        });
        const highlightLayerConfig = {
          _canonicalId: id,
          id: id + "-highlight",
          data: config.data,
          settings: hlLayerSettings,
          visibility: config.visibility,
          beforeId: config.beforeId,
          slot: config.slot,
          tooltip: null,
          popup: null,
        };

        const highlightLayer = makeLayer(
          highlightLayerConfig,
          HTMLWidgetsInstance,
          map,
          highlightLocations,
          highlightFlows,
          false  // not pickable
        );
        layers.push(highlightLayer);
      } else {
        const normalLayer = makeLayer(config, HTMLWidgetsInstance, map, baseLocations, baseFlows);
        layers.push(normalLayer);
      }
    });

    map._mapglFlowmapLayers = layers;
    const overlay = map._mapglFlowmapOverlay || map._deckgl;
    if (overlay) {
      overlay.setProps({ layers: layers });
    }
  }

  function handleHoverInteraction(map, config, info) {
    const id = config.id;
    const settings = config.settings || {};
    const hoveredObject = info && info.object;

    if (map._flowmapHoverTimers[id]) {
      clearTimeout(map._flowmapHoverTimers[id]);
      map._flowmapHoverTimers[id] = null;
    }

    if (hoveredObject) {
      const objectType = hoveredObject.type;  // 'location' or 'flow' — set by FlowmapLayer

      if (objectType === "location" || objectType === "flow") {
        const currentHovered = map._flowmapHoveredLocation[id];
        let isSame = false;
        if (currentHovered && currentHovered.type === objectType) {
          if (objectType === "location") {
            isSame = String(currentHovered.id) === String(hoveredObject.id);
          } else {
            // For flows: use origin.id / dest.id from the enriched location objects
            const curOrig = currentHovered.origin && String(currentHovered.origin.id);
            const curDest = currentHovered.dest && String(currentHovered.dest.id);
            const newOrig = hoveredObject.origin && String(hoveredObject.origin.id);
            const newDest = hoveredObject.dest && String(hoveredObject.dest.id);
            isSame = curOrig === newOrig && curDest === newDest;
          }
        }

        if (!isSame) {
          const delay = settings.hoverHighlightDelay != null ? settings.hoverHighlightDelay : 500;
          if (delay > 0) {
            map._flowmapHoverTimers[id] = setTimeout(function () {
              map._flowmapHoveredLocation[id] = hoveredObject;
              map._flowmapHoverTimers[id] = null;
              redrawFlowmaps(map, map._flowmapHTMLWidgets);
            }, delay);
          } else {
            map._flowmapHoveredLocation[id] = hoveredObject;
            redrawFlowmaps(map, map._flowmapHTMLWidgets);
          }
        }
        return;
      }
    }

    // Mouse left the element — clear pending timer and reset highlight
    if (map._flowmapHoveredLocation[id] !== null) {
      map._flowmapHoveredLocation[id] = null;
      redrawFlowmaps(map, map._flowmapHTMLWidgets);
    }
  }

  function handleClickInteraction(map, config, info) {
    const id = config.id;
    const clickedObject = info && info.object;
    if (!clickedObject) {
      return;
    }

    const objectType = clickedObject.type;  // 'location' or 'flow'

    if (objectType === "location") {
      const locId = clickedObject.id;
      const selected = map._flowmapSelectedLocations[id] || [];
      const index = selected.indexOf(locId);
      let newSelected;
      if (index > -1) {
        // Deselect
        newSelected = selected.filter(function(s) { return s !== locId; });
      } else {
        // Select — copy to avoid mutation
        newSelected = selected.concat([locId]);
      }
      map._flowmapSelectedLocations[id] = newSelected;
      // Clear hover highlight when selection changes
      if (map._flowmapHoverTimers[id]) {
        clearTimeout(map._flowmapHoverTimers[id]);
        map._flowmapHoverTimers[id] = null;
      }
      map._flowmapHoveredLocation[id] = null;
      redrawFlowmaps(map, map._flowmapHTMLWidgets);
    }
  }

  function hasLayer(map, id) {
    // First check canonical config store (works even when hover replaces the layer id)
    if (map && map._flowmapsConfig) {
      return map._flowmapsConfig.some(function (c) { return c.id === id; });
    }
    return Boolean(
      map &&
        map._mapglFlowmapLayers &&
        map._mapglFlowmapLayers.some(function (layer) {
          return layer.id === id;
        }),
    );
  }

  function getVisibility(map, id) {
    if (!map._flowmapsConfig) return undefined;
    const config = map._flowmapsConfig.find(function (c) {
      return c.id === id;
    });
    return config ? (config.visibility === "none" ? "none" : "visible") : undefined;
  }

  function setVisibility(map, id, visibility) {
    if (!map._flowmapsConfig) {
      return false;
    }
    let found = false;
    map._flowmapsConfig.forEach(function (config) {
      if (config.id === id) {
        config.visibility = visibility;
        if (visibility === "none") {
          hideFlowmapTooltip(map, id);
        }
        found = true;
      }
    });
    if (found) {
      redrawFlowmaps(map, map._flowmapHTMLWidgets);
      return true;
    }
    return false;
  }

  function setFilter(map, id, filter) {
    if (!map._flowmapsConfig) {
      return false;
    }
    let found = false;
    map._flowmapsConfig.forEach(function (config) {
      if (config.id === id) {
        if (!config.settings) {
          config.settings = {};
        }
        Object.assign(config.settings, filter);

        if (filter.selectedLocations) {
          map._flowmapSelectedLocations[id] = filter.selectedLocations;
        }

        found = true;
      }
    });
    if (found) {
      redrawFlowmaps(map, map._flowmapHTMLWidgets);
      return true;
    }
    return false;
  }

  function normalizeTimeRanges(ranges) {
    if (!Array.isArray(ranges)) {
      return null;
    }

    return ranges
      .filter(function (range) {
        return Array.isArray(range) && range.length === 2;
      })
      .map(function (range) {
        return [new Date(range[0]), new Date(range[1])];
      });
  }

  function setSettings(map, id, settings) {
    if (!map._flowmapsConfig) {
      return false;
    }
    let found = false;
    map._flowmapsConfig.forEach(function (config) {
      if (config.id === id) {
        if (!config.settings) {
          config.settings = {};
        }
        Object.assign(config.settings, settings);
        found = true;
      }
    });
    if (found) {
      redrawFlowmaps(map, map._flowmapHTMLWidgets);
      return true;
    }
    return false;
  }

  return {
    init: init,
    hasLayer: hasLayer,
    getVisibility: getVisibility,
    setVisibility: setVisibility,
    setFilter: setFilter,
    setSettings: setSettings,
  };
})();
