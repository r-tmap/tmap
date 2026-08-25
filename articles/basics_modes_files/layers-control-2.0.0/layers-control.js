// Shared layers control for all mapgl widgets (mapboxgl / maplibregl,
// standalone and compare). Implements the GL IControl interface so the
// control participates in the native control stack via map.addControl()
// instead of floating over it.
//
// The constructor accepts the R payload object verbatim (either the initial
// x.layers_control or a proxy message): control_id, layers, layers_config,
// collapsible, use_icon, custom_colors, margin_top/right/bottom/left.
// Extra fields (position, type, map) are ignored.
(function () {
  "use strict";

  var LAYERS_ICON_SVG =
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">' +
    '<polygon points="12 2 2 7 12 12 22 7 12 2"></polygon>' +
    '<polyline points="2 17 12 22 22 17"></polyline>' +
    '<polyline points="2 12 12 17 22 12"></polyline>' +
    "</svg>";

  function MapglLayersControl(config) {
    this._config = config || {};
    this._single = this._config.mode === "single";
    this._map = null;
    this._container = null;
    this._links = [];
  }

  MapglLayersControl.prototype.onAdd = function (map) {
    var config = this._config;
    var self = this;
    this._map = map;

    map._mapglLayersControls = map._mapglLayersControls || [];
    map._mapglLayersControls.push(this);

    var container = document.createElement("div");
    this._container = container;
    if (config.control_id) {
      container.id = config.control_id;
    }
    // Dual-prefix so the native ctrl-group chrome applies under either engine;
    // .layers-control and #control_id stay on the root for user CSS overrides
    container.className =
      "mapboxgl-ctrl maplibregl-ctrl mapboxgl-ctrl-group maplibregl-ctrl-group layers-control";
    if (config.collapsible) {
      container.classList.add("collapsible");
    }

    // Custom colors become per-control CSS variables consumed by
    // layers-control.css; keys are what R sends from add_layers_control()
    var colors = config.custom_colors;
    if (colors) {
      if (colors.background) {
        container.style.setProperty("--lc-bg", colors.background);
      }
      if (colors.text) {
        container.style.setProperty("--lc-text", colors.text);
      }
      if (colors.active) {
        container.style.setProperty("--lc-active-bg", colors.active);
      }
      if (colors.activeText) {
        container.style.setProperty("--lc-active-text", colors.activeText);
      }
      if (colors.hover) {
        container.style.setProperty("--lc-hover-bg", colors.hover);
      }
    }

    // Margins are applied only when explicitly requested; the native stack
    // spaces controls on its own
    if (config.margin_top != null) {
      container.style.marginTop = config.margin_top + "px";
    }
    if (config.margin_right != null) {
      container.style.marginRight = config.margin_right + "px";
    }
    if (config.margin_bottom != null) {
      container.style.marginBottom = config.margin_bottom + "px";
    }
    if (config.margin_left != null) {
      container.style.marginLeft = config.margin_left + "px";
    }

    var layersList = document.createElement("div");
    layersList.className = "layers-list";

    if (config.collapsible) {
      var toggleButton = document.createElement("button");
      toggleButton.type = "button";
      toggleButton.className = "toggle-button";
      toggleButton.setAttribute("aria-expanded", "false");
      toggleButton.setAttribute("aria-label", "Toggle layers list");
      if (config.use_icon) {
        container.classList.add("icon-only");
        toggleButton.innerHTML = LAYERS_ICON_SVG;
      } else {
        toggleButton.textContent = "Layers";
      }
      toggleButton.onclick = function () {
        var open = container.classList.toggle("open");
        toggleButton.setAttribute("aria-expanded", open ? "true" : "false");
      };
      container.appendChild(toggleButton);
    }
    container.appendChild(layersList);

    this._resolveEntries().forEach(function (entry) {
      layersList.appendChild(self._buildLink(entry));
    });

    // Single mode shows at most one entry: the first visible entry wins and
    // any later visible entries are switched off
    if (this._single) {
      var firstActive = false;
      this._links.forEach(function (entryState) {
        if (entryState.el.className !== "active") {
          return;
        }
        if (firstActive) {
          self._setEntryState(entryState, false);
        } else {
          firstActive = true;
        }
      });
    }

    return container;
  };

  MapglLayersControl.prototype.onRemove = function () {
    if (this._container && this._container.parentNode) {
      this._container.parentNode.removeChild(this._container);
    }
    if (this._map && this._map._mapglLayersControls) {
      var idx = this._map._mapglLayersControls.indexOf(this);
      if (idx !== -1) {
        this._map._mapglLayersControls.splice(idx, 1);
      }
    }
    this._map = null;
    this._container = null;
    this._links = [];
  };

  // Re-derive each entry's active state from actual layer visibility (used
  // after style reloads, when visibility is restored from saved state)
  MapglLayersControl.prototype.syncVisibilityStates = function () {
    var self = this;
    this._links.forEach(function (entry) {
      var visibility = self._getVisibility(entry.ids[0]);
      if (visibility === null) {
        return; // layer not (yet) present — leave the link untouched
      }
      var active = visibility !== "none";
      entry.el.className = active ? "active" : "";
      entry.ids.forEach(function (id) {
        self._setLegendDisplay(id, active);
      });
    });
  };

  // Normalize the three payload shapes into [{label, ids, type}]
  MapglLayersControl.prototype._resolveEntries = function () {
    var config = this._config;
    var entries = [];

    if (config.layers_config && Array.isArray(config.layers_config)) {
      config.layers_config.forEach(function (c) {
        var ids = Array.isArray(c.ids) ? c.ids : [c.ids];
        entries.push({ label: c.label, ids: ids, type: c.type || "single" });
      });
      return entries;
    }

    var layers = config.layers;
    if (layers != null) {
      if (!Array.isArray(layers)) {
        layers = [layers];
      }
    } else {
      // Regular GL style layers only, minus the basemap's own layers when
      // known; flowmap layers live outside the style and are not discovered
      var styleLayers = (this._map.getStyle() || {}).layers || [];
      var basemapIds = this._map._basemapLayerIds;
      layers = styleLayers
        .map(function (l) {
          return l.id;
        })
        .filter(function (id) {
          return !basemapIds || !basemapIds.has(id);
        });
    }
    layers.forEach(function (id) {
      entries.push({ label: id, ids: [id], type: "single" });
    });
    return entries;
  };

  MapglLayersControl.prototype._buildLink = function (entry) {
    var self = this;
    var link = document.createElement("a");
    link.id = entry.ids.join("-");
    link.href = "#";
    link.textContent = entry.label;
    link.setAttribute("data-layer-ids", JSON.stringify(entry.ids));
    link.setAttribute("data-layer-type", entry.type);

    var entryState = { el: link, ids: entry.ids };
    this._links.push(entryState);

    var active = this._getVisibility(entry.ids[0]) !== "none";
    link.className = active ? "active" : "";
    if (!active) {
      entry.ids.forEach(function (id) {
        self._setLegendDisplay(id, false);
      });
    }

    link.onclick = function (e) {
      e.preventDefault();
      e.stopPropagation();
      if (self._single) {
        if (self._getVisibility(entryState.ids[0]) !== "none") {
          return; // the active entry stays on in single mode
        }
        self._links.forEach(function (other) {
          if (other !== entryState) {
            self._setEntryState(other, false);
          }
        });
        self._setEntryState(entryState, true);
        return;
      }
      self._setEntryState(
        entryState,
        self._getVisibility(entryState.ids[0]) === "none",
      );
    };

    return link;
  };

  // Show or hide all of an entry's layers, its linked legends, and its link
  MapglLayersControl.prototype._setEntryState = function (entry, show) {
    var self = this;
    entry.ids.forEach(function (id) {
      self._setVisibility(id, show ? "visible" : "none");
      self._setLegendDisplay(id, show);
    });
    entry.el.className = show ? "active" : "";
  };

  MapglLayersControl.prototype._getVisibility = function (layerId) {
    var map = this._map;
    if (
      window.MapGLFlowmapPlugin &&
      window.MapGLFlowmapPlugin.hasLayer(map, layerId)
    ) {
      return window.MapGLFlowmapPlugin.getVisibility(map, layerId);
    }
    if (map.getLayer(layerId)) {
      return map.getLayoutProperty(layerId, "visibility") || "visible";
    }
    return null;
  };

  MapglLayersControl.prototype._setVisibility = function (layerId, visibility) {
    var map = this._map;
    if (
      window.MapGLFlowmapPlugin &&
      window.MapGLFlowmapPlugin.setVisibility(map, layerId, visibility)
    ) {
      return;
    }
    if (map.getLayer(layerId)) {
      map.setLayoutProperty(layerId, "visibility", visibility);
    }
  };

  // Legend sync is per-map: only legends inside this map's container are
  // toggled (compare sides stay independent; target = "compare" legends and
  // other widgets on the page are untouched)
  MapglLayersControl.prototype._setLegendDisplay = function (layerId, show) {
    if (!this._map) {
      return;
    }
    var container = this._map.getContainer();
    if (!container) {
      return;
    }
    container
      .querySelectorAll('.mapboxgl-legend[data-layer-id="' + layerId + '"]')
      .forEach(function (legend) {
        legend.style.display = show ? "" : "none";
      });
  };

  window.MapglLayersControl = MapglLayersControl;
})();
