// Shared Terra Draw control for all mapgl widgets (mapboxgl / maplibregl,
// standalone and compare). Implements the GL IControl interface so the
// control participates in the native control stack via map.addControl(),
// and exposes a MapboxDraw-compatible facade (getAll / deleteAll / add /
// getMode / get / setFeatureProperty) so the existing drawn-features
// handlers work unchanged for either draw provider.
//
// The constructor accepts the R payload object verbatim (either the initial
// x.draw_control or a proxy message) plus fields injected by the binding
// call site:
//   gl:      "maplibre" | "mapbox" — selects the vendored adapter; never
//            feature-detected, because a page can embed both widget types
//   sync:    { inputId, syncUrl, mapglId } — drawn-features round trip
//   helpers: optional page-global helpers from the standalone bindings
//            (createMeasurementBox, formatMeasurements,
//            initializeDrawAttributeEditor); compare bindings omit them and
//            measurements / the attribute editor degrade to no-ops there.
//
// Terra Draw's adapters do not survive map.setStyle() (upstream #590), so
// the control owns style survival: on style.load it snapshots the store,
// tears the instance down, and rebuilds with a fresh adapter. Selection and
// undo history reset by design; an unfinished drawing is discarded.
(function () {
  "use strict";

  var PREFIX_ID = "mapgl-terradraw";

  // Properties terra-draw stamps on guidance features (never user data);
  // curveGuidance marks the mapgl curve modes' handle/node helpers
  var GUIDANCE_PROPS = [
    "midPoint",
    "selectionPoint",
    "coordinatePoint",
    "snappingPoint",
    "closingPoint",
    "curveGuidance",
  ];

  // Transient/reserved state stripped from anything R-facing
  var TRANSIENT_PROPS = [
    "selected",
    "currentlyDrawing",
    "edited",
    "coordinatePointFeatureId",
    "coordinatePointIds",
    "provisionalCoordinateCount",
    "committedCoordinateCount",
  ];

  var UUID_RE =
    /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;

  // Terra Draw rejects coordinates with more than 9 decimal places (the
  // adapter default), and real-world data (sf objects) carries full double
  // precision — round on the way in (~0.1mm, lossless in practice)
  var COORDINATE_PRECISION_FACTOR = 1e9;

  function roundGeometryCoordinates(coords) {
    if (typeof coords[0] === "number") {
      return coords.map(function (value) {
        return Math.round(value * COORDINATE_PRECISION_FACTOR) /
          COORDINATE_PRECISION_FACTOR;
      });
    }
    return coords.map(roundGeometryCoordinates);
  }

  // Class names resolved from window.terraDraw, or from
  // window.MapglTerraDrawModes for mapgl-authored custom modes
  var MODE_CLASSES = {
    point: "TerraDrawPointMode",
    linestring: "TerraDrawLineStringMode",
    polygon: "TerraDrawPolygonMode",
    rectangle: "TerraDrawRectangleMode",
    circle: "TerraDrawCircleMode",
    freehand: "TerraDrawFreehandMode",
    "freehand-linestring": "TerraDrawFreehandLineStringMode",
    "angled-rectangle": "TerraDrawAngledRectangleMode",
    sector: "TerraDrawSectorMode",
    sensor: "TerraDrawSensorMode",
    curve: "TerraDrawCurveMode",
    "curve-linestring": "TerraDrawCurveLineStringMode",
    select: "TerraDrawSelectMode",
  };

  var CURVE_MODES = ["curve", "curve-linestring"];

  function resolveModeClass(name) {
    var className = MODE_CLASSES[name];
    return (
      window.terraDraw[className] ||
      (window.MapglTerraDrawModes || {})[className]
    );
  }

  // Modes whose features are polygon-shaped (share the fill styling family)
  var POLYGON_FAMILY = [
    "polygon",
    "rectangle",
    "circle",
    "freehand",
    "angled-rectangle",
    "sector",
    "sensor",
    "curve",
  ];

  var MODE_TITLES = {
    point: "Draw point",
    linestring: "Draw line",
    polygon: "Draw polygon",
    rectangle: "Draw rectangle",
    circle: "Draw circle",
    freehand: "Draw freehand polygon",
    "freehand-linestring": "Draw freehand line",
    "angled-rectangle": "Draw angled rectangle",
    sector: "Draw sector",
    sensor: "Draw sensor",
    curve: "Draw curved polygon",
    "curve-linestring": "Draw curved line",
    select: "Select and edit features",
  };

  // Icon glyphs follow the Feather Icons visual language (24px viewBox, 2px
  // currentColor stroke). sensor, select, trash, and download are adapted
  // from Feather (MIT, https://feathericons.com); the remaining glyphs are
  // mapgl-drawn in the same style (Feather has no GIS shapes).
  var SVG_OPEN =
    '<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">';

  var MODE_ICONS = {
    point: SVG_OPEN + '<circle cx="12" cy="12" r="3.5" fill="currentColor" stroke="none"></circle></svg>',
    linestring:
      SVG_OPEN +
      '<path d="M4 19 10 9l5 5 5-10"></path><circle cx="4" cy="19" r="1.6" fill="currentColor" stroke="none"></circle><circle cx="20" cy="4" r="1.6" fill="currentColor" stroke="none"></circle></svg>',
    polygon:
      SVG_OPEN +
      '<polygon points="12 3 21 9.5 17.5 20 6.5 20 3 9.5"></polygon></svg>',
    rectangle: SVG_OPEN + '<rect x="4" y="6" width="16" height="12"></rect></svg>',
    circle: SVG_OPEN + '<circle cx="12" cy="12" r="8.5"></circle></svg>',
    freehand:
      SVG_OPEN +
      '<path d="M5 14c-2-5 2-9 6-9s9 2 8 7-5 4-7 7c-1.5 2.2-5-1-7-5z"></path></svg>',
    "freehand-linestring":
      SVG_OPEN + '<path d="M3 17c4 3 5-9 9-7s2 8 9-4"></path></svg>',
    "angled-rectangle":
      SVG_OPEN +
      '<polygon points="7.2 3.5 20.5 9.7 16.8 20.5 3.5 14.3"></polygon></svg>',
    sector:
      SVG_OPEN + '<path d="M6 20V8a12 12 0 0 1 12 12z"></path></svg>',
    sensor:
      SVG_OPEN +
      '<path d="M4 11a9 9 0 0 1 9 9"></path><path d="M4 4a16 16 0 0 1 16 16"></path><circle cx="5" cy="19" r="1.5" fill="currentColor" stroke="none"></circle></svg>',
    curve:
      SVG_OPEN +
      '<path d="M4 20 V12 C 4 3, 20 3, 20 12 V20 Z"></path></svg>',
    "curve-linestring":
      SVG_OPEN +
      '<path d="M4 19 C 8 7, 16 7, 20 19"></path><path d="M7 9.7 L17 9.7"></path><circle cx="7" cy="9.7" r="1.4" fill="currentColor" stroke="none"></circle><circle cx="17" cy="9.7" r="1.4" fill="currentColor" stroke="none"></circle></svg>',
    select:
      SVG_OPEN +
      '<path d="M3 3l7.07 16.97 2.51-7.39 7.39-2.51L3 3z"></path><path d="M13 13l6 6"></path></svg>',
    trash:
      SVG_OPEN +
      '<polyline points="3 6 5 6 21 6"></polyline><path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"></path><line x1="10" y1="11" x2="10" y2="17"></line><line x1="14" y1="11" x2="14" y2="17"></line></svg>',
    download:
      SVG_OPEN +
      '<path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"></path><polyline points="7 10 12 15 17 10"></polyline><line x1="12" y1="15" x2="12" y2="3"></line></svg>',
  };

  function isTerraDrawId(id) {
    return (
      typeof id === "string" &&
      (id === PREFIX_ID || id.indexOf(PREFIX_ID + "-") === 0)
    );
  }

  function isGuidanceFeature(feature) {
    var props = (feature && feature.properties) || {};
    for (var i = 0; i < GUIDANCE_PROPS.length; i++) {
      if (props[GUIDANCE_PROPS[i]]) return true;
    }
    return false;
  }

  function baseModeForGeometry(geometry) {
    if (!geometry) return null;
    if (geometry.type === "Point") return "point";
    if (geometry.type === "LineString") return "linestring";
    if (geometry.type === "Polygon") return "polygon";
    return null;
  }

  // Shallow-merge b into a, with the `styles` sub-object merged key-by-key
  // (precedence rule for terradraw_options(modes = ...) raw overrides)
  function mergeModeOptions(a, b) {
    var out = {};
    var key;
    for (key in a) out[key] = a[key];
    for (key in b) {
      if (key === "styles" && out.styles && b.styles) {
        var styles = {};
        var s;
        for (s in out.styles) styles[s] = out.styles[s];
        for (s in b.styles) styles[s] = b.styles[s];
        out.styles = styles;
      } else {
        out[key] = b[key];
      }
    }
    return out;
  }

  function MapglTerraDrawControl(config) {
    this._config = config || {};
    this._modes = this._resolveModeNames();
    this._map = null;
    this._container = null;
    this._buttons = {};
    this._draw = null;
    this._selectedIds = [];
    this._suppressSync = false;
    this._rebuilding = false;
    this._syncTimer = null;
    this._measureHideTimer = null;
    this._styleLoadHandler = null;
    this._editor = null;
    this._measurementBox = null;
    this._panLocked = false;
  }

  MapglTerraDrawControl.isTerraDrawId = isTerraDrawId;
  MapglTerraDrawControl.PREFIX_ID = PREFIX_ID;

  // Requested modes in R-supplied order; base modes are additionally
  // registered (buttonless) so addFeatures() always finds an owning mode
  MapglTerraDrawControl.prototype._resolveModeNames = function () {
    var requested = this._config.modes || [];
    if (typeof requested === "string") requested = [requested];
    return requested.filter(function (m) {
      return Object.prototype.hasOwnProperty.call(MODE_CLASSES, m);
    });
  };

  // The authoritative effective-mode list: requested modes plus the
  // always-registered buttonless base modes (so addFeatures always finds an
  // owning mode — including "curve"/"curve-linestring", whose features would
  // otherwise be silently demoted to plain polygons/linestrings on an R
  // round trip), filtered to classes that actually resolved (a missing
  // class is a packaging bug; it is reported once and excluded everywhere:
  // construction, select flags, and toolbar state all consume this list).
  MapglTerraDrawControl.prototype._registeredModeNames = function () {
    var names = this._modes.slice();
    [
      "point",
      "linestring",
      "polygon",
      "curve",
      "curve-linestring",
      "select",
    ].forEach(function (m) {
      if (names.indexOf(m) === -1) names.push(m);
    });
    return names.filter(function (name) {
      if (resolveModeClass(name)) return true;
      console.error(
        "mapgl terra-draw: mode class for \"" +
          name +
          "\" is not loaded; the mode is disabled",
      );
      return false;
    });
  };

  MapglTerraDrawControl.prototype._hasButtonMode = function (name) {
    return this._modes.indexOf(name) !== -1;
  };

  MapglTerraDrawControl.prototype.onAdd = function (map) {
    var self = this;
    this._map = map;

    map._mapglTerraDrawControls = map._mapglTerraDrawControls || [];
    map._mapglTerraDrawControls.push(this);

    var container = document.createElement("div");
    this._container = container;
    // Dual-prefix so the native ctrl-group chrome applies under either engine
    container.className =
      "mapboxgl-ctrl maplibregl-ctrl mapboxgl-ctrl-group maplibregl-ctrl-group mapgl-terradraw";
    if (this._config.orientation === "horizontal") {
      container.classList.add("mapgl-terradraw-horizontal");
    }

    this._modes.forEach(function (mode) {
      if (mode === "select") return; // select gets its button after the shape tools
      container.appendChild(self._buildModeButton(mode));
    });
    if (this._hasButtonMode("select")) {
      container.appendChild(this._buildModeButton("select"));
    }
    container.appendChild(this._buildTrashButton());
    if (this._config.download_button) {
      container.appendChild(this._buildDownloadButton());
    }

    var init = function () {
      self._createDraw();
      self._initHelpers();
      self._replayInitialFeatures();
      // Registered after the initial build so only subsequent style swaps
      // trigger a rebuild (the adapter does not survive setStyle upstream)
      self._styleLoadHandler = function () {
        self._rebuild();
      };
      map.on("style.load", self._styleLoadHandler);
    };

    if (!map.isStyleLoaded || map.isStyleLoaded()) {
      init();
    } else {
      map.once("idle", init);
    }

    return container;
  };

  MapglTerraDrawControl.prototype.onRemove = function () {
    var map = this._map;
    clearTimeout(this._syncTimer);
    clearTimeout(this._measureHideTimer);
    if (map && this._styleLoadHandler) {
      map.off("style.load", this._styleLoadHandler);
      this._styleLoadHandler = null;
    }
    if (this._editor) {
      if (typeof this._editor.destroy === "function") {
        this._editor.destroy();
      } else if (typeof this._editor.hide === "function") {
        this._editor.hide();
      }
      this._editor = null;
    }
    if (this._measurementBox && this._measurementBox.parentNode) {
      this._measurementBox.parentNode.removeChild(this._measurementBox);
    }
    this._measurementBox = null;
    if (this._draw) {
      try {
        this._draw.stop();
      } catch (e) {
        // MapLibre fires error events rather than throwing; Mapbox may throw
        // if the style was already torn down
      }
      this._draw = null;
    }
    if (this._panLocked && map && map.dragPan) {
      try {
        map.dragPan.enable();
      } catch (e) {}
      this._panLocked = false;
    }
    if (map) {
      if (map._mapglTerraDrawControls) {
        var idx = map._mapglTerraDrawControls.indexOf(this);
        if (idx !== -1) map._mapglTerraDrawControls.splice(idx, 1);
      }
      if (map._mapgl_draw === this) {
        map._mapgl_draw = null;
      }
    }
    var widget = this._findWidget();
    if (widget && widget.drawControl === this) {
      widget.drawControl = null;
    }
    if (this._container && this._container.parentNode) {
      this._container.parentNode.removeChild(this._container);
    }
    this._container = null;
    this._buttons = {};
    this._selectedIds = [];
    this._map = null;
  };

  // ---------------------------------------------------------------------
  // Toolbar

  MapglTerraDrawControl.prototype._buildModeButton = function (mode) {
    var self = this;
    var button = document.createElement("button");
    button.type = "button";
    button.className = "mapgl-terradraw-button mapgl-terradraw-mode-" + mode;
    button.title = MODE_TITLES[mode] || mode;
    button.setAttribute("aria-pressed", "false");
    button.innerHTML = MODE_ICONS[mode] || "";
    button.onclick = function () {
      if (!self._draw) return;
      if (self._draw.getMode() === mode) {
        // Toggling the active tool off returns to select (or static)
        self._setActiveMode(self._idleMode());
      } else {
        self._setActiveMode(mode);
      }
    };
    this._buttons[mode] = button;
    return button;
  };

  MapglTerraDrawControl.prototype._buildTrashButton = function () {
    var self = this;
    var button = document.createElement("button");
    button.type = "button";
    button.className = "mapgl-terradraw-button mapgl-terradraw-trash";
    button.title = "Delete selected feature";
    button.innerHTML = MODE_ICONS.trash;
    button.onclick = function () {
      self._trash();
    };
    this._buttons.trash = button;
    this._updateTrashState();
    return button;
  };

  MapglTerraDrawControl.prototype._buildDownloadButton = function () {
    var self = this;
    var button = document.createElement("button");
    button.type = "button";
    button.className = "mapgl-terradraw-button mapgl-terradraw-download";
    button.title = "Download drawn features as GeoJSON";
    button.innerHTML = MODE_ICONS.download;
    button.onclick = function () {
      var fc = self.getAll();
      if (!fc.features.length) {
        alert("No features to download. Please draw something first!");
        return;
      }
      var blob = new Blob([JSON.stringify(fc, null, 2)], {
        type: "application/geo+json",
      });
      var url = URL.createObjectURL(blob);
      var link = document.createElement("a");
      link.href = url;
      link.download =
        (self._config.download_filename || "drawn-features") + ".geojson";
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
      URL.revokeObjectURL(url);
    };
    return button;
  };

  MapglTerraDrawControl.prototype._refreshButtonStates = function () {
    var active = this._draw ? this._draw.getMode() : null;
    var buttons = this._buttons;
    Object.keys(buttons).forEach(function (mode) {
      if (mode === "trash") return;
      var on = mode === active;
      buttons[mode].classList.toggle("active", on);
      buttons[mode].setAttribute("aria-pressed", on ? "true" : "false");
    });
    this._updateTrashState();
  };

  MapglTerraDrawControl.prototype._updateTrashState = function () {
    var trash = this._buttons.trash;
    if (!trash) return;
    var enabled = this._selectedIds.length > 0;
    trash.classList.toggle("mapgl-terradraw-disabled", !enabled);
    trash.title = enabled
      ? "Delete selected feature"
      : "Select a feature to delete it";
  };

  // Mode to fall back to when no tool is engaged
  MapglTerraDrawControl.prototype._idleMode = function () {
    return this._hasButtonMode("select") ? "select" : "static";
  };

  MapglTerraDrawControl.prototype._setActiveMode = function (mode) {
    if (!this._draw) return;
    try {
      this._draw.setMode(mode);
    } catch (e) {
      console.warn("mapgl terra-draw: could not activate mode " + mode, e);
    }
    this._refreshButtonStates();
    this._updatePanLock();
  };

  // Provider-neutral hook for the bindings' feature-click suppression
  MapglTerraDrawControl.prototype.isDrawing = function () {
    if (!this._draw) return false;
    var mode = this._draw.getMode();
    return mode !== "select" && mode !== "static";
  };

  // ---------------------------------------------------------------------
  // Terra Draw instance

  MapglTerraDrawControl.prototype._createDraw = function () {
    var config = this._config;
    var adapterNamespace =
      config.gl === "mapbox"
        ? window.terraDrawMapboxGlAdapter
        : window.terraDrawMaplibreGlAdapter;
    if (!window.terraDraw || !adapterNamespace) {
      console.error(
        "mapgl terra-draw: terra-draw scripts are not loaded on this page",
      );
      return;
    }
    var Adapter =
      config.gl === "mapbox"
        ? adapterNamespace.TerraDrawMapboxGLAdapter
        : adapterNamespace.TerraDrawMapLibreGLAdapter;

    var self = this;
    var modes = this._registeredModeNames().map(function (name) {
      var ModeClass = resolveModeClass(name);
      return new ModeClass(self._modeConfig(name));
    });

    this._draw = new window.terraDraw.TerraDraw({
      adapter: new Adapter({ map: this._map, prefixId: PREFIX_ID }),
      modes: modes,
    });
    this._wireEvents();
    this._draw.start();
    this._setActiveMode(this._idleMode());
  };

  // Layered option assembly for one mode. Precedence: mapgl defaults ->
  // top-level styling args -> terradraw_options() behavior args -> raw
  // per-mode overrides from terradraw_options(modes = ...), styles merged
  // key-by-key and other keys replacing wholesale.
  MapglTerraDrawControl.prototype._modeConfig = function (name) {
    var styling = this._config.styling || {};
    var td = this._config.terradraw || {};
    var options = {};
    var styles = {};

    if (name === "point") {
      styles.pointColor = styling.point_color;
      styles.pointWidth = styling.vertex_radius;
      styles.pointOutlineColor = "#FFFFFF";
      styles.pointOutlineWidth = 2;
    } else if (
      name === "linestring" ||
      name === "freehand-linestring" ||
      name === "curve-linestring"
    ) {
      styles.lineStringColor = styling.line_color;
      styles.lineStringWidth = styling.line_width;
    } else if (POLYGON_FAMILY.indexOf(name) !== -1) {
      styles.fillColor = styling.fill_color;
      styles.fillOpacity = styling.fill_opacity;
      styles.outlineColor = styling.line_color;
      styles.outlineWidth = styling.line_width;
    } else if (name === "select") {
      styles.selectedPointColor = styling.active_color;
      styles.selectedPointOutlineColor = "#FFFFFF";
      styles.selectedLineStringColor = styling.active_color;
      styles.selectedPolygonColor = styling.active_color;
      styles.selectedPolygonFillOpacity = styling.fill_opacity;
      styles.selectedPolygonOutlineColor = styling.active_color;
      styles.selectionPointColor = styling.active_color;
      styles.selectionPointWidth = styling.vertex_radius;
      styles.selectionPointOutlineColor = "#FFFFFF";
      styles.midPointColor = styling.active_color;
    }
    if (CURVE_MODES.indexOf(name) !== -1) {
      // node anchors tinted like the active/selection chrome; handle
      // styling defaults live in the mode and are overridable via the raw
      // terradraw_options(modes=) escape hatch
      styles.nodeColor = styling.active_color;
    }
    Object.keys(styles).forEach(function (key) {
      if (styles[key] == null) delete styles[key];
    });
    if (Object.keys(styles).length) options.styles = styles;

    if (CURVE_MODES.indexOf(name) !== -1) {
      // pointer-down bridge: the adapter reports onDragStart with the
      // threshold-crossing event, so the mode captures the true press
      // position itself from the map container
      var mapRef = this._map;
      options.mapglGetContainer = function () {
        return mapRef ? mapRef.getContainer() : null;
      };
    }

    // terradraw_options() behavior arguments
    var snapping = null;
    if (td.snap_to_coordinates || td.snap_to_lines) {
      snapping = {
        toCoordinate: !!td.snap_to_coordinates,
        toLine: !!td.snap_to_lines,
      };
    }
    if (name === "polygon" || name === "linestring") {
      if (snapping) options.snapping = snapping;
      if (td.editable_while_drawing) options.editable = true;
      if (td.show_coordinate_points) options.showCoordinatePoints = true;
    }
    if (name === "point" && td.editable_while_drawing) {
      options.editable = true;
    }
    if ((name === "rectangle" || name === "circle") && td.drag_interaction) {
      options.drawInteraction = td.drag_interaction;
    }
    if (name !== "select" && td.pointer_distance != null) {
      options.pointerDistance = td.pointer_distance;
    }

    if (name === "select") {
      options.flags = this._selectFlags();
    }

    var raw = td.modes && td.modes[name];
    if (raw) {
      options = mergeModeOptions(options, raw);
      delete options.modeName;
    }
    return options;
  };

  MapglTerraDrawControl.prototype._selectFlags = function () {
    var td = this._config.terradraw || {};
    var val = function (v, fallback) {
      return v == null ? fallback : !!v;
    };
    var flags = {};
    var self = this;
    this._registeredModeNames().forEach(function (name) {
      if (name === "select") return;
      if (CURVE_MODES.indexOf(name) !== -1) {
        // Move-only: vertex drags would desync the densified geometry from
        // the curveNodes control net, and rotate/scale are non-translations
        // the metadata resync cannot repair
        flags[name] = { feature: { draggable: val(td.drag_features, true) } };
        return;
      }
      var feature = {
        draggable: val(td.drag_features, true),
      };
      if (val(td.rotate_features, false)) feature.rotateable = true;
      if (val(td.scale_features, false)) feature.scaleable = true;
      if (name !== "point") {
        var coordinates = {
          draggable: val(td.drag_vertices, true),
          deletable: val(td.delete_vertices, true),
          midpoints: val(td.midpoints, true),
        };
        if (td.snap_to_coordinates || td.snap_to_lines) {
          // `snappable: true` would mean coordinate snapping only — pass the
          // explicit shape so snap_to_lines works while editing too
          coordinates.snappable = {
            toCoordinate: !!td.snap_to_coordinates,
            toLine: !!td.snap_to_lines,
          };
        }
        if (td.resize) {
          coordinates.resizable = td.resize;
          // terra-draw disallows midpoints alongside resizable
          coordinates.midpoints = false;
        }
        feature.coordinates = coordinates;
      }
      flags[name] = { feature: feature };
    });
    return flags;
  };

  MapglTerraDrawControl.prototype._keepModeActive = function () {
    var td = this._config.terradraw || {};
    return !!td.keep_mode_active;
  };

  // ---------------------------------------------------------------------
  // Events

  MapglTerraDrawControl.prototype._wireEvents = function () {
    var self = this;
    var draw = this._draw;

    draw.on("change", function (ids, type) {
      if (type === "styling") return;
      if (type === "delete") {
        self._selectedIds = self._selectedIds.filter(function (id) {
          return !ids || ids.indexOf(id) === -1;
        });
        self._updateTrashState();
        self._fireSynthetic("draw.delete", []);
      }
      if (
        (type === "create" || type === "update") &&
        self._isDrawingState()
      ) {
        self._showMeasurementsFor(ids && ids[ids.length - 1]);
      }
      self._scheduleSync(type === "update");
    });

    draw.on("finish", function (id, context) {
      var action = context && context.action;
      if (action === "dragFeature") {
        // re-sync BEFORE the public feature is read or draw.update fires so
        // listeners and R always see geometry and metadata in agreement
        self._resyncCurveNodes(id);
      }
      var feature = self._publicFeature(id);
      if (action === "draw") {
        self._fireSynthetic("draw.create", feature ? [feature] : []);
        self._showMeasurementsFor(id, true);
        if (!self._keepModeActive()) {
          self._setActiveMode(self._idleMode());
          if (self._hasButtonMode("select")) {
            try {
              draw.selectFeature(id);
            } catch (e) {
              // feature may fail selection flags; selection is best-effort
            }
          }
        }
      } else {
        self._fireSynthetic("draw.update", feature ? [feature] : []);
        self._showMeasurementsFor(id, true);
      }
      self._scheduleSync(false);
    });

    draw.on("select", function (id) {
      self._selectedIds = [id];
      self._updateTrashState();
      var feature = self._publicFeature(id);
      self._fireSynthetic("draw.selectionchange", feature ? [feature] : []);
      self._showMeasurementsFor(id, true);
    });

    draw.on("deselect", function () {
      self._selectedIds = [];
      self._updateTrashState();
      self._fireSynthetic("draw.selectionchange", []);
      self._hideMeasurements();
    });
  };

  MapglTerraDrawControl.prototype._isDrawingState = function () {
    try {
      return this._draw && this._draw.getModeState() === "drawing";
    } catch (e) {
      return false;
    }
  };

  // Whole-feature drags translate a curve's geometry; translate its
  // curveNodes control net by the same delta so the metadata stays true.
  // The ring/line starts at node 0's anchor by construction.
  MapglTerraDrawControl.prototype._resyncCurveNodes = function (id) {
    if (!this._draw || !window.MapglTerraDrawModes) return;
    var feature;
    try {
      feature = this._draw.getSnapshotFeature(id);
    } catch (e) {
      return;
    }
    if (!feature || typeof (feature.properties || {}).curveNodes !== "string") {
      return;
    }
    var nodes;
    try {
      nodes = JSON.parse(feature.properties.curveNodes);
    } catch (e) {
      return;
    }
    if (!Array.isArray(nodes) || !nodes.length || !nodes[0].coords) return;
    var anchor =
      feature.geometry.type === "Polygon"
        ? feature.geometry.coordinates[0][0]
        : feature.geometry.coordinates[0];
    var dLng = anchor[0] - nodes[0].coords[0];
    var dLat = anchor[1] - nodes[0].coords[1];
    if (dLng === 0 && dLat === 0) return;
    window.MapglTerraDrawModes.translateCurveNodes(nodes, dLng, dLat);
    try {
      this._draw.updateFeatureProperties(id, {
        curveNodes: JSON.stringify(nodes),
      });
    } catch (e) {
      console.warn("mapgl terra-draw: could not re-sync curve metadata", e);
    }
  };

  // Pen-tool curve modes claim left-drag for handle placement, so map
  // panning is disabled for the whole time a curve tool is active and
  // restored on every exit path (mode change, finish, rebuild, removal)
  MapglTerraDrawControl.prototype._updatePanLock = function () {
    var map = this._map;
    if (!map || !map.dragPan) return;
    var mode = this._draw ? this._draw.getMode() : null;
    var lock = mode === "curve" || mode === "curve-linestring";
    try {
      if (lock && !this._panLocked) {
        map.dragPan.disable();
        this._panLocked = true;
      } else if (!lock && this._panLocked) {
        map.dragPan.enable();
        this._panLocked = false;
      }
    } catch (e) {}
  };

  // Synthetic MapboxDraw-style map events so engine-agnostic consumers (the
  // attribute editor) work unchanged. Safe: a map has at most one draw
  // control, and the terra path never attaches the MapboxDraw listeners.
  MapglTerraDrawControl.prototype._fireSynthetic = function (name, features) {
    if (!this._map || typeof this._map.fire !== "function") return;
    try {
      this._map.fire(name, { features: features });
    } catch (e) {
      // never let a listener error break drawing
    }
  };

  MapglTerraDrawControl.prototype._trash = function () {
    if (!this._draw || !this._selectedIds.length) return;
    var ids = this._selectedIds.slice();
    ids.forEach(function (id) {
      try {
        this._draw.deselectFeature(id);
      } catch (e) {}
    }, this);
    try {
      this._draw.removeFeatures(ids);
    } catch (e) {
      console.warn("mapgl terra-draw: could not delete selection", e);
    }
    this._selectedIds = [];
    this._updateTrashState();
    this._hideMeasurements();
  };

  // ---------------------------------------------------------------------
  // Sync (self-contained: compare bindings have no shared sync helper)

  MapglTerraDrawControl.prototype._findWidget = function () {
    var sync = this._config.sync || {};
    if (!sync.inputId || typeof HTMLWidgets === "undefined") return null;
    try {
      return HTMLWidgets.find("#" + sync.inputId) || null;
    } catch (e) {
      return null;
    }
  };

  MapglTerraDrawControl.prototype._syncNow = function () {
    var sync = this._config.sync || {};
    var fc = this.getAll();
    if (
      typeof HTMLWidgets !== "undefined" &&
      HTMLWidgets.shinyMode &&
      typeof Shiny !== "undefined" &&
      sync.inputId
    ) {
      Shiny.setInputValue(sync.inputId + "_drawn_features", fc, {
        priority: "event",
      });
    } else if (sync.syncUrl && sync.mapglId && typeof fetch === "function") {
      fetch(sync.syncUrl, {
        method: "POST",
        body: JSON.stringify(fc),
      }).catch(function (e) {
        if (typeof console !== "undefined" && console.debug) {
          console.debug("mapgl draw sync failed", e);
        }
      });
    }
    var widget = this._findWidget();
    if (widget) {
      widget.drawFeatures = fc;
    }
  };

  // Terra change events fire per drag frame, so update-type changes are
  // debounced on both the Shiny and httpuv paths (create/delete immediate)
  MapglTerraDrawControl.prototype._scheduleSync = function (debounce) {
    if (this._suppressSync) return;
    var self = this;
    clearTimeout(this._syncTimer);
    if (debounce) {
      this._syncTimer = setTimeout(function () {
        self._syncNow();
      }, 150);
    } else {
      self._syncNow();
    }
  };

  // ---------------------------------------------------------------------
  // Public output sanitation

  MapglTerraDrawControl.prototype._publicFeatures = function () {
    if (!this._draw) return [];
    var snapshot;
    try {
      snapshot = this._draw.getSnapshot() || [];
    } catch (e) {
      return [];
    }
    return snapshot.filter(function (feature) {
      return !isGuidanceFeature(feature);
    }).map(sanitizeFeature);
  };

  function sanitizeFeature(feature) {
    var props = {};
    var source = feature.properties || {};
    Object.keys(source).forEach(function (key) {
      if (TRANSIENT_PROPS.indexOf(key) !== -1) return;
      props[key] = source[key];
    });
    var out = {
      type: "Feature",
      geometry: feature.geometry,
      properties: props,
    };
    if (feature.id != null) out.id = feature.id;
    return out;
  }

  MapglTerraDrawControl.prototype._publicFeature = function (id) {
    if (!this._draw) return null;
    var feature = null;
    try {
      feature = this._draw.getSnapshotFeature(id);
    } catch (e) {
      return null;
    }
    if (!feature || isGuidanceFeature(feature)) return null;
    return sanitizeFeature(feature);
  };

  // ---------------------------------------------------------------------
  // MapboxDraw-compatible facade

  MapglTerraDrawControl.prototype.getAll = function () {
    return { type: "FeatureCollection", features: this._publicFeatures() };
  };

  MapglTerraDrawControl.prototype.deleteAll = function () {
    if (!this._draw) return;
    try {
      this._draw.clear();
    } catch (e) {
      console.warn("mapgl terra-draw: could not clear features", e);
    }
    this._selectedIds = [];
    this._updateTrashState();
    this._hideMeasurements();
    this._fireSynthetic("draw.delete", []);
    this._scheduleSync(false);
  };

  MapglTerraDrawControl.prototype.getMode = function () {
    return this._draw ? this._draw.getMode() : "static";
  };

  MapglTerraDrawControl.prototype.get = function (id) {
    return this._publicFeature(id) || undefined;
  };

  MapglTerraDrawControl.prototype.setFeatureProperty = function (
    id,
    name,
    value,
  ) {
    if (!this._draw) return;
    var props = {};
    props[name] = value;
    try {
      this._draw.updateFeatureProperties(id, props);
    } catch (e) {
      // reserved names are rejected in R; this guards direct JS misuse
      console.warn(
        "mapgl terra-draw: could not set property " + name + " on " + id,
        e,
      );
    }
  };

  MapglTerraDrawControl.prototype.getTerraDraw = function () {
    return this._draw;
  };

  // Load external features (source= / add_features_to_draw()). Terra Draw
  // rejects Multi* geometries, foreign non-UUID ids, and unregistered owning
  // modes, so features are sanitized first and failures retried once with
  // the geometry's base mode.
  MapglTerraDrawControl.prototype.add = function (data) {
    if (!this._draw || !data) return;
    var incoming;
    if (data.type === "FeatureCollection") {
      incoming = data.features || [];
    } else if (data.type === "Feature") {
      incoming = [data];
    } else if (Array.isArray(data)) {
      incoming = data;
    } else {
      incoming = [];
    }

    var registered = this._registeredModeNames();
    var prepared = [];
    var skipped = 0;
    var holesStripped = 0;

    incoming.forEach(function (feature) {
      if (!feature || !feature.geometry || !feature.geometry.type) {
        skipped++;
        return;
      }
      var geometries = [];
      var type = feature.geometry.type;
      if (type === "MultiPoint" || type === "MultiLineString" || type === "MultiPolygon") {
        var partType = type.replace("Multi", "");
        (feature.geometry.coordinates || []).forEach(function (coords) {
          geometries.push({ type: partType, coordinates: coords });
        });
      } else if (type === "GeometryCollection") {
        skipped++;
        return;
      } else {
        geometries.push(feature.geometry);
      }

      // Terra Draw does not support polygons with holes; keep the outer ring
      geometries = geometries.map(function (geometry) {
        if (
          geometry.type === "Polygon" &&
          (geometry.coordinates || []).length > 1
        ) {
          holesStripped++;
          return { type: "Polygon", coordinates: [geometry.coordinates[0]] };
        }
        return geometry;
      });

      geometries.forEach(function (geometry) {
        var baseMode = baseModeForGeometry(geometry);
        if (!baseMode) {
          skipped++;
          return;
        }
        var props = {};
        Object.keys(feature.properties || {}).forEach(function (key) {
          if (TRANSIENT_PROPS.indexOf(key) !== -1) return;
          if (GUIDANCE_PROPS.indexOf(key) !== -1) return;
          props[key] = feature.properties[key];
        });
        var mode = props.mode;
        if (typeof mode !== "string" || registered.indexOf(mode) === -1) {
          mode = baseMode;
        }
        props.mode = mode;
        var out = {
          type: "Feature",
          geometry: {
            type: geometry.type,
            coordinates: roundGeometryCoordinates(geometry.coordinates),
          },
          properties: props,
        };
        // Exploded Multi* parts must NOT share the original id — Terra Draw
        // accepts the first and rejects the rest as duplicates
        if (
          geometries.length === 1 &&
          typeof feature.id === "string" &&
          UUID_RE.test(feature.id)
        ) {
          out.id = feature.id;
        }
        prepared.push({ feature: out, baseMode: baseMode });
      });
    });

    var failures = [];
    if (prepared.length) {
      var results = [];
      try {
        results =
          this._draw.addFeatures(
            prepared.map(function (p) {
              return p.feature;
            }),
          ) || [];
      } catch (e) {
        console.warn("mapgl terra-draw: addFeatures failed", e);
        return;
      }
      var self = this;
      results.forEach(function (result, i) {
        if (!result || result.valid !== false) return;
        var entry = prepared[i];
        if (entry.feature.properties.mode !== entry.baseMode) {
          // Retry once under the geometry's base mode before giving up
          var retry = {
            type: "Feature",
            geometry: entry.feature.geometry,
            properties: {},
          };
          Object.keys(entry.feature.properties).forEach(function (key) {
            retry.properties[key] = entry.feature.properties[key];
          });
          retry.properties.mode = entry.baseMode;
          if (entry.feature.id) retry.id = entry.feature.id;
          var retried;
          try {
            retried = self._draw.addFeatures([retry]) || [];
          } catch (e) {
            retried = [];
          }
          if (retried[0] && retried[0].valid !== false) return;
          failures.push(retried[0] && retried[0].reason);
        } else {
          failures.push(result.reason);
        }
      });
    }

    if (skipped || failures.length) {
      console.warn(
        "mapgl terra-draw: " +
          (skipped + failures.length) +
          " feature(s) could not be added" +
          (failures.length ? " (" + failures.join("; ") + ")" : ""),
      );
    }
    if (holesStripped) {
      console.warn(
        "mapgl terra-draw: removed interior ring(s) from " +
          holesStripped +
          " polygon(s); Terra Draw does not support holes",
      );
    }
    this._scheduleSync(false);
  };

  // ---------------------------------------------------------------------
  // Initial feature replay (source= and add_features_to_draw() queue)

  MapglTerraDrawControl.prototype._replayInitialFeatures = function () {
    var config = this._config;
    var replayed = false;
    this._suppressSync = true;
    try {
      if (config.source && typeof config.getSourceData === "function") {
        var data = config.getSourceData(config.source);
        if (data) {
          this.add(data);
          replayed = true;
        } else {
          console.warn("Source not found or has no data:", config.source);
        }
      }
      if (Array.isArray(config.featuresQueue)) {
        var self = this;
        config.featuresQueue.forEach(function (entry) {
          if (!entry) return;
          if (entry.clear_existing && self._draw) {
            try {
              self._draw.clear();
            } catch (e) {}
          }
          if (
            entry.source &&
            typeof config.getSourceData === "function"
          ) {
            var queued = config.getSourceData(entry.source);
            if (queued) self.add(queued);
          }
          replayed = true;
        });
      }
    } finally {
      this._suppressSync = false;
    }
    if (replayed) {
      this._scheduleSync(false);
    }
  };

  // ---------------------------------------------------------------------
  // Measurements (standalone bindings inject the shared box/format helpers;
  // compare passes none and this degrades to a no-op, as today)

  MapglTerraDrawControl.prototype._measurementsEnabled = function () {
    var helpers = this._config.helpers || {};
    return (
      this._config.show_measurements &&
      typeof helpers.createMeasurementBox === "function" &&
      typeof helpers.formatMeasurements === "function" &&
      typeof window.turf !== "undefined"
    );
  };

  MapglTerraDrawControl.prototype._ensureMeasurementBox = function () {
    if (this._measurementBox) return this._measurementBox;
    var helpers = this._config.helpers || {};
    this._measurementBox = helpers.createMeasurementBox(this._map);
    return this._measurementBox;
  };

  MapglTerraDrawControl.prototype._measureFeature = function (feature) {
    if (!feature || !feature.geometry) return null;
    var geometry = feature.geometry;
    try {
      if (geometry.type === "LineString") {
        if ((geometry.coordinates || []).length < 2) return null;
        return {
          type: "distance",
          value: turf.length(feature, { units: "kilometers" }),
        };
      }
      if (geometry.type === "Polygon") {
        var props = feature.properties || {};
        if (props.mode === "circle" && props.radiusKilometers != null) {
          var radius = props.radiusKilometers;
          return {
            type: "radius",
            value: radius,
            area: Math.PI * radius * radius,
          };
        }
        var area = turf.area(feature) / 1000000;
        var perimeter = turf.length(turf.polygonToLine(feature), {
          units: "kilometers",
        });
        return { type: "area", value: area, perimeter: perimeter };
      }
    } catch (e) {
      return null;
    }
    return null;
  };

  MapglTerraDrawControl.prototype._showMeasurementsFor = function (
    id,
    autoHide,
  ) {
    if (!this._measurementsEnabled() || id == null) return;
    var helpers = this._config.helpers;
    var feature = null;
    try {
      feature = this._draw && this._draw.getSnapshotFeature(id);
    } catch (e) {
      return;
    }
    if (!feature || isGuidanceFeature(feature)) return;
    var measurements = this._measureFeature(feature);
    if (!measurements) return;
    var box = this._ensureMeasurementBox();
    var formatted = helpers.formatMeasurements(
      measurements,
      this._config.measurement_units || "both",
    );
    if (!formatted.primary) return;
    var primary = box.querySelector("#measurement-primary");
    var secondary = box.querySelector("#measurement-secondary");
    if (primary) primary.textContent = formatted.primary;
    if (secondary) secondary.textContent = formatted.secondary;
    box.style.display = "block";
    clearTimeout(this._measureHideTimer);
    if (autoHide) {
      var self = this;
      this._measureHideTimer = setTimeout(function () {
        // keep showing while a measured feature stays selected
        if (!self._selectedIds.length) self._hideMeasurements();
      }, 3000);
    }
  };

  MapglTerraDrawControl.prototype._hideMeasurements = function () {
    clearTimeout(this._measureHideTimer);
    if (this._measurementBox) {
      this._measurementBox.style.display = "none";
    }
  };

  // ---------------------------------------------------------------------
  // Attribute editor bridge (helpers injected by standalone bindings; the
  // editor consumes the facade's get()/setFeatureProperty() plus the
  // synthetic draw.* events, so it is reused verbatim)

  MapglTerraDrawControl.prototype._initHelpers = function () {
    var config = this._config;
    var helpers = config.helpers || {};
    if (
      !this._editor &&
      typeof helpers.initializeDrawAttributeEditor === "function" &&
      Array.isArray(config.attributes) &&
      config.attributes.length
    ) {
      var self = this;
      this._editor = helpers.initializeDrawAttributeEditor(this._map, this, {
        attributes: config.attributes,
        position: config.position,
        onChange: function (debounce) {
          self._scheduleSync(!!debounce);
        },
      });
    }
  };

  // ---------------------------------------------------------------------
  // Style survival: the adapter neither survives setStyle nor re-renders
  // after stop()/start(), so rebuild with a fresh adapter + instance.
  // Selection and undo history reset by design; an in-progress drawing is
  // discarded.

  MapglTerraDrawControl.prototype._rebuild = function () {
    if (this._rebuilding || !this._draw) return;
    this._rebuilding = true;
    this._suppressSync = true;
    var lost = 0;
    try {
      var snapshot = [];
      try {
        snapshot = (this._draw.getSnapshot() || [])
          .filter(function (feature) {
            if (isGuidanceFeature(feature)) return false;
            var props = feature.properties || {};
            if (props.currentlyDrawing) return false; // discard unfinished drawing
            return true;
          })
          .map(sanitizeFeature);
      } catch (e) {
        snapshot = [];
      }
      try {
        this._draw.stop();
      } catch (e) {}
      this._draw = null;
      this._selectedIds = [];
      this._createDraw();
      if (this._draw && snapshot.length) {
        var results = [];
        try {
          results = this._draw.addFeatures(snapshot) || [];
        } catch (e) {
          lost = snapshot.length;
        }
        var self = this;
        results.forEach(function (result, i) {
          if (!result || result.valid !== false) return;
          var feature = snapshot[i];
          var baseMode = baseModeForGeometry(feature.geometry);
          if (baseMode && feature.properties.mode !== baseMode) {
            var retry = sanitizeFeature(feature);
            retry.properties.mode = baseMode;
            var retried;
            try {
              retried = self._draw.addFeatures([retry]) || [];
            } catch (e) {
              retried = [];
            }
            if (retried[0] && retried[0].valid !== false) return;
          }
          lost++;
        });
      }
    } finally {
      this._suppressSync = false;
      this._rebuilding = false;
    }
    if (lost) {
      console.error(
        "mapgl terra-draw: " +
          lost +
          " drawn feature(s) could not be restored after the style change",
      );
    }
    this._updateTrashState();
    this._refreshButtonStates();
    this._scheduleSync(false);
  };

  window.MapglTerraDrawControl = MapglTerraDrawControl;
})();
