// Measurement functionality
function createMeasurementBox(map) {
  const box = document.createElement("div");
  box.id = `measurement-box-${map._container.id}`;
  box.className = "mapgl-measurement-box";
  box.style.cssText = `
    position: absolute;
    bottom: 45px;
    left: 10px;
    background: white;
    padding: 10px 15px;
    border-radius: 4px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
    font-size: 12px;
    line-height: 1.4;
    z-index: 1;
    display: none;
    min-width: 120px;
    max-width: 200px;
    border: 1px solid rgba(0,0,0,0.1);
  `;

  box.innerHTML = `
    <div style="font-weight: 600; margin-bottom: 5px; color: #333; font-size: 11px; text-transform: uppercase;">
      Measurement
    </div>
    <div class="measurement-content">
      <div id="measurement-primary" style="font-size: 14px; font-weight: 500; color: #000; margin-bottom: 2px;"></div>
      <div id="measurement-secondary" style="font-size: 11px; color: #666;"></div>
    </div>
  `;

  map.getContainer().appendChild(box);
  return box;
}

function calculateDrawingMeasurements(mode, state, coords) {
  try {
    if (mode === "draw_line_string" && coords && coords.length >= 2) {
      const line = turf.lineString(coords);
      const distance = turf.length(line, { units: "kilometers" });
      return { type: "distance", value: distance };
    } else if (
      (mode === "draw_polygon" || mode === "draw_freehand") &&
      coords &&
      coords.length >= 3
    ) {
      // Ensure polygon is closed by adding first point at end if needed
      const closedCoords = [...coords];
      if (
        closedCoords[0][0] !== closedCoords[closedCoords.length - 1][0] ||
        closedCoords[0][1] !== closedCoords[closedCoords.length - 1][1]
      ) {
        closedCoords.push(closedCoords[0]);
      }

      try {
        const polygon = turf.polygon([closedCoords]);
        const area = turf.area(polygon) / 1000000; // Convert to km²
        const perimeter = turf.length(turf.polygonToLine(polygon), {
          units: "kilometers",
        });
        return { type: "area", value: area, perimeter: perimeter };
      } catch (error) {
        return null;
      }
    } else if (mode === "draw_rectangle" && coords && coords.length >= 4) {
      const polygon = turf.polygon([coords]);
      const area = turf.area(polygon) / 1000000; // Convert to km²
      const perimeter = turf.length(turf.polygonToLine(polygon), {
        units: "kilometers",
      });
      return { type: "area", value: area, perimeter: perimeter };
    } else if (mode === "draw_radius" && coords && coords.length >= 2) {
      const center = turf.point(coords[0]);
      const edge = turf.point(coords[1]);
      const radius = turf.distance(center, edge, { units: "kilometers" });
      const area = Math.PI * radius * radius; // πr²
      return { type: "radius", value: radius, area: area };
    }
  } catch (e) {
    return null;
  }

  return null;
}

function formatMeasurements(measurements, units) {
  if (!measurements) return { primary: "", secondary: "" };

  const formatDistance = function (km) {
    let result = [];

    if (units === "metric" || units === "both") {
      if (km < 1) {
        result.push(`${(km * 1000).toFixed(0)} m`);
      } else {
        result.push(`${km.toFixed(2)} km`);
      }
    }

    if (units === "imperial" || units === "both") {
      const miles = km * 0.621371;
      if (miles < 0.1) {
        result.push(`${(miles * 5280).toFixed(0)} ft`);
      } else {
        result.push(`${miles.toFixed(2)} mi`);
      }
    }

    return result;
  };

  const formatArea = function (sqKm) {
    let result = [];

    if (units === "metric" || units === "both") {
      if (sqKm < 0.01) {
        result.push(`${(sqKm * 1000000).toFixed(0)} m²`);
      } else if (sqKm < 1) {
        result.push(`${(sqKm * 100).toFixed(2)} ha`);
      } else {
        result.push(`${sqKm.toFixed(2)} km²`);
      }
    }

    if (units === "imperial" || units === "both") {
      const sqMiles = sqKm * 0.386102;
      if (sqMiles < 0.001) {
        result.push(`${(sqMiles * 640).toFixed(2)} acres`);
      } else {
        result.push(`${sqMiles.toFixed(3)} mi²`);
      }
    }

    return result;
  };

  if (measurements.type === "distance") {
    const formatted = formatDistance(measurements.value);
    return {
      primary: formatted[0] || "",
      secondary: formatted[1] || "",
    };
  } else if (measurements.type === "area") {
    const areaFormatted = formatArea(measurements.value);
    const perimeterFormatted = formatDistance(measurements.perimeter);

    if (units === "both") {
      return {
        primary: areaFormatted[0] || "",
        secondary: `${areaFormatted[1] || ""} • ${perimeterFormatted[0] || ""}`,
      };
    } else {
      return {
        primary: areaFormatted[0] || "",
        secondary: `Perimeter: ${perimeterFormatted[0] || ""}`,
      };
    }
  } else if (measurements.type === "radius") {
    const distFormatted = formatDistance(measurements.value);
    const areaFormatted = formatArea(measurements.area);

    return {
      primary: `Radius: ${distFormatted[0] || ""}`,
      secondary:
        units === "both"
          ? `${distFormatted[1] || ""} • ${areaFormatted[0] || ""}`
          : `Area: ${areaFormatted[0] || ""}`,
    };
  }

  return { primary: "", secondary: "" };
}

function updateMeasurementDisplay(box, measurements, units) {
  const primary = box.querySelector("#measurement-primary");
  const secondary = box.querySelector("#measurement-secondary");

  const formatted = formatMeasurements(measurements, units);

  if (formatted.primary) {
    primary.textContent = formatted.primary;
    secondary.textContent = formatted.secondary;
    box.style.display = "block";
  } else {
    box.style.display = "none";
  }
}

function initializeMeasurements(map, draw, units) {
  const measurementBox = createMeasurementBox(map);
  const DRAWING_MODES = [
    "draw_line_string",
    "draw_polygon",
    "draw_rectangle",
    "draw_radius",
    "draw_freehand",
  ];

  // Store original handlers
  const originalHandlers = {};

  DRAWING_MODES.forEach((mode) => {
    const modeObj = MapboxDraw.modes[mode];
    if (!modeObj) return;

    // Wrap onClick for polygon mode (better for click-based drawing)
    if (modeObj.onClick && mode === "draw_polygon") {
      originalHandlers[mode + "_onClick"] = modeObj.onClick;
      modeObj.onClick = function (state, e) {
        const result = originalHandlers[mode + "_onClick"].call(this, state, e);

        // For polygon mode, show measurements after each click
        if (
          state.polygon &&
          state.polygon.coordinates &&
          state.polygon.coordinates[0].length >= 3
        ) {
          const coords = state.polygon.coordinates[0];
          const measurements = calculateDrawingMeasurements(
            mode,
            state,
            coords,
          );
          updateMeasurementDisplay(measurementBox, measurements, units);
        }

        return result;
      };
    }

    // Wrap onMouseMove for real-time updates (lines, rectangles, radius)
    if (modeObj.onMouseMove && mode !== "draw_polygon") {
      originalHandlers[mode + "_onMouseMove"] = modeObj.onMouseMove;
      modeObj.onMouseMove = function (state, e) {
        originalHandlers[mode + "_onMouseMove"].call(this, state, e);

        let coords = null;
        if (state.line && state.line.coordinates) {
          coords = [...state.line.coordinates, [e.lngLat.lng, e.lngLat.lat]];
        } else if (state.rectangle && state.rectangle.coordinates) {
          coords = state.rectangle.coordinates[0];
        }

        if (coords) {
          const measurements = calculateDrawingMeasurements(
            mode,
            state,
            coords,
          );
          updateMeasurementDisplay(measurementBox, measurements, units);
        }
      };
    }

    // Wrap onDrag for freehand mode
    if (modeObj.onDrag) {
      originalHandlers[mode + "_onDrag"] = modeObj.onDrag;
      modeObj.onDrag = function (state, e) {
        originalHandlers[mode + "_onDrag"].call(this, state, e);

        if (state.polygon && state.polygon.coordinates) {
          const coords = state.polygon.coordinates[0];
          if (coords.length >= 3) {
            const measurements = calculateDrawingMeasurements(
              mode,
              state,
              coords,
            );
            updateMeasurementDisplay(measurementBox, measurements, units);
          }
        }
      };
    }
  });

  // Hide measurement box when drawing stops and handle button states
  map.on("draw.modechange", (e) => {
    if (e.mode === "simple_select") {
      measurementBox.style.display = "none";
      // Reset button states when switching to select mode
      const drawControlGroup = map
        .getContainer()
        .querySelector(".mapboxgl-ctrl-group");
      if (drawControlGroup) {
        drawControlGroup
          .querySelectorAll("button")
          .forEach((btn) => btn.classList.remove("active"));
      }
    }
  });

  map.on("draw.create", () => {
    measurementBox.style.display = "none";
    // Reset button states when drawing is completed
    const drawControlGroup = map
      .getContainer()
      .querySelector(".mapboxgl-ctrl-group");
    if (drawControlGroup) {
      drawControlGroup
        .querySelectorAll("button")
        .forEach((btn) => btn.classList.remove("active"));
    }
  });

  map.on("draw.delete", () => {
    measurementBox.style.display = "none";
  });

  // Special handling for freehand mode using data update events
  map.on("draw.update", (e) => {
    const currentMode = draw.getMode();
    if (currentMode === "draw_freehand" && e.features && e.features[0]) {
      const feature = e.features[0];
      if (
        feature.geometry.type === "Polygon" &&
        feature.geometry.coordinates[0].length >= 3
      ) {
        const coords = feature.geometry.coordinates[0];
        const measurements = calculateDrawingMeasurements(
          "draw_freehand",
          {},
          coords,
        );
        updateMeasurementDisplay(measurementBox, measurements, units);
      }
    }

    // Also handle editing mode - when features are being edited
    if (currentMode === "direct_select" && e.features && e.features[0]) {
      const feature = e.features[0];
      if (
        feature.geometry.type === "Polygon" &&
        feature.geometry.coordinates[0].length >= 3
      ) {
        const coords = feature.geometry.coordinates[0];
        const measurements = calculateDrawingMeasurements(
          "draw_polygon",
          {},
          coords,
        );
        updateMeasurementDisplay(measurementBox, measurements, units);
      } else if (
        feature.geometry.type === "LineString" &&
        feature.geometry.coordinates.length >= 2
      ) {
        const coords = feature.geometry.coordinates;
        const measurements = calculateDrawingMeasurements(
          "draw_line_string",
          {},
          coords,
        );
        updateMeasurementDisplay(measurementBox, measurements, units);
      }
    }
  });

  // Show measurements when selecting features for editing
  map.on("draw.selectionchange", (e) => {
    if (e.features && e.features.length > 0) {
      const feature = e.features[0];
      if (
        feature.geometry.type === "Polygon" &&
        feature.geometry.coordinates[0].length >= 3
      ) {
        const coords = feature.geometry.coordinates[0];
        const measurements = calculateDrawingMeasurements(
          "draw_polygon",
          {},
          coords,
        );
        updateMeasurementDisplay(measurementBox, measurements, units);
      } else if (
        feature.geometry.type === "LineString" &&
        feature.geometry.coordinates.length >= 2
      ) {
        const coords = feature.geometry.coordinates;
        const measurements = calculateDrawingMeasurements(
          "draw_line_string",
          {},
          coords,
        );
        updateMeasurementDisplay(measurementBox, measurements, units);
      }
    } else {
      // No features selected, hide measurement box
      measurementBox.style.display = "none";
    }
  });
}

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
        .map((item) => evaluateExpression(item, properties))
        .join("");
    case "to-string":
      return String(evaluateExpression(expression[1], properties));
    case "to-number":
      return Number(evaluateExpression(expression[1], properties));
    case "number-format":
      const value = evaluateExpression(expression[1], properties);
      const options = expression[2] || {};

      // Handle locale option
      const locale = options.locale || "en-US";

      // Build Intl.NumberFormat options
      const formatOptions = {};

      // Style options
      if (options.style) formatOptions.style = options.style; // 'decimal', 'currency', 'percent', 'unit'
      if (options.currency) formatOptions.currency = options.currency;
      if (options.unit) formatOptions.unit = options.unit;

      // Digit options
      if (options.hasOwnProperty("min-fraction-digits")) {
        formatOptions.minimumFractionDigits = options["min-fraction-digits"];
      }
      if (options.hasOwnProperty("max-fraction-digits")) {
        formatOptions.maximumFractionDigits = options["max-fraction-digits"];
      }
      if (options.hasOwnProperty("min-integer-digits")) {
        formatOptions.minimumIntegerDigits = options["min-integer-digits"];
      }

      // Notation options
      if (options.notation) formatOptions.notation = options.notation; // 'standard', 'scientific', 'engineering', 'compact'
      if (options.compactDisplay)
        formatOptions.compactDisplay = options.compactDisplay; // 'short', 'long'

      // Grouping
      if (options.hasOwnProperty("useGrouping")) {
        formatOptions.useGrouping = options.useGrouping;
      }

      return new Intl.NumberFormat(locale, formatOptions).format(value);
    default:
      // For literals and other simple values
      return expression;
  }
}

function onMouseMoveTooltip(e, map, tooltipPopup, tooltipProperty) {
  map.getCanvas().style.cursor = "pointer";
  if (e.features.length > 0) {
    // Clear any existing active tooltip first to prevent stacking
    if (window._activeTooltip && window._activeTooltip !== tooltipPopup) {
      window._activeTooltip.remove();
    }

    let description;

    // Check if tooltipProperty is an expression (array) or a simple property name (string)
    if (Array.isArray(tooltipProperty)) {
      // It's an expression, evaluate it
      description = evaluateExpression(
        tooltipProperty,
        e.features[0].properties,
      );
    } else {
      // It's a property name, get the value
      description = e.features[0].properties[tooltipProperty];
    }

    tooltipPopup.setLngLat(e.lngLat).setHTML(description).addTo(map);

    // Store reference to currently active tooltip
    window._activeTooltip = tooltipPopup;
  } else {
    tooltipPopup.remove();
    // If this was the active tooltip, clear the reference
    if (window._activeTooltip === tooltipPopup) {
      delete window._activeTooltip;
    }
  }
}

function onMouseLeaveTooltip(map, tooltipPopup) {
  map.getCanvas().style.cursor = "";
  tooltipPopup.remove();
  if (window._activeTooltip === tooltipPopup) {
    delete window._activeTooltip;
  }
}

function onClickPopup(e, map, popupProperty, layerId) {
  let description;

  // Check if popupProperty is an expression (array) or a simple property name (string)
  if (Array.isArray(popupProperty)) {
    // It's an expression, evaluate it
    description = evaluateExpression(popupProperty, e.features[0].properties);
  } else {
    // It's a property name, get the value
    description = e.features[0].properties[popupProperty];
  }

  // Remove any existing popup for this layer
  if (window._mapboxPopups && window._mapboxPopups[layerId]) {
    window._mapboxPopups[layerId].remove();
  }

  // Create and show the popup
  const popup = new mapboxgl.Popup({ maxWidth: '400px' })
    .setLngLat(e.lngLat)
    .setHTML(description)
    .addTo(map);

  // Store reference to this popup
  if (!window._mapboxPopups) {
    window._mapboxPopups = {};
  }
  window._mapboxPopups[layerId] = popup;

  // Remove reference when popup is closed
  popup.on("close", function () {
    if (window._mapboxPopups[layerId] === popup) {
      delete window._mapboxPopups[layerId];
    }
  });
}

// Helper function to generate draw styles based on parameters
function generateDrawStyles(styling) {
  if (!styling) return null;

  return [
    // Point styles
    {
      id: "gl-draw-point-active",
      type: "circle",
      filter: [
        "all",
        ["==", "$type", "Point"],
        ["==", "meta", "feature"],
        ["==", "active", "true"],
      ],
      paint: {
        "circle-radius": styling.vertex_radius + 2,
        "circle-color": styling.active_color,
      },
    },
    {
      id: "gl-draw-point",
      type: "circle",
      filter: [
        "all",
        ["==", "$type", "Point"],
        ["==", "meta", "feature"],
        ["==", "active", "false"],
      ],
      paint: {
        "circle-radius": styling.vertex_radius,
        "circle-color": styling.point_color,
      },
    },
    // Line styles
    {
      id: "gl-draw-line",
      type: "line",
      filter: ["all", ["==", "$type", "LineString"]],
      layout: {
        "line-cap": "round",
        "line-join": "round",
      },
      paint: {
        "line-color": [
          "case",
          ["==", ["get", "active"], "true"],
          styling.active_color,
          styling.line_color,
        ],
        "line-width": styling.line_width,
      },
    },
    // Polygon fill
    {
      id: "gl-draw-polygon-fill",
      type: "fill",
      filter: ["all", ["==", "$type", "Polygon"]],
      paint: {
        "fill-color": [
          "case",
          ["==", ["get", "active"], "true"],
          styling.active_color,
          styling.fill_color,
        ],
        "fill-outline-color": [
          "case",
          ["==", ["get", "active"], "true"],
          styling.active_color,
          styling.fill_color,
        ],
        "fill-opacity": styling.fill_opacity,
      },
    },
    // Polygon outline
    {
      id: "gl-draw-polygon-stroke",
      type: "line",
      filter: ["all", ["==", "$type", "Polygon"]],
      layout: {
        "line-cap": "round",
        "line-join": "round",
      },
      paint: {
        "line-color": [
          "case",
          ["==", ["get", "active"], "true"],
          styling.active_color,
          styling.line_color,
        ],
        "line-width": styling.line_width,
      },
    },
    // Midpoints
    {
      id: "gl-draw-polygon-midpoint",
      type: "circle",
      filter: ["all", ["==", "$type", "Point"], ["==", "meta", "midpoint"]],
      paint: {
        "circle-radius": 3,
        "circle-color": styling.active_color,
      },
    },
    // Vertex point halos
    {
      id: "gl-draw-vertex-halo-active",
      type: "circle",
      filter: ["all", ["==", "meta", "vertex"], ["==", "$type", "Point"]],
      paint: {
        "circle-radius": [
          "case",
          ["==", ["get", "active"], "true"],
          styling.vertex_radius + 4,
          styling.vertex_radius + 2,
        ],
        "circle-color": "#FFF",
      },
    },
    // Vertex points
    {
      id: "gl-draw-vertex-active",
      type: "circle",
      filter: ["all", ["==", "meta", "vertex"], ["==", "$type", "Point"]],
      paint: {
        "circle-radius": [
          "case",
          ["==", ["get", "active"], "true"],
          styling.vertex_radius + 2,
          styling.vertex_radius,
        ],
        "circle-color": styling.active_color,
      },
    },
  ];
}

// Helper function to add features from a source to draw
function addSourceFeaturesToDraw(draw, sourceId, map) {
  const source = map.getSource(sourceId);
  if (source && source._data) {
    draw.add(source._data);
  } else {
    console.warn("Source not found or has no data:", sourceId);
  }
}

HTMLWidgets.widget({
  name: "mapboxgl",

  type: "output",

  factory: function (el, width, height) {
    let map;
    let draw;

    return {
      renderValue: function (x) {
        if (typeof mapboxgl === "undefined") {
          console.error("Mapbox GL JS is not loaded.");
          return;
        }

        // Register PMTiles source type if available
        if (
          typeof MapboxPmTilesSource !== "undefined" &&
          typeof pmtiles !== "undefined"
        ) {
          try {
            mapboxgl.Style.setSourceType(
              PMTILES_SOURCE_TYPE,
              MapboxPmTilesSource,
            );
            console.log("PMTiles support enabled for Mapbox GL JS");
          } catch (e) {
            console.warn("Failed to register PMTiles source type:", e);
          }
        }

        mapboxgl.accessToken = x.access_token;

        map = new mapboxgl.Map({
          container: el.id,
          style: x.style,
          center: x.center,
          zoom: x.zoom,
          bearing: x.bearing,
          pitch: x.pitch,
          projection: x.projection,
          parallels: x.parallels,
          ...x.additional_params,
        });

        map.controls = [];

        map.on("style.load", function () {
          map.resize();

          if (HTMLWidgets.shinyMode) {
            map.on("load", function () {
              var bounds = map.getBounds();
              var center = map.getCenter();
              var zoom = map.getZoom();

              Shiny.setInputValue(el.id + "_zoom", zoom);
              Shiny.setInputValue(el.id + "_center", {
                lng: center.lng,
                lat: center.lat,
              });
              Shiny.setInputValue(el.id + "_bbox", {
                xmin: bounds.getWest(),
                ymin: bounds.getSouth(),
                xmax: bounds.getEast(),
                ymax: bounds.getNorth(),
              });
            });

            map.on("moveend", function (e) {
              var map = e.target;
              var bounds = map.getBounds();
              var center = map.getCenter();
              var zoom = map.getZoom();

              Shiny.onInputChange(el.id + "_zoom", zoom);
              Shiny.onInputChange(el.id + "_center", {
                lng: center.lng,
                lat: center.lat,
              });
              Shiny.onInputChange(el.id + "_bbox", {
                xmin: bounds.getWest(),
                ymin: bounds.getSouth(),
                xmax: bounds.getEast(),
                ymax: bounds.getNorth(),
              });
            });
          }

          // Set config properties if provided
          if (x.config_properties) {
            x.config_properties.forEach(function (config) {
              map.setConfigProperty(
                config.importId,
                config.configName,
                config.value,
              );
            });
          }

          if (x.markers) {
            if (!window.mapboxglMarkers) {
              window.mapboxglMarkers = [];
            }
            x.markers.forEach(function (marker) {
              const markerOptions = {
                color: marker.color,
                rotation: marker.rotation,
                draggable: marker.options.draggable || false,
                ...marker.options,
              };
              const mapMarker = new mapboxgl.Marker(markerOptions)
                .setLngLat([marker.lng, marker.lat])
                .addTo(map);

              if (marker.popup) {
                mapMarker.setPopup(
                  new mapboxgl.Popup({ offset: 25, maxWidth: '400px' }).setHTML(marker.popup),
                );
              }

              if (HTMLWidgets.shinyMode) {
                const markerId = marker.id;
                if (markerId) {
                  const lngLat = mapMarker.getLngLat();
                  Shiny.setInputValue(el.id + "_marker_" + markerId, {
                    id: markerId,
                    lng: lngLat.lng,
                    lat: lngLat.lat,
                  });

                  mapMarker.on("dragend", function () {
                    const lngLat = mapMarker.getLngLat();
                    Shiny.setInputValue(el.id + "_marker_" + markerId, {
                      id: markerId,
                      lng: lngLat.lng,
                      lat: lngLat.lat,
                    });
                  });
                }
              }

              window.mapboxglMarkers.push(mapMarker);
            });
          }

          // Add sources if provided
          if (x.sources) {
            x.sources.forEach(function (source) {
              if (source.type === "vector") {
                const sourceOptions = {
                  type: "vector",
                };
                // Add url or tiles
                if (source.url) {
                  sourceOptions.url = source.url;
                }
                if (source.tiles) {
                  sourceOptions.tiles = source.tiles;
                }
                // Add promoteId if provided
                if (source.promoteId) {
                  sourceOptions.promoteId = source.promoteId;
                }
                // Add any other additional options
                for (const [key, value] of Object.entries(source)) {
                  if (!["id", "type", "url", "tiles", "promoteId"].includes(key)) {
                    sourceOptions[key] = value;
                  }
                }
                map.addSource(source.id, sourceOptions);
              } else if (source.type === "geojson") {
                const geojsonData = source.data;
                const sourceOptions = {
                  type: "geojson",
                  data: geojsonData,
                  generateId: source.generateId,
                };

                // Add additional options
                for (const [key, value] of Object.entries(source)) {
                  if (!["id", "type", "data", "generateId"].includes(key)) {
                    sourceOptions[key] = value;
                  }
                }

                map.addSource(source.id, sourceOptions);
              } else if (source.type === "raster") {
                if (source.url) {
                  map.addSource(source.id, {
                    type: "raster",
                    url: source.url,
                    tileSize: source.tileSize,
                    maxzoom: source.maxzoom,
                  });
                } else if (source.tiles) {
                  map.addSource(source.id, {
                    type: "raster",
                    tiles: source.tiles,
                    tileSize: source.tileSize,
                    maxzoom: source.maxzoom,
                  });
                }
              } else if (source.type === "raster-dem") {
                map.addSource(source.id, {
                  type: "raster-dem",
                  url: source.url,
                  tileSize: source.tileSize,
                  maxzoom: source.maxzoom,
                });
              } else if (source.type === "image") {
                map.addSource(source.id, {
                  type: "image",
                  url: source.url,
                  coordinates: source.coordinates,
                });
              } else if (source.type === "video") {
                map.addSource(source.id, {
                  type: "video",
                  urls: source.urls,
                  coordinates: source.coordinates,
                });
              } else {
                // Handle custom source types (like pmtile-source)
                const sourceOptions = { type: source.type };

                // Copy all properties except id
                for (const [key, value] of Object.entries(source)) {
                  if (key !== "id") {
                    sourceOptions[key] = value;
                  }
                }

                map.addSource(source.id, sourceOptions);
              }
            });
          }

          // Add layers if provided
          if (x.layers) {
            x.layers.forEach(function (layer) {
              try {
                const layerConfig = {
                  id: layer.id,
                  type: layer.type,
                  source: layer.source,
                  layout: layer.layout || {},
                  paint: layer.paint || {},
                };

                // Check if source is an object and set generateId if source type is 'geojson'
                if (
                  typeof layer.source === "object" &&
                  layer.source.type === "geojson"
                ) {
                  layerConfig.source.generateId = true;
                } else if (typeof layer.source === "string") {
                  // Handle string source if needed
                  layerConfig.source = layer.source;
                }

                if (layer.source_layer) {
                  layerConfig["source-layer"] = layer.source_layer;
                }

                if (layer.slot) {
                  layerConfig["slot"] = layer.slot;
                }

                if (layer.minzoom) {
                  layerConfig["minzoom"] = layer.minzoom;
                }
                if (layer.maxzoom) {
                  layerConfig["maxzoom"] = layer.maxzoom;
                }

                if (layer.filter) {
                  layerConfig["filter"] = layer.filter;
                }

                if (layer.before_id) {
                  map.addLayer(layerConfig, layer.before_id);
                } else {
                  map.addLayer(layerConfig);
                }

                // Add popups or tooltips if provided
                if (layer.popup) {
                  // Initialize popup tracking if it doesn't exist
                  if (!window._mapboxPopups) {
                    window._mapboxPopups = {};
                  }

                  // Create click handler for this layer
                  const clickHandler = function (e) {
                    onClickPopup(e, map, layer.popup, layer.id);
                  };

                  // Store these handler references so we can remove them later if needed
                  if (!window._mapboxClickHandlers) {
                    window._mapboxClickHandlers = {};
                  }
                  window._mapboxClickHandlers[layer.id] = clickHandler;

                  // Add the click handler
                  map.on("click", layer.id, clickHandler);

                  // Change cursor to pointer when hovering over the layer
                  map.on("mouseenter", layer.id, function () {
                    map.getCanvas().style.cursor = "pointer";
                  });

                  // Change cursor back to default when leaving the layer
                  map.on("mouseleave", layer.id, function () {
                    map.getCanvas().style.cursor = "";
                  });
                }

                if (layer.tooltip) {
                  const tooltip = new mapboxgl.Popup({
                    closeButton: false,
                    closeOnClick: false,
                    maxWidth: '400px',
                  });

                  // Create a reference to the mousemove handler function.
                  // We need to pass 'e', 'map', 'tooltip', and 'layer.tooltip' to onMouseMoveTooltip.
                  const mouseMoveHandler = function (e) {
                    onMouseMoveTooltip(e, map, tooltip, layer.tooltip);
                  };

                  // Create a reference to the mouseleave handler function.
                  // We need to pass 'map' and 'tooltip' to onMouseLeaveTooltip.
                  const mouseLeaveHandler = function () {
                    onMouseLeaveTooltip(map, tooltip);
                  };

                  // Attach the named handler references, not anonymous functions.
                  map.on("mousemove", layer.id, mouseMoveHandler);
                  map.on("mouseleave", layer.id, mouseLeaveHandler);

                  // Store these handler references so you can remove them later if needed
                  if (!window._mapboxHandlers) {
                    window._mapboxHandlers = {};
                  }
                  window._mapboxHandlers[layer.id] = {
                    mousemove: mouseMoveHandler,
                    mouseleave: mouseLeaveHandler,
                  };
                }

                // Add hover effect if provided
                if (layer.hover_options) {
                  const jsHoverOptions = {};
                  for (const [key, value] of Object.entries(
                    layer.hover_options,
                  )) {
                    const jsKey = key.replace(/_/g, "-");
                    jsHoverOptions[jsKey] = value;
                  }

                  let hoveredFeatureId = null;

                  map.on("mousemove", layer.id, function (e) {
                    if (e.features.length > 0) {
                      if (hoveredFeatureId !== null) {
                        const featureState = {
                          source:
                            typeof layer.source === "string"
                              ? layer.source
                              : layer.id,
                          id: hoveredFeatureId,
                        };
                        if (layer.source_layer) {
                          featureState.sourceLayer = layer.source_layer;
                        }
                        map.setFeatureState(featureState, { hover: false });
                      }
                      hoveredFeatureId = e.features[0].id;
                      const featureState = {
                        source:
                          typeof layer.source === "string"
                            ? layer.source
                            : layer.id,
                        id: hoveredFeatureId,
                      };
                      if (layer.source_layer) {
                        featureState.sourceLayer = layer.source_layer;
                      }
                      map.setFeatureState(featureState, {
                        hover: true,
                      });
                    }
                  });

                  map.on("mouseleave", layer.id, function () {
                    if (hoveredFeatureId !== null) {
                      const featureState = {
                        source:
                          typeof layer.source === "string"
                            ? layer.source
                            : layer.id,
                        id: hoveredFeatureId,
                      };
                      if (layer.source_layer) {
                        featureState.sourceLayer = layer.source_layer;
                      }
                      map.setFeatureState(featureState, {
                        hover: false,
                      });
                    }
                    hoveredFeatureId = null;
                  });

                  Object.keys(jsHoverOptions).forEach(function (key) {
                    const originalPaint =
                      map.getPaintProperty(layer.id, key) || layer.paint[key];
                    map.setPaintProperty(layer.id, key, [
                      "case",
                      ["boolean", ["feature-state", "hover"], false],
                      jsHoverOptions[key],
                      originalPaint,
                    ]);
                  });
                }
              } catch (e) {
                console.error("Failed to add layer: ", layer, e);
              }
            });
          }

          // Apply setFilter if provided
          if (x.setFilter) {
            x.setFilter.forEach(function (filter) {
              map.setFilter(filter.layer, filter.filter);
            });
          }

          // Apply moveLayer operations if provided
          if (x.moveLayer) {
            x.moveLayer.forEach(function (moveOp) {
              if (map.getLayer(moveOp.layer)) {
                if (moveOp.before) {
                  map.moveLayer(moveOp.layer, moveOp.before);
                } else {
                  map.moveLayer(moveOp.layer);
                }
              }
            });
          }

          // Set terrain if provided
          if (x.terrain) {
            map.setTerrain({
              source: x.terrain.source,
              exaggeration: x.terrain.exaggeration,
            });
          }

          // Set fog
          if (x.fog) {
            map.setFog(x.fog);
          }

          // Set rain effect if provided
          if (x.rain) {
            map.setRain(x.rain);
          }

          // Set snow effect if provided
          if (x.snow) {
            map.setSnow(x.snow);
          }

          if (x.fitBounds) {
            map.fitBounds(x.fitBounds.bounds, x.fitBounds.options);
          }
          if (x.flyTo) {
            map.flyTo(x.flyTo);
          }
          if (x.easeTo) {
            map.easeTo(x.easeTo);
          }
          if (x.setCenter) {
            map.setCenter(x.setCenter);
          }
          if (x.setZoom) {
            map.setZoom(x.setZoom);
          }
          if (x.jumpTo) {
            map.jumpTo(x.jumpTo);
          }

          // Add scale control if enabled
          if (x.scale_control) {
            const scaleControl = new mapboxgl.ScaleControl({
              maxWidth: x.scale_control.maxWidth,
              unit: x.scale_control.unit,
            });
            map.addControl(scaleControl, x.scale_control.position);
            map.controls.push(scaleControl);
          }

          // Add globe minimap if enabled
          if (x.globe_minimap && x.globe_minimap.enabled) {
            const globeMinimapOptions = {
              globeSize: x.globe_minimap.globe_size,
              landColor: x.globe_minimap.land_color,
              waterColor: x.globe_minimap.water_color,
              markerColor: x.globe_minimap.marker_color,
              markerSize: x.globe_minimap.marker_size,
            };
            const globeMinimap = new GlobeMinimap(globeMinimapOptions);
            map.addControl(globeMinimap, x.globe_minimap.position);
            map.controls.push(globeMinimap);
          }

          // Add custom controls if any are defined
          if (x.custom_controls) {
            Object.keys(x.custom_controls).forEach(function (key) {
              const controlOptions = x.custom_controls[key];
              const customControlContainer = document.createElement("div");

              if (controlOptions.className) {
                customControlContainer.className = controlOptions.className;
              } else {
                customControlContainer.className =
                  "mapboxgl-ctrl mapboxgl-ctrl-group";
              }

              customControlContainer.innerHTML = controlOptions.html;

              const customControl = {
                onAdd: function () {
                  return customControlContainer;
                },
                onRemove: function () {
                  if (customControlContainer.parentNode) {
                    customControlContainer.parentNode.removeChild(
                      customControlContainer,
                    );
                  }
                },
              };

              map.addControl(
                customControl,
                controlOptions.position || "top-right",
              );
              map.controls.push(customControl);
            });
          }

          // Add geocoder control if enabled
          if (x.geocoder_control) {
            const geocoderOptions = {
              accessToken: mapboxgl.accessToken,
              mapboxgl: mapboxgl,
              ...x.geocoder_control,
            };

            // Set default values if not provided
            if (!geocoderOptions.placeholder)
              geocoderOptions.placeholder = "Search";
            if (typeof geocoderOptions.collapsed === "undefined")
              geocoderOptions.collapsed = false;

            const geocoder = new MapboxGeocoder(geocoderOptions);

            map.addControl(
              geocoder,
              x.geocoder_control.position || "top-right",
            );
            map.controls.push(geocoder);

            // Handle geocoder results in Shiny mode
            if (HTMLWidgets.shinyMode) {
              geocoder.on("result", function (e) {
                Shiny.setInputValue(el.id + "_geocoder", {
                  result: e.result,
                  time: new Date(),
                });
              });
            }
          }

          if (x.draw_control && x.draw_control.enabled) {
            let drawOptions = x.draw_control.options || {};

            // Generate styles if styling parameters provided
            if (x.draw_control.styling) {
              const generatedStyles = generateDrawStyles(
                x.draw_control.styling,
              );
              if (generatedStyles) {
                drawOptions.styles = generatedStyles;
              }
            }

            if (x.draw_control.freehand) {
              drawOptions = Object.assign({}, drawOptions, {
                modes: Object.assign({}, MapboxDraw.modes, {
                  draw_polygon: Object.assign(
                    {},
                    MapboxDraw.modes.draw_freehand,
                    {
                      // Store the simplify_freehand option on the map object
                      onSetup: function (opts) {
                        const state =
                          MapboxDraw.modes.draw_freehand.onSetup.call(
                            this,
                            opts,
                          );
                        this.map.simplify_freehand =
                          x.draw_control.simplify_freehand;
                        return state;
                      },
                    },
                  ),
                }),
                // defaultMode: 'draw_polygon' # Don't set the default yet
              });
            }

            // Add rectangle mode if enabled
            if (x.draw_control.rectangle) {
              if (!drawOptions.modes) {
                drawOptions.modes = Object.assign({}, MapboxDraw.modes);
              }
              drawOptions.modes.draw_rectangle =
                MapboxDraw.modes.draw_rectangle;
            }

            // Add radius mode if enabled
            if (x.draw_control.radius) {
              if (!drawOptions.modes) {
                drawOptions.modes = Object.assign({}, MapboxDraw.modes);
              }
              drawOptions.modes.draw_radius = MapboxDraw.modes.draw_radius;
            }

            draw = new MapboxDraw(drawOptions);
            map.addControl(draw, x.draw_control.position);
            map.controls.push(draw);

            // Add lasso icon CSS for freehand mode
            if (x.draw_control.freehand) {
              if (!document.querySelector("#mapgl-freehand-lasso-styles")) {
                const style = document.createElement("style");
                style.id = "mapgl-freehand-lasso-styles";
                style.textContent = `
                  .mapbox-gl-draw_polygon {
                    background-image: url('data:image/svg+xml;utf8,%3Csvg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 20 20"%3E%3Cpath d="M 7 3 C 5 2.5, 3.5 3, 3 5 C 2.5 7, 3 8.5, 4 9.5 C 5 10.5, 5.5 11.5, 5 13 C 4.5 14.5, 5 15.5, 6.5 16 C 8 16.5, 9.5 16, 11 15 C 12.5 14, 13.5 13, 14.5 11.5 C 15.5 10, 16 8.5, 15.5 7 C 15 5.5, 14 4.5, 12.5 3.5 C 11 2.5, 9 2.5, 7 3 Z" fill="none" stroke="%23000000" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/%3E%3C/svg%3E') !important;
                  }
                `;
                document.head.appendChild(style);
              }

              // Update tooltip for freehand mode
              setTimeout(() => {
                const polygonButton = map
                  .getContainer()
                  .querySelector(".mapbox-gl-draw_polygon");
                if (polygonButton) {
                  polygonButton.title = "Freehand polygon tool (p)";
                }
              }, 100);
            }

            // Add rectangle icon CSS if rectangle mode is enabled
            if (x.draw_control.rectangle) {
              if (!document.querySelector("#mapgl-rectangle-styles")) {
                const style = document.createElement("style");
                style.id = "mapgl-rectangle-styles";
                style.textContent = `
                  .mapbox-gl-draw_rectangle {
                    background: transparent;
                    border: none;
                    cursor: pointer;
                    display: block;
                    height: 30px;
                    width: 30px;
                    padding: 0;
                    outline: none;
                    background-image: url('data:image/svg+xml;utf8,%3Csvg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 20 20"%3E%3Crect x="4" y="5" width="12" height="10" fill="none" stroke="%23000000" stroke-width="2"/%3E%3C/svg%3E') !important;
                    background-repeat: no-repeat !important;
                    background-position: center !important;
                  }
                  .mapbox-gl-draw_rectangle:hover {
                    background-color: rgba(0, 0, 0, 0.05);
                  }
                  .mapbox-gl-draw_rectangle.active {
                    background-color: rgba(0, 0, 0, 0.05);
                  }
                `;
                document.head.appendChild(style);
              }
            }

            // Add radius/circle icon CSS if radius mode is enabled
            if (x.draw_control.radius) {
              if (!document.querySelector("#mapgl-radius-styles")) {
                const style = document.createElement("style");
                style.id = "mapgl-radius-styles";
                style.textContent = `
                  .mapbox-gl-draw_radius {
                    background: transparent;
                    border: none;
                    cursor: pointer;
                    display: block;
                    height: 30px;
                    width: 30px;
                    padding: 0;
                    outline: none;
                    background-image: url('data:image/svg+xml;utf8,%3Csvg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 20 20"%3E%3Ccircle cx="10" cy="10" r="7" fill="none" stroke="%23000000" stroke-width="2"/%3E%3Ccircle cx="10" cy="10" r="1.5" fill="%23000000"/%3E%3C/svg%3E') !important;
                    background-repeat: no-repeat !important;
                    background-position: center !important;
                  }
                  .mapbox-gl-draw_radius:hover {
                    background-color: rgba(0, 0, 0, 0.05);
                  }
                  .mapbox-gl-draw_radius.active {
                    background-color: rgba(0, 0, 0, 0.05);
                  }
                `;
                document.head.appendChild(style);
              }
            }

            // Add event listeners
            map.on("draw.create", updateDrawnFeatures);
            map.on("draw.delete", updateDrawnFeatures);
            map.on("draw.update", updateDrawnFeatures);

            // Add measurement functionality if enabled
            if (x.draw_control.show_measurements) {
              initializeMeasurements(
                map,
                draw,
                x.draw_control.measurement_units,
              );
            }

            // Add initial features if provided
            if (x.draw_control.source) {
              addSourceFeaturesToDraw(draw, x.draw_control.source, map);
            }

            // Process any queued features
            if (x.draw_features_queue) {
              x.draw_features_queue.forEach(function (data) {
                if (data.clear_existing) {
                  draw.deleteAll();
                }
                addSourceFeaturesToDraw(draw, data.source, map);
              });
            }

            // Add custom mode buttons
            setTimeout(() => {
              const drawControlGroup = map
                .getContainer()
                .querySelector(".mapboxgl-ctrl-group");

              if (x.draw_control.rectangle && drawControlGroup) {
                const rectangleBtn = document.createElement("button");
                rectangleBtn.className = "mapbox-gl-draw_rectangle";
                rectangleBtn.title = "Rectangle tool";
                rectangleBtn.type = "button";
                rectangleBtn.onclick = function () {
                  draw.changeMode("draw_rectangle");
                  // Update active state
                  drawControlGroup
                    .querySelectorAll("button")
                    .forEach((btn) => btn.classList.remove("active"));
                  rectangleBtn.classList.add("active");
                };
                drawControlGroup.appendChild(rectangleBtn);
              }

              if (x.draw_control.radius && drawControlGroup) {
                const radiusBtn = document.createElement("button");
                radiusBtn.className = "mapbox-gl-draw_radius";
                radiusBtn.title = "Radius/Circle tool";
                radiusBtn.type = "button";
                radiusBtn.onclick = function () {
                  draw.changeMode("draw_radius");
                  // Update active state
                  drawControlGroup
                    .querySelectorAll("button")
                    .forEach((btn) => btn.classList.remove("active"));
                  radiusBtn.classList.add("active");
                };
                drawControlGroup.appendChild(radiusBtn);
              }
            }, 100);

            // Apply orientation styling
            if (x.draw_control.orientation === "horizontal") {
              const drawBar = map
                .getContainer()
                .querySelector(".mapboxgl-ctrl-group");
              if (drawBar) {
                drawBar.style.display = "flex";
                drawBar.style.flexDirection = "row";
              }
            }

            // Add download button if requested
            if (x.draw_control.download_button) {
              // Add CSS for download button if not already added
              if (!document.querySelector("#mapgl-draw-download-styles")) {
                const style = document.createElement("style");
                style.id = "mapgl-draw-download-styles";
                style.textContent = `
                  .mapbox-gl-draw_download {
                    background: transparent;
                    border: none;
                    cursor: pointer;
                    display: block;
                    height: 30px;
                    width: 30px;
                    padding: 0;
                    outline: none;
                  }
                  .mapbox-gl-draw_download:hover {
                    background-color: rgba(0, 0, 0, 0.05);
                  }
                  .mapbox-gl-draw_download svg {
                    width: 20px;
                    height: 20px;
                    margin: 5px;
                    fill: #333;
                  }
                `;
                document.head.appendChild(style);
              }

              // Small delay to ensure Draw control is fully rendered
              setTimeout(() => {
                // Find the Draw control button group
                const drawButtons = map
                  .getContainer()
                  .querySelector(
                    ".mapboxgl-ctrl-group:has(.mapbox-gl-draw_polygon)",
                  );

                if (drawButtons) {
                  // Create download button
                  const downloadBtn = document.createElement("button");
                  downloadBtn.className = "mapbox-gl-draw_download";
                  downloadBtn.title = "Download drawn features as GeoJSON";

                  // Add SVG download icon
                  downloadBtn.innerHTML = `
                    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
                      <path d="M19 9h-4V3H9v6H5l7 7 7-7zM5 18v2h14v-2H5z"/>
                    </svg>
                  `;

                  downloadBtn.addEventListener("click", () => {
                    // Get all drawn features
                    const data = draw.getAll();

                    if (data.features.length === 0) {
                      alert(
                        "No features to download. Please draw something first!",
                      );
                      return;
                    }

                    // Convert to string with nice formatting
                    const dataStr = JSON.stringify(data, null, 2);

                    // Create blob and download
                    const blob = new Blob([dataStr], {
                      type: "application/json",
                    });
                    const url = URL.createObjectURL(blob);

                    const a = document.createElement("a");
                    a.href = url;
                    a.download = `${x.draw_control.download_filename}.geojson`;
                    document.body.appendChild(a);
                    a.click();
                    document.body.removeChild(a);
                    URL.revokeObjectURL(url);
                  });

                  // Append to the Draw control button group
                  drawButtons.appendChild(downloadBtn);
                }
              }, 100);
            }
          }

          function updateDrawnFeatures() {
            if (draw) {
              var drawnFeatures = draw.getAll();
              if (HTMLWidgets.shinyMode) {
                Shiny.setInputValue(
                  el.id + "_drawn_features",
                  JSON.stringify(drawnFeatures),
                );
              }
              // Store drawn features in the widget's data
              if (el.querySelector) {
                var widget = HTMLWidgets.find("#" + el.id);
                if (widget) {
                  widget.drawFeatures = drawnFeatures;
                }
              }
            }
          }

          if (!x.add) {
            const existingLegends = el.querySelectorAll(".mapboxgl-legend");
            existingLegends.forEach((legend) => legend.remove());
          }

          if (x.legend_html && x.legend_css) {
            const legendCss = document.createElement("style");
            legendCss.innerHTML = x.legend_css;
            document.head.appendChild(legendCss);

            const legend = document.createElement("div");
            legend.innerHTML = x.legend_html;
            legend.classList.add("mapboxgl-legend");
            el.appendChild(legend);
          }

          // Add fullscreen control if enabled
          if (x.fullscreen_control && x.fullscreen_control.enabled) {
            const position = x.fullscreen_control.position || "top-right";
            const fullscreen = new mapboxgl.FullscreenControl();
            map.addControl(fullscreen, position);
            map.controls.push(fullscreen);
          }

          // Add geolocate control if enabled
          if (x.geolocate_control) {
            const geolocate = new mapboxgl.GeolocateControl({
              positionOptions: x.geolocate_control.positionOptions,
              trackUserLocation: x.geolocate_control.trackUserLocation,
              showAccuracyCircle: x.geolocate_control.showAccuracyCircle,
              showUserLocation: x.geolocate_control.showUserLocation,
              showUserHeading: x.geolocate_control.showUserHeading,
              fitBoundsOptions: x.geolocate_control.fitBoundsOptions,
            });
            map.addControl(geolocate, x.geolocate_control.position);
            map.controls.push(geolocate);

            if (HTMLWidgets.shinyMode) {
              geolocate.on("geolocate", function (event) {
                console.log("Geolocate event triggered");
                console.log("Element ID:", el.id);
                console.log("Event coords:", event.coords);

                Shiny.setInputValue(el.id + "_geolocate", {
                  coords: event.coords,
                  time: new Date(),
                });
              });

              geolocate.on("trackuserlocationstart", function () {
                Shiny.setInputValue(el.id + "_geolocate_tracking", {
                  status: "start",
                  time: new Date(),
                });
              });

              geolocate.on("trackuserlocationend", function () {
                Shiny.setInputValue(el.id + "_geolocate_tracking", {
                  status: "end",
                  time: new Date(),
                });
              });

              geolocate.on("error", function (error) {
                if (error.error.code === 1) {
                  Shiny.setInputValue(el.id + "_geolocate_error", {
                    message: "Location permission denied",
                    time: new Date(),
                  });
                }
              });
            }
          }

          // Add navigation control if enabled
          if (x.navigation_control) {
            const nav = new mapboxgl.NavigationControl({
              showCompass: x.navigation_control.show_compass,
              showZoom: x.navigation_control.show_zoom,
              visualizePitch: x.navigation_control.visualize_pitch,
            });
            map.addControl(nav, x.navigation_control.position);
            map.controls.push(nav);

            if (x.navigation_control.orientation === "horizontal") {
              const navBar = map
                .getContainer()
                .querySelector(
                  ".mapboxgl-ctrl.mapboxgl-ctrl-group:not(.mapbox-gl-draw_ctrl-draw-btn)",
                );
              if (navBar) {
                navBar.style.display = "flex";
                navBar.style.flexDirection = "row";
              }
            }
          }

          // Add reset control if enabled
          if (x.reset_control) {
            const resetControl = document.createElement("button");
            resetControl.className = "mapboxgl-ctrl-icon mapboxgl-ctrl-reset";
            resetControl.type = "button";
            resetControl.setAttribute("aria-label", "Reset");
            resetControl.innerHTML = "⟲";
            resetControl.style.fontSize = "30px";
            resetControl.style.fontWeight = "bold";
            resetControl.style.backgroundColor = "white";
            resetControl.style.border = "none";
            resetControl.style.cursor = "pointer";
            resetControl.style.padding = "0";
            resetControl.style.width = "30px";
            resetControl.style.height = "30px";
            resetControl.style.display = "flex";
            resetControl.style.justifyContent = "center";
            resetControl.style.alignItems = "center";
            resetControl.style.transition = "background-color 0.2s";
            resetControl.addEventListener("mouseover", function () {
              this.style.backgroundColor = "#f0f0f0";
            });
            resetControl.addEventListener("mouseout", function () {
              this.style.backgroundColor = "white";
            });

            const resetContainer = document.createElement("div");
            resetContainer.className = "mapboxgl-ctrl mapboxgl-ctrl-group";
            resetContainer.appendChild(resetControl);

            // Initialize with empty object, will be populated after map loads
            let initialView = {};

            // Capture the initial view after the map has loaded and all view operations are complete
            map.once("load", function () {
              initialView = {
                center: map.getCenter(),
                zoom: map.getZoom(),
                pitch: map.getPitch(),
                bearing: map.getBearing(),
                animate: x.reset_control.animate,
              };

              if (x.reset_control.duration) {
                initialView.duration = x.reset_control.duration;
              }
            });

            resetControl.onclick = function () {
              // Only reset if we have captured the initial view
              if (initialView.center) {
                map.easeTo(initialView);
              }
            };

            map.addControl(
              {
                onAdd: function () {
                  return resetContainer;
                },
                onRemove: function () {
                  resetContainer.parentNode.removeChild(resetContainer);
                },
              },
              x.reset_control.position,
            );

            map.controls.push({
              onAdd: function () {
                return resetContainer;
              },
              onRemove: function () {
                resetContainer.parentNode.removeChild(resetContainer);
              },
            });
          }

          if (x.setProjection) {
            x.setProjection.forEach(function (projectionConfig) {
              if (projectionConfig.projection) {
                map.setProjection(projectionConfig.projection);
              }
            });
          }

          if (x.images && Array.isArray(x.images)) {
            x.images.forEach(function (imageInfo) {
              map.loadImage(imageInfo.url, function (error, image) {
                if (error) {
                  console.error("Error loading image:", error);
                  return;
                }
                if (!map.hasImage(imageInfo.id)) {
                  map.addImage(imageInfo.id, image, imageInfo.options);
                }
              });
            });
          } else if (x.images) {
            console.error("x.images is not an array:", x.images);
          }

          // Add the layers control if provided
          if (x.layers_control) {
            const layersControl = document.createElement("div");
            layersControl.id = x.layers_control.control_id;
            layersControl.className = x.layers_control.collapsible
              ? "layers-control collapsible"
              : "layers-control";
            layersControl.style.position = "absolute";

            // Set the position correctly - fix position bug by using correct CSS positioning
            const position = x.layers_control.position || "top-left";
            if (position === "top-left") {
              layersControl.style.top =
                (x.layers_control.margin_top || 10) + "px";
              layersControl.style.left =
                (x.layers_control.margin_left || 10) + "px";
            } else if (position === "top-right") {
              layersControl.style.top =
                (x.layers_control.margin_top || 10) + "px";
              layersControl.style.right =
                (x.layers_control.margin_right || 10) + "px";
            } else if (position === "bottom-left") {
              layersControl.style.bottom =
                (x.layers_control.margin_bottom || 30) + "px";
              layersControl.style.left =
                (x.layers_control.margin_left || 10) + "px";
            } else if (position === "bottom-right") {
              layersControl.style.bottom =
                (x.layers_control.margin_bottom || 40) + "px";
              layersControl.style.right =
                (x.layers_control.margin_right || 10) + "px";
            }

            // Apply custom colors if provided
            if (x.layers_control.custom_colors) {
              const colors = x.layers_control.custom_colors;

              // Create a style element for custom colors
              const styleEl = document.createElement("style");
              let css = "";

              if (colors.background) {
                css += `#${x.layers_control.control_id} { background-color: ${colors.background}; }\n`;
              }

              if (colors.text) {
                css += `#${x.layers_control.control_id} a { color: ${colors.text}; }\n`;
              }

              if (colors.active) {
                css += `#${x.layers_control.control_id} a.active { background-color: ${colors.active}; }\n`;
                css += `#${x.layers_control.control_id} .toggle-button { background-color: ${colors.active}; }\n`;
              }

              if (colors.activeText) {
                css += `#${x.layers_control.control_id} a.active { color: ${colors.activeText}; }\n`;
                css += `#${x.layers_control.control_id} .toggle-button { color: ${colors.activeText}; }\n`;
              }

              if (colors.hover) {
                css += `#${x.layers_control.control_id} a:hover { background-color: ${colors.hover}; }\n`;
                css += `#${x.layers_control.control_id} .toggle-button:hover { background-color: ${colors.hover}; }\n`;
              }

              styleEl.textContent = css;
              document.head.appendChild(styleEl);
            }

            el.appendChild(layersControl);

            const layersList = document.createElement("div");
            layersList.className = "layers-list";
            layersControl.appendChild(layersList);

            // Fetch layers to be included in the control
            let layers =
              x.layers_control.layers ||
              map.getStyle().layers.map((layer) => layer.id);
            let layersConfig = x.layers_control.layers_config;

            // If we have a layers_config, use that; otherwise fall back to original behavior
            if (layersConfig && Array.isArray(layersConfig)) {
              layersConfig.forEach((config, index) => {
                const link = document.createElement("a");
                // Ensure config.ids is always an array
                const layerIds = Array.isArray(config.ids)
                  ? config.ids
                  : [config.ids];
                link.id = layerIds.join("-");
                link.href = "#";
                link.textContent = config.label;
                link.setAttribute("data-layer-ids", JSON.stringify(layerIds));
                link.setAttribute("data-layer-type", config.type);

                // Check if the first layer's visibility is set to "none" initially
                const firstLayerId = layerIds[0];
                const initialVisibility = map.getLayoutProperty(
                  firstLayerId,
                  "visibility",
                );
                link.className = initialVisibility === "none" ? "" : "active";

                // Also hide any associated legends if the layer is initially hidden
                if (initialVisibility === "none") {
                  layerIds.forEach((layerId) => {
                    const associatedLegends = document.querySelectorAll(
                      `.mapboxgl-legend[data-layer-id="${layerId}"]`,
                    );
                    associatedLegends.forEach((legend) => {
                      legend.style.display = "none";
                    });
                  });
                }

                // Show or hide layer(s) when the toggle is clicked
                link.onclick = function (e) {
                  e.preventDefault();
                  e.stopPropagation();

                  const layerIds = JSON.parse(
                    this.getAttribute("data-layer-ids"),
                  );
                  const firstLayerId = layerIds[0];
                  const visibility = map.getLayoutProperty(
                    firstLayerId,
                    "visibility",
                  );

                  // Toggle visibility for all layer IDs in the group
                  if (visibility === "visible") {
                    layerIds.forEach((layerId) => {
                      map.setLayoutProperty(layerId, "visibility", "none");
                      // Hide associated legends
                      const associatedLegends = document.querySelectorAll(
                        `.mapboxgl-legend[data-layer-id="${layerId}"]`,
                      );
                      associatedLegends.forEach((legend) => {
                        legend.style.display = "none";
                      });
                    });
                    this.className = "";
                  } else {
                    layerIds.forEach((layerId) => {
                      map.setLayoutProperty(layerId, "visibility", "visible");
                      // Show associated legends
                      const associatedLegends = document.querySelectorAll(
                        `.mapboxgl-legend[data-layer-id="${layerId}"]`,
                      );
                      associatedLegends.forEach((legend) => {
                        legend.style.display = "";
                      });
                    });
                    this.className = "active";
                  }
                };

                layersList.appendChild(link);
              });
            } else {
              // Fallback to original behavior for simple layer arrays
              // Ensure layers is always an array
              if (!Array.isArray(layers)) {
                layers = [layers];
              }

              layers.forEach((layerId, index) => {
                const link = document.createElement("a");
                link.id = layerId;
                link.href = "#";
                link.textContent = layerId;

                // Check if the layer visibility is set to "none" initially
                const initialVisibility = map.getLayoutProperty(
                  layerId,
                  "visibility",
                );
                link.className = initialVisibility === "none" ? "" : "active";

                // Also hide any associated legends if the layer is initially hidden
                if (initialVisibility === "none") {
                  const associatedLegends = document.querySelectorAll(
                    `.mapboxgl-legend[data-layer-id="${layerId}"]`,
                  );
                  associatedLegends.forEach((legend) => {
                    legend.style.display = "none";
                  });
                }

                // Show or hide layer when the toggle is clicked
                link.onclick = function (e) {
                  const clickedLayer = this.textContent;
                  e.preventDefault();
                  e.stopPropagation();

                  const visibility = map.getLayoutProperty(
                    clickedLayer,
                    "visibility",
                  );

                  // Toggle layer visibility by changing the layout object's visibility property
                  if (visibility === "visible") {
                    map.setLayoutProperty(clickedLayer, "visibility", "none");
                    this.className = "";

                    // Hide associated legends
                    const associatedLegends = document.querySelectorAll(
                      `.mapboxgl-legend[data-layer-id="${clickedLayer}"]`,
                    );
                    associatedLegends.forEach((legend) => {
                      legend.style.display = "none";
                    });
                  } else {
                    this.className = "active";
                    map.setLayoutProperty(
                      clickedLayer,
                      "visibility",
                      "visible",
                    );

                    // Show associated legends
                    const associatedLegends = document.querySelectorAll(
                      `.mapboxgl-legend[data-layer-id="${clickedLayer}"]`,
                    );
                    associatedLegends.forEach((legend) => {
                      legend.style.display = "";
                    });
                  }
                };

                layersList.appendChild(link);
              });
            }

            // Handle collapsible behavior
            if (x.layers_control.collapsible) {
              const toggleButton = document.createElement("div");
              toggleButton.className = "toggle-button";

              // Use stacked layers icon instead of text if requested
              if (x.layers_control.use_icon) {
                // Add icon-only class to the control for compact styling
                layersControl.classList.add("icon-only");

                // More GIS-like layers stack icon
                toggleButton.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                    <polygon points="12 2 2 7 12 12 22 7 12 2"></polygon>
                                    <polyline points="2 17 12 22 22 17"></polyline>
                                    <polyline points="2 12 12 17 22 12"></polyline>
                                </svg>`;
                toggleButton.style.display = "flex";
                toggleButton.style.alignItems = "center";
                toggleButton.style.justifyContent = "center";
              } else {
                toggleButton.textContent = "Layers";
              }

              toggleButton.onclick = function () {
                layersControl.classList.toggle("open");
              };
              layersControl.insertBefore(toggleButton, layersList);
            }
          }

          // If clusters are present, add event handling
          map.getStyle().layers.forEach((layer) => {
            if (layer.id.includes("-clusters")) {
              map.on("click", layer.id, (e) => {
                const features = map.queryRenderedFeatures(e.point, {
                  layers: [layer.id],
                });
                const clusterId = features[0].properties.cluster_id;
                map
                  .getSource(layer.source)
                  .getClusterExpansionZoom(clusterId, (err, zoom) => {
                    if (err) return;

                    map.easeTo({
                      center: features[0].geometry.coordinates,
                      zoom: zoom,
                    });
                  });
              });

              map.on("mouseenter", layer.id, () => {
                map.getCanvas().style.cursor = "pointer";
              });
              map.on("mouseleave", layer.id, () => {
                map.getCanvas().style.cursor = "";
              });
            }
          });

          // Add click event listener in shinyMode
          if (HTMLWidgets.shinyMode) {
            map.on("click", function (e) {
              // Check if draw control is active and in a drawing mode
              let isDrawing = false;
              if (typeof draw !== "undefined" && draw) {
                const mode = draw.getMode();
                isDrawing =
                  mode === "draw_point" ||
                  mode === "draw_line_string" ||
                  mode === "draw_polygon";
              }

              // Only process feature clicks if not actively drawing
              if (!isDrawing) {
                const features = map.queryRenderedFeatures(e.point);

                if (features.length > 0) {
                  const feature = features[0];
                  Shiny.onInputChange(el.id + "_feature_click", {
                    id: feature.id,
                    properties: feature.properties,
                    layer: feature.layer.id,
                    lng: e.lngLat.lng,
                    lat: e.lngLat.lat,
                    time: new Date(),
                  });
                } else {
                  Shiny.onInputChange(el.id + "_feature_click", null);
                }
              }

              // Event listener for the map (always fire this)
              Shiny.onInputChange(el.id + "_click", {
                lng: e.lngLat.lng,
                lat: e.lngLat.lat,
                time: new Date(),
              });
            });

            // add hover listener for shinyMode if enabled
            if (x.hover_events && x.hover_events.enabled) {
              map.on("mousemove", function (e) {
                // Feature hover events
                if (x.hover_events.features) {
                  const options = x.hover_events.layer_id
                    ? {
                        layers: Array.isArray(x.hover_events.layer_id)
                          ? x.hover_events.layer_id
                          : x.hover_events.layer_id
                              .split(",")
                              .map((id) => id.trim()),
                      }
                    : undefined;
                  const features = map.queryRenderedFeatures(e.point, options);

                  if (features.length > 0) {
                    const feature = features[0];
                    Shiny.onInputChange(el.id + "_feature_hover", {
                      id: feature.id,
                      properties: feature.properties,
                      layer: feature.layer.id,
                      lng: e.lngLat.lng,
                      lat: e.lngLat.lat,
                      time: new Date(),
                    });
                  } else {
                    Shiny.onInputChange(el.id + "_feature_hover", null);
                  }
                }

                // Coordinate hover events
                if (x.hover_events.coordinates) {
                  Shiny.onInputChange(el.id + "_hover", {
                    lng: e.lngLat.lng,
                    lat: e.lngLat.lat,
                    time: new Date(),
                  });
                }
              });
            }
          }

          // Process turf operations for static maps
          if (x.turf_operations) {
            processTurfOperationsOnLoad(map, x.turf_operations, el.id);
          }

          el.map = map;
        });

        el.map = map;
      },

      getMap: function () {
        return map; // Return the map instance
      },

      getDraw: function () {
        return draw; // Return the draw instance
      },

      getDrawnFeatures: function () {
        return (
          this.drawFeatures || {
            type: "FeatureCollection",
            features: [],
          }
        );
      },

      resize: function (width, height) {
        if (map) {
          map.resize();
        }
      },
    };
  },
});

if (HTMLWidgets.shinyMode) {
  Shiny.addCustomMessageHandler("mapboxgl-proxy", function (data) {
    var widget = HTMLWidgets.find("#" + data.id);
    if (!widget) return;
    var map = widget.getMap();
    if (map) {
      var message = data.message;

      // Initialize layer state tracking if not already present
      if (!window._mapglLayerState) {
        window._mapglLayerState = {};
      }
      const mapId = map.getContainer().id;
      if (!window._mapglLayerState[mapId]) {
        window._mapglLayerState[mapId] = {
          filters: {}, // layerId -> filter expression
          paintProperties: {}, // layerId -> {propertyName -> value}
          layoutProperties: {}, // layerId -> {propertyName -> value}
          tooltips: {}, // layerId -> tooltip property
          popups: {}, // layerId -> popup property
          legends: {}, // legendId -> {html: string, css: string}
        };
      }
      const layerState = window._mapglLayerState[mapId];

      // Helper function to update drawn features
      function updateDrawnFeatures() {
        var drawControl = widget.drawControl || widget.getDraw();
        if (drawControl) {
          var drawnFeatures = drawControl.getAll();
          if (HTMLWidgets.shinyMode) {
            Shiny.setInputValue(
              data.id + "_drawn_features",
              JSON.stringify(drawnFeatures),
            );
          }
          // Store drawn features in the widget's data
          if (widget) {
            widget.drawFeatures = drawnFeatures;
          }
        }
      }
      if (message.type === "set_filter") {
        map.setFilter(message.layer, message.filter);
        // Track filter state for layer restoration
        layerState.filters[message.layer] = message.filter;
      } else if (message.type === "add_source") {
        if (message.source.type === "vector") {
          const sourceConfig = {
            type: "vector",
          };
          // Add url or tiles
          if (message.source.url) {
            sourceConfig.url = message.source.url;
          }
          if (message.source.tiles) {
            sourceConfig.tiles = message.source.tiles;
          }
          // Add promoteId if provided
          if (message.source.promoteId) {
            sourceConfig.promoteId = message.source.promoteId;
          }
          // Add any other properties from the source object
          Object.keys(message.source).forEach(function (key) {
            if (
              key !== "id" &&
              key !== "type" &&
              key !== "url" &&
              key !== "tiles" &&
              key !== "promoteId"
            ) {
              sourceConfig[key] = message.source[key];
            }
          });
          map.addSource(message.source.id, sourceConfig);
        } else if (message.source.type === "geojson") {
          const sourceConfig = {
            type: "geojson",
            data: message.source.data,
            generateId: message.source.generateId,
          };
          // Add any other properties from the source object
          Object.keys(message.source).forEach(function (key) {
            if (
              key !== "id" &&
              key !== "type" &&
              key !== "data" &&
              key !== "generateId"
            ) {
              sourceConfig[key] = message.source[key];
            }
          });
          map.addSource(message.source.id, sourceConfig);
        } else if (message.source.type === "raster") {
          const sourceConfig = {
            type: "raster",
            tileSize: message.source.tileSize,
          };
          if (message.source.url) {
            sourceConfig.url = message.source.url;
          } else if (message.source.tiles) {
            sourceConfig.tiles = message.source.tiles;
          }
          if (message.source.maxzoom) {
            sourceConfig.maxzoom = message.source.maxzoom;
          }
          // Add any other properties from the source object
          Object.keys(message.source).forEach(function (key) {
            if (
              key !== "id" &&
              key !== "type" &&
              key !== "url" &&
              key !== "tiles" &&
              key !== "tileSize" &&
              key !== "maxzoom"
            ) {
              sourceConfig[key] = message.source[key];
            }
          });
          map.addSource(message.source.id, sourceConfig);
        } else if (message.source.type === "raster-dem") {
          const sourceConfig = {
            type: "raster-dem",
            url: message.source.url,
            tileSize: message.source.tileSize,
          };
          if (message.source.maxzoom) {
            sourceConfig.maxzoom = message.source.maxzoom;
          }
          // Add any other properties from the source object
          Object.keys(message.source).forEach(function (key) {
            if (
              key !== "id" &&
              key !== "type" &&
              key !== "url" &&
              key !== "tileSize" &&
              key !== "maxzoom"
            ) {
              sourceConfig[key] = message.source[key];
            }
          });
          map.addSource(message.source.id, sourceConfig);
        } else if (message.source.type === "image") {
          const sourceConfig = {
            type: "image",
            url: message.source.url,
            coordinates: message.source.coordinates,
          };
          // Add any other properties from the source object
          Object.keys(message.source).forEach(function (key) {
            if (
              key !== "id" &&
              key !== "type" &&
              key !== "url" &&
              key !== "coordinates"
            ) {
              sourceConfig[key] = message.source[key];
            }
          });
          map.addSource(message.source.id, sourceConfig);
        } else if (message.source.type === "video") {
          const sourceConfig = {
            type: "video",
            urls: message.source.urls,
            coordinates: message.source.coordinates,
          };
          // Add any other properties from the source object
          Object.keys(message.source).forEach(function (key) {
            if (
              key !== "id" &&
              key !== "type" &&
              key !== "urls" &&
              key !== "coordinates"
            ) {
              sourceConfig[key] = message.source[key];
            }
          });
          map.addSource(message.source.id, sourceConfig);
        } else {
          // Handle custom source types (like pmtile-source)
          const sourceConfig = { type: message.source.type };

          // Copy all properties except id
          Object.keys(message.source).forEach(function (key) {
            if (key !== "id") {
              sourceConfig[key] = message.source[key];
            }
          });

          map.addSource(message.source.id, sourceConfig);
        }
      } else if (message.type === "add_layer") {
        try {
          if (message.layer.before_id) {
            map.addLayer(message.layer, message.layer.before_id);
          } else {
            map.addLayer(message.layer);
          }

          // Add popups or tooltips if provided
          if (message.layer.popup) {
            // Initialize popup tracking if it doesn't exist
            if (!window._mapboxPopups) {
              window._mapboxPopups = {};
            }

            // Create click handler for this layer
            const clickHandler = function (e) {
              onClickPopup(e, map, message.layer.popup, message.layer.id);
            };

            // Store these handler references so we can remove them later if needed
            if (!window._mapboxClickHandlers) {
              window._mapboxClickHandlers = {};
            }
            window._mapboxClickHandlers[message.layer.id] = clickHandler;

            // Add the click handler
            map.on("click", message.layer.id, clickHandler);

            // Change cursor to pointer when hovering over the layer
            map.on("mouseenter", message.layer.id, function () {
              map.getCanvas().style.cursor = "pointer";
            });

            // Change cursor back to default when leaving the layer
            map.on("mouseleave", message.layer.id, function () {
              map.getCanvas().style.cursor = "";
            });
          }

          if (message.layer.tooltip) {
            const tooltip = new mapboxgl.Popup({
              closeButton: false,
              closeOnClick: false,
              maxWidth: '400px',
            });

            // Define named handler functions:
            const mouseMoveHandler = function (e) {
              onMouseMoveTooltip(e, map, tooltip, message.layer.tooltip);
            };

            const mouseLeaveHandler = function () {
              onMouseLeaveTooltip(map, tooltip);
            };

            // Attach handlers by reference:
            map.on("mousemove", message.layer.id, mouseMoveHandler);
            map.on("mouseleave", message.layer.id, mouseLeaveHandler);

            // Store these handler references for later removal:
            if (!window._mapboxHandlers) {
              window._mapboxHandlers = {};
            }
            window._mapboxHandlers[message.layer.id] = {
              mousemove: mouseMoveHandler,
              mouseleave: mouseLeaveHandler,
            };
          }

          // Add hover effect if provided
          if (message.layer.hover_options) {
            const jsHoverOptions = {};
            for (const [key, value] of Object.entries(
              message.layer.hover_options,
            )) {
              const jsKey = key.replace(/_/g, "-");
              jsHoverOptions[jsKey] = value;
            }

            let hoveredFeatureId = null;

            map.on("mousemove", message.layer.id, function (e) {
              if (e.features.length > 0) {
                if (hoveredFeatureId !== null) {
                  const featureState = {
                    source:
                      typeof message.layer.source === "string"
                        ? message.layer.source
                        : message.layer.id,
                    id: hoveredFeatureId,
                  };
                  if (message.layer.source_layer) {
                    featureState.sourceLayer = message.layer.source_layer;
                  }
                  map.setFeatureState(featureState, {
                    hover: false,
                  });
                }
                hoveredFeatureId = e.features[0].id;
                const featureState = {
                  source:
                    typeof message.layer.source === "string"
                      ? message.layer.source
                      : message.layer.id,
                  id: hoveredFeatureId,
                };
                if (message.layer.source_layer) {
                  featureState.sourceLayer = message.layer.source_layer;
                }
                map.setFeatureState(featureState, {
                  hover: true,
                });
              }
            });

            map.on("mouseleave", message.layer.id, function () {
              if (hoveredFeatureId !== null) {
                const featureState = {
                  source:
                    typeof message.layer.source === "string"
                      ? message.layer.source
                      : message.layer.id,
                  id: hoveredFeatureId,
                };
                if (message.layer.source_layer) {
                  featureState.sourceLayer = message.layer.source_layer;
                }
                map.setFeatureState(featureState, {
                  hover: false,
                });
              }
              hoveredFeatureId = null;
            });

            Object.keys(jsHoverOptions).forEach(function (key) {
              const originalPaint =
                map.getPaintProperty(message.layer.id, key) ||
                message.layer.paint[key];
              map.setPaintProperty(message.layer.id, key, [
                "case",
                ["boolean", ["feature-state", "hover"], false],
                jsHoverOptions[key],
                originalPaint,
              ]);
            });
          }
        } catch (e) {
          console.error("Failed to add layer via proxy: ", message.layer, e);
        }
      } else if (message.type === "remove_layer") {
        // If there's an active tooltip, remove it first
        if (window._activeTooltip) {
          window._activeTooltip.remove();
          delete window._activeTooltip;
        }

        // If there's an active popup for this layer, remove it
        if (window._mapboxPopups && window._mapboxPopups[message.layer]) {
          window._mapboxPopups[message.layer].remove();
          delete window._mapboxPopups[message.layer];
        }

        if (map.getLayer(message.layer)) {
          // Remove tooltip handlers
          if (window._mapboxHandlers && window._mapboxHandlers[message.layer]) {
            const handlers = window._mapboxHandlers[message.layer];
            if (handlers.mousemove) {
              map.off("mousemove", message.layer, handlers.mousemove);
            }
            if (handlers.mouseleave) {
              map.off("mouseleave", message.layer, handlers.mouseleave);
            }
            // Clean up the reference
            delete window._mapboxHandlers[message.layer];
          }

          // Remove click handlers for popups
          if (
            window._mapboxClickHandlers &&
            window._mapboxClickHandlers[message.layer]
          ) {
            map.off(
              "click",
              message.layer,
              window._mapboxClickHandlers[message.layer],
            );
            delete window._mapboxClickHandlers[message.layer];
          }

          // Remove the layer
          map.removeLayer(message.layer);
        }
        if (map.getSource(message.layer)) {
          map.removeSource(message.layer);
        }

        // Clean up tracked layer state
        const mapId = map.getContainer().id;
        if (window._mapglLayerState && window._mapglLayerState[mapId]) {
          const layerState = window._mapglLayerState[mapId];
          delete layerState.filters[message.layer];
          delete layerState.paintProperties[message.layer];
          delete layerState.layoutProperties[message.layer];
          delete layerState.tooltips[message.layer];
          delete layerState.popups[message.layer];
          // Note: legends are not tied to specific layers, so we don't clear them here
        }
      } else if (message.type === "fit_bounds") {
        map.fitBounds(message.bounds, message.options);
      } else if (message.type === "fly_to") {
        map.flyTo(message.options);
      } else if (message.type === "ease_to") {
        map.easeTo(message.options);
      } else if (message.type === "set_center") {
        map.setCenter(message.center);
      } else if (message.type === "set_zoom") {
        map.setZoom(message.zoom);
      } else if (message.type === "jump_to") {
        map.jumpTo(message.options);
      } else if (message.type === "set_layout_property") {
        map.setLayoutProperty(message.layer, message.name, message.value);
        // Track layout property state for layer restoration
        if (!layerState.layoutProperties[message.layer]) {
          layerState.layoutProperties[message.layer] = {};
        }
        layerState.layoutProperties[message.layer][message.name] =
          message.value;
      } else if (message.type === "set_paint_property") {
        const layerId = message.layer;
        const propertyName = message.name;
        const newValue = message.value;

        // Check if the layer has hover options
        const layerStyle = map
          .getStyle()
          .layers.find((layer) => layer.id === layerId);
        const currentPaintProperty = map.getPaintProperty(
          layerId,
          propertyName,
        );

        if (
          currentPaintProperty &&
          Array.isArray(currentPaintProperty) &&
          currentPaintProperty[0] === "case"
        ) {
          // This property has hover options, so we need to preserve them
          const hoverValue = currentPaintProperty[2];
          const newPaintProperty = [
            "case",
            ["boolean", ["feature-state", "hover"], false],
            hoverValue,
            newValue,
          ];
          map.setPaintProperty(layerId, propertyName, newPaintProperty);
        } else {
          // No hover options, just set the new value directly
          map.setPaintProperty(layerId, propertyName, newValue);
        }
        // Track paint property state for layer restoration
        if (!layerState.paintProperties[layerId]) {
          layerState.paintProperties[layerId] = {};
        }
        layerState.paintProperties[layerId][propertyName] = newValue;
      } else if (message.type === "query_rendered_features") {
        // Query rendered features
        let queryOptions = {};
        if (message.layers) {
          // Ensure layers is always an array
          queryOptions.layers = Array.isArray(message.layers)
            ? message.layers
            : [message.layers];
        }
        if (message.filter) queryOptions.filter = message.filter;

        let features;
        if (message.geometry) {
          features = map.queryRenderedFeatures(message.geometry, queryOptions);
        } else {
          // No geometry specified - query entire viewport
          features = map.queryRenderedFeatures(queryOptions);
        }

        // Deduplicate features by id or by properties if no id
        const uniqueFeatures = new Map();
        features.forEach(function (feature) {
          let key;
          if (feature.id !== undefined && feature.id !== null) {
            key = feature.id;
          } else {
            // Create a key from properties if no id available
            key = JSON.stringify(feature.properties);
          }

          if (!uniqueFeatures.has(key)) {
            uniqueFeatures.set(key, feature);
          }
        });

        // Convert to GeoJSON FeatureCollection
        const deduplicatedFeatures = Array.from(uniqueFeatures.values());
        const featureCollection = {
          type: "FeatureCollection",
          features: deduplicatedFeatures,
        };

        Shiny.setInputValue(
          data.id + "_queried_features",
          JSON.stringify(featureCollection),
        );
      } else if (message.type === "add_legend") {
        // Extract legend ID from HTML to track it
        const legendIdMatch = message.html.match(/id="([^"]+)"/);
        const legendId = legendIdMatch ? legendIdMatch[1] : null;

        if (!message.add) {
          const existingLegends = document.querySelectorAll(
            `#${data.id} .mapboxgl-legend`,
          );
          existingLegends.forEach((legend) => legend.remove());

          // Clean up any existing legend styles that might have been added
          const legendStyles = document.querySelectorAll(
            "style[data-mapgl-legend-css]",
          );
          legendStyles.forEach((style) => style.remove());

          // Clear legend state when replacing all legends
          layerState.legends = {};
        }

        // Track legend state
        if (legendId) {
          layerState.legends[legendId] = {
            html: message.html,
            css: message.legend_css,
          };
        }

        const legendCss = document.createElement("style");
        legendCss.innerHTML = message.legend_css;
        legendCss.setAttribute("data-mapgl-legend-css", data.id); // Mark this style for later cleanup
        document.head.appendChild(legendCss);

        const legend = document.createElement("div");
        legend.innerHTML = message.html;
        legend.classList.add("mapboxgl-legend");
        document.getElementById(data.id).appendChild(legend);
      } else if (message.type === "set_config_property") {
        map.setConfigProperty(
          message.importId,
          message.configName,
          message.value,
        );
      } else if (message.type === "set_style") {
        // Default preserve_layers to true if not specified
        const preserveLayers = message.preserve_layers !== false;

        // If we should preserve layers and sources
        if (preserveLayers) {
          // Store the current style before changing it
          const currentStyle = map.getStyle();
          const userSourceIds = [];
          const userLayers = [];

          // Identify user-added sources (those not in the original style)
          // We'll assume any source that's not "composite", "mapbox", or starts with "mapbox-" is user-added
          for (const sourceId in currentStyle.sources) {
            if (
              sourceId !== "composite" &&
              sourceId !== "mapbox" &&
              !sourceId.startsWith("mapbox-")
            ) {
              userSourceIds.push(sourceId);
              const source = currentStyle.sources[sourceId];
              // Store layer-specific handler references
              if (window._mapboxHandlers) {
                const handlers = window._mapboxHandlers;
                for (const layerId in handlers) {
                  // Find layers associated with this source
                  const layer = currentStyle.layers.find(
                    (l) => l.id === layerId,
                  );
                  if (layer && layer.source === sourceId) {
                    layer._handlers = handlers[layerId];
                  }
                }
              }
            }
          }

          // Identify layers using user-added sources
          currentStyle.layers.forEach(function (layer) {
            if (userSourceIds.includes(layer.source)) {
              userLayers.push(layer);
            }
          });

          // Set up event listener to re-add sources and layers after style loads
          const onStyleLoad = function () {
            // Re-add user sources
            userSourceIds.forEach(function (sourceId) {
              if (!map.getSource(sourceId)) {
                const source = currentStyle.sources[sourceId];
                map.addSource(sourceId, source);
              }
            });

            // Re-add user layers
            userLayers.forEach(function (layer) {
              if (!map.getLayer(layer.id)) {
                map.addLayer(layer);

                // Re-add event handlers for tooltips and hover effects
                if (layer._handlers) {
                  const handlers = layer._handlers;

                  if (handlers.mousemove) {
                    map.on("mousemove", layer.id, handlers.mousemove);
                  }

                  if (handlers.mouseleave) {
                    map.on("mouseleave", layer.id, handlers.mouseleave);
                  }
                }

                // Recreate hover states if needed
                if (layer.paint) {
                  for (const key in layer.paint) {
                    const value = layer.paint[key];
                    if (
                      Array.isArray(value) &&
                      value[0] === "case" &&
                      Array.isArray(value[1]) &&
                      value[1][0] === "boolean" &&
                      value[1][1][0] === "feature-state" &&
                      value[1][1][1] === "hover"
                    ) {
                      // This is a hover-enabled paint property
                      map.setPaintProperty(layer.id, key, value);
                    }
                  }
                }
              }
            });

            // Clear any active tooltips before restoration to prevent stacking
            if (window._activeTooltip) {
              window._activeTooltip.remove();
              delete window._activeTooltip;
            }

            // Restore tracked layer modifications
            const mapId = map.getContainer().id;
            const savedLayerState =
              window._mapglLayerState && window._mapglLayerState[mapId];
            if (savedLayerState) {
              // Restore filters
              for (const layerId in savedLayerState.filters) {
                if (map.getLayer(layerId)) {
                  map.setFilter(layerId, savedLayerState.filters[layerId]);
                }
              }

              // Restore paint properties
              for (const layerId in savedLayerState.paintProperties) {
                if (map.getLayer(layerId)) {
                  const properties = savedLayerState.paintProperties[layerId];
                  for (const propertyName in properties) {
                    const savedValue = properties[propertyName];

                    // Check if layer has hover effects that need to be preserved
                    const currentValue = map.getPaintProperty(
                      layerId,
                      propertyName,
                    );
                    if (
                      currentValue &&
                      Array.isArray(currentValue) &&
                      currentValue[0] === "case"
                    ) {
                      // Preserve hover effects while updating base value
                      const hoverValue = currentValue[2];
                      const newPaintProperty = [
                        "case",
                        ["boolean", ["feature-state", "hover"], false],
                        hoverValue,
                        savedValue,
                      ];
                      map.setPaintProperty(
                        layerId,
                        propertyName,
                        newPaintProperty,
                      );
                    } else {
                      map.setPaintProperty(layerId, propertyName, savedValue);
                    }
                  }
                }
              }

              // Restore layout properties
              for (const layerId in savedLayerState.layoutProperties) {
                if (map.getLayer(layerId)) {
                  const properties = savedLayerState.layoutProperties[layerId];
                  for (const propertyName in properties) {
                    map.setLayoutProperty(
                      layerId,
                      propertyName,
                      properties[propertyName],
                    );
                  }
                }
              }

              // Restore tooltips
              for (const layerId in savedLayerState.tooltips) {
                if (map.getLayer(layerId)) {
                  const tooltipProperty = savedLayerState.tooltips[layerId];

                  // Remove existing tooltip handlers first
                  if (
                    window._mapboxHandlers &&
                    window._mapboxHandlers[layerId]
                  ) {
                    if (window._mapboxHandlers[layerId].mousemove) {
                      map.off(
                        "mousemove",
                        layerId,
                        window._mapboxHandlers[layerId].mousemove,
                      );
                    }
                    if (window._mapboxHandlers[layerId].mouseleave) {
                      map.off(
                        "mouseleave",
                        layerId,
                        window._mapboxHandlers[layerId].mouseleave,
                      );
                    }
                  }

                  // Create new tooltip
                  const tooltip = new mapboxgl.Popup({
                    closeButton: false,
                    closeOnClick: false,
                    maxWidth: '400px',
                  });

                  const mouseMoveHandler = function (e) {
                    onMouseMoveTooltip(e, map, tooltip, tooltipProperty);
                  };

                  const mouseLeaveHandler = function () {
                    onMouseLeaveTooltip(map, tooltip);
                  };

                  map.on("mousemove", layerId, mouseMoveHandler);
                  map.on("mouseleave", layerId, mouseLeaveHandler);

                  // Store handler references
                  if (!window._mapboxHandlers) {
                    window._mapboxHandlers = {};
                  }
                  window._mapboxHandlers[layerId] = {
                    mousemove: mouseMoveHandler,
                    mouseleave: mouseLeaveHandler,
                  };
                }
              }

              // Restore popups
              for (const layerId in savedLayerState.popups) {
                if (map.getLayer(layerId)) {
                  const popupProperty = savedLayerState.popups[layerId];

                  // Remove existing popup handlers first
                  if (
                    window._mapboxHandlers &&
                    window._mapboxHandlers[layerId] &&
                    window._mapboxHandlers[layerId].click
                  ) {
                    map.off(
                      "click",
                      layerId,
                      window._mapboxHandlers[layerId].click,
                    );
                  }

                  // Create new popup handler
                  const clickHandler = function (e) {
                    onClickPopup(e, map, popupProperty, layerId);
                  };

                  map.on("click", layerId, clickHandler);

                  // Store handler reference
                  if (!window._mapboxHandlers) {
                    window._mapboxHandlers = {};
                  }
                  if (!window._mapboxHandlers[layerId]) {
                    window._mapboxHandlers[layerId] = {};
                  }
                  window._mapboxHandlers[layerId].click = clickHandler;
                }
              }

              // Restore legends
              if (Object.keys(savedLayerState.legends).length > 0) {
                // Clear any existing legends first to prevent stacking
                const existingLegends = document.querySelectorAll(
                  `#${mapId} .mapboxgl-legend`,
                );
                existingLegends.forEach((legend) => legend.remove());

                // Clear existing legend styles
                const legendStyles = document.querySelectorAll(
                  `style[data-mapgl-legend-css="${mapId}"]`,
                );
                legendStyles.forEach((style) => style.remove());

                // Restore each legend
                for (const legendId in savedLayerState.legends) {
                  const legendData = savedLayerState.legends[legendId];

                  // Add legend CSS
                  const legendCss = document.createElement("style");
                  legendCss.innerHTML = legendData.css;
                  legendCss.setAttribute("data-mapgl-legend-css", mapId);
                  document.head.appendChild(legendCss);

                  // Add legend HTML
                  const legend = document.createElement("div");
                  legend.innerHTML = legendData.html;
                  legend.classList.add("mapboxgl-legend");
                  const mapContainer = document.getElementById(mapId);
                  if (mapContainer) {
                    mapContainer.appendChild(legend);
                  }
                }
              }
            }

            // Remove this listener to avoid adding the same layers multiple times
            map.off("style.load", onStyleLoad);
          };

          map.on("style.load", onStyleLoad);
        }

        // Change the style
        map.setStyle(message.style, {
          config: message.config,
          diff: message.diff,
        });
      } else if (message.type === "add_navigation_control") {
        const nav = new mapboxgl.NavigationControl({
          showCompass: message.options.show_compass,
          showZoom: message.options.show_zoom,
          visualizePitch: message.options.visualize_pitch,
        });
        map.addControl(nav, message.position);
        map.controls.push({ type: "navigation", control: nav });

        if (message.orientation === "horizontal") {
          const navBar = map
            .getContainer()
            .querySelector(
              ".mapboxgl-ctrl.mapboxgl-ctrl-group:not(.mapbox-gl-draw_ctrl-draw-btn)",
            );
          if (navBar) {
            navBar.style.display = "flex";
            navBar.style.flexDirection = "row";
          }
        }
      } else if (message.type === "add_reset_control") {
        const resetControl = document.createElement("button");
        resetControl.className = "mapboxgl-ctrl-icon mapboxgl-ctrl-reset";
        resetControl.type = "button";
        resetControl.setAttribute("aria-label", "Reset");
        resetControl.innerHTML = "⟲";
        resetControl.style.fontSize = "30px";
        resetControl.style.fontWeight = "bold";
        resetControl.style.backgroundColor = "white";
        resetControl.style.border = "none";
        resetControl.style.cursor = "pointer";
        resetControl.style.padding = "0";
        resetControl.style.width = "30px";
        resetControl.style.height = "30px";
        resetControl.style.display = "flex";
        resetControl.style.justifyContent = "center";
        resetControl.style.alignItems = "center";
        resetControl.style.transition = "background-color 0.2s";
        resetControl.addEventListener("mouseover", function () {
          this.style.backgroundColor = "#f0f0f0";
        });
        resetControl.addEventListener("mouseout", function () {
          this.style.backgroundColor = "white";
        });

        const resetContainer = document.createElement("div");
        resetContainer.className = "mapboxgl-ctrl mapboxgl-ctrl-group";
        resetContainer.appendChild(resetControl);

        const initialView = {
          center: map.getCenter(),
          zoom: map.getZoom(),
          pitch: map.getPitch(),
          bearing: map.getBearing(),
          animate: message.animate,
        };

        if (message.duration) {
          initialView.duration = message.duration;
        }

        resetControl.onclick = function () {
          map.easeTo(initialView);
        };

        map.addControl(
          {
            onAdd: function () {
              return resetContainer;
            },
            onRemove: function () {
              resetContainer.parentNode.removeChild(resetContainer);
            },
          },
          message.position,
        );

        map.controls.push({
          onAdd: function () {
            return resetContainer;
          },
          onRemove: function () {
            resetContainer.parentNode.removeChild(resetContainer);
          },
        });
      } else if (message.type === "add_draw_control") {
        let drawOptions = message.options || {};

        // Generate styles if styling parameters provided
        if (message.styling) {
          const generatedStyles = generateDrawStyles(message.styling);
          if (generatedStyles) {
            drawOptions.styles = generatedStyles;
          }
        }

        if (message.freehand) {
          drawOptions = Object.assign({}, drawOptions, {
            modes: Object.assign({}, MapboxDraw.modes, {
              draw_polygon: MapboxDraw.modes.draw_freehand,
            }),
            // defaultMode: 'draw_polygon' # Don't set the default yet
          });
        }

        // Create the draw control
        var drawControl = new MapboxDraw(drawOptions);
        map.addControl(drawControl, message.position);
        map.controls.push(drawControl);

        // Store the draw control on the widget for later access
        widget.drawControl = drawControl;

        // Add lasso icon CSS for freehand mode
        if (message.freehand) {
          if (!document.querySelector("#mapgl-freehand-lasso-styles")) {
            const style = document.createElement("style");
            style.id = "mapgl-freehand-lasso-styles";
            style.textContent = `
              .mapbox-gl-draw_polygon {
                background-image: url('data:image/svg+xml;utf8,%3Csvg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 20 20"%3E%3Cpath d="M 7 3 C 5 2.5, 3.5 3, 3 5 C 2.5 7, 3 8.5, 4 9.5 C 5 10.5, 5.5 11.5, 5 13 C 4.5 14.5, 5 15.5, 6.5 16 C 8 16.5, 9.5 16, 11 15 C 12.5 14, 13.5 13, 14.5 11.5 C 15.5 10, 16 8.5, 15.5 7 C 15 5.5, 14 4.5, 12.5 3.5 C 11 2.5, 9 2.5, 7 3 Z" fill="none" stroke="%23000000" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/%3E%3C/svg%3E') !important;
              }
            `;
            document.head.appendChild(style);
          }

          // Update tooltip for freehand mode
          setTimeout(() => {
            const polygonButton = map
              .getContainer()
              .querySelector(".mapbox-gl-draw_polygon");
            if (polygonButton) {
              polygonButton.title = "Freehand polygon tool (p)";
            }
          }, 100);
        }

        // Add event listeners
        map.on("draw.create", updateDrawnFeatures);
        map.on("draw.delete", updateDrawnFeatures);
        map.on("draw.update", updateDrawnFeatures);

        // Add initial features if provided
        if (message.source) {
          addSourceFeaturesToDraw(drawControl, message.source, map);
        }

        if (message.orientation === "horizontal") {
          const drawBar = map
            .getContainer()
            .querySelector(".mapboxgl-ctrl-group");
          if (drawBar) {
            drawBar.style.display = "flex";
            drawBar.style.flexDirection = "row";
          }
        }

        // Add download button if requested
        if (message.download_button) {
          // Add CSS for download button if not already added
          if (!document.querySelector("#mapgl-draw-download-styles")) {
            const style = document.createElement("style");
            style.id = "mapgl-draw-download-styles";
            style.textContent = `
              .mapbox-gl-draw_download {
                background: transparent;
                border: none;
                cursor: pointer;
                display: block;
                height: 30px;
                width: 30px;
                padding: 0;
                outline: none;
              }
              .mapbox-gl-draw_download:hover {
                background-color: rgba(0, 0, 0, 0.05);
              }
              .mapbox-gl-draw_download svg {
                width: 20px;
                height: 20px;
                margin: 5px;
                fill: #333;
              }
            `;
            document.head.appendChild(style);
          }

          // Small delay to ensure Draw control is fully rendered
          setTimeout(() => {
            // Find the Draw control button group
            const drawButtons = map
              .getContainer()
              .querySelector(
                ".mapboxgl-ctrl-group:has(.mapbox-gl-draw_polygon)",
              );

            if (drawButtons) {
              // Create download button
              const downloadBtn = document.createElement("button");
              downloadBtn.className = "mapbox-gl-draw_download";
              downloadBtn.title = "Download drawn features as GeoJSON";

              // Add SVG download icon
              downloadBtn.innerHTML = `
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
                  <path d="M19 9h-4V3H9v6H5l7 7 7-7zM5 18v2h14v-2H5z"/>
                </svg>
              `;

              downloadBtn.addEventListener("click", () => {
                // Get all drawn features
                const data = drawControl.getAll();

                if (data.features.length === 0) {
                  alert(
                    "No features to download. Please draw something first!",
                  );
                  return;
                }

                // Convert to string with nice formatting
                const dataStr = JSON.stringify(data, null, 2);

                // Create blob and download
                const blob = new Blob([dataStr], { type: "application/json" });
                const url = URL.createObjectURL(blob);

                const a = document.createElement("a");
                a.href = url;
                a.download = `${message.download_filename || "drawn-features"}.geojson`;
                document.body.appendChild(a);
                a.click();
                document.body.removeChild(a);
                URL.revokeObjectURL(url);
              });

              // Append to the Draw control button group
              drawButtons.appendChild(downloadBtn);
            }
          }, 100);
        }
      } else if (message.type === "get_drawn_features") {
        var drawControl = widget.drawControl || widget.getDraw();
        if (drawControl) {
          const features = drawControl.getAll();
          Shiny.setInputValue(
            data.id + "_drawn_features",
            JSON.stringify(features),
          );
        } else {
          Shiny.setInputValue(
            data.id + "_drawn_features",
            JSON.stringify(null),
          );
        }
      } else if (message.type === "clear_drawn_features") {
        var drawControl = widget.drawControl || widget.getDraw();
        if (drawControl) {
          drawControl.deleteAll();
          // Update the drawn features
          updateDrawnFeatures();
        }
      } else if (message.type.startsWith("turf_")) {
        // Delegate to shared turf operations module
        handleTurfOperation(map, message, data.id);
      } else if (message.type === "add_features_to_draw") {
        var drawControl = widget.drawControl || widget.getDraw();
        if (drawControl) {
          if (message.data.clear_existing) {
            drawControl.deleteAll();
          }
          addSourceFeaturesToDraw(drawControl, message.data.source, map);
          // Update the drawn features
          updateDrawnFeatures();
        } else {
          console.warn("Draw control not initialized");
        }
      } else if (message.type === "add_markers") {
        if (!window.mapboxglMarkers) {
          window.mapboxglMarkers = [];
        }
        message.markers.forEach(function (marker) {
          const markerOptions = {
            color: marker.color,
            rotation: marker.rotation,
            draggable: marker.options.draggable || false,
            ...marker.options,
          };
          const mapMarker = new mapboxgl.Marker(markerOptions)
            .setLngLat([marker.lng, marker.lat])
            .addTo(map);

          if (marker.popup) {
            mapMarker.setPopup(
              new mapboxgl.Popup({ offset: 25, maxWidth: '400px' }).setHTML(marker.popup),
            );
          }

          const markerId = marker.id;
          if (markerId) {
            const lngLat = mapMarker.getLngLat();
            Shiny.setInputValue(data.id + "_marker_" + markerId, {
              id: markerId,
              lng: lngLat.lng,
              lat: lngLat.lat,
            });

            mapMarker.on("dragend", function () {
              const lngLat = mapMarker.getLngLat();
              Shiny.setInputValue(data.id + "_marker_" + markerId, {
                id: markerId,
                lng: lngLat.lng,
                lat: lngLat.lat,
              });
            });
          }

          window.mapboxglMarkers.push(mapMarker);
        });
      } else if (message.type === "clear_markers") {
        if (window.mapboxglMarkers) {
          window.mapboxglMarkers.forEach(function (marker) {
            marker.remove();
          });
          window.mapboxglMarkers = [];
        }
      } else if (message.type === "add_fullscreen_control") {
        const position = message.position || "top-right";
        const fullscreen = new mapboxgl.FullscreenControl();
        map.addControl(fullscreen, position);
        map.controls.push(fullscreen);
      } else if (message.type === "add_scale_control") {
        const scaleControl = new mapboxgl.ScaleControl({
          maxWidth: message.options.maxWidth,
          unit: message.options.unit,
        });
        map.addControl(scaleControl, message.options.position);
        map.controls.push(scaleControl);
      } else if (message.type === "add_geolocate_control") {
        const geolocate = new mapboxgl.GeolocateControl({
          positionOptions: message.options.positionOptions,
          trackUserLocation: message.options.trackUserLocation,
          showAccuracyCircle: message.options.showAccuracyCircle,
          showUserLocation: message.options.showUserLocation,
          showUserHeading: message.options.showUserHeading,
          fitBoundsOptions: message.options.fitBoundsOptions,
        });
        map.addControl(geolocate, message.options.position);
        map.controls.push(geolocate);

        if (HTMLWidgets.shinyMode) {
          geolocate.on("geolocate", function (event) {
            Shiny.setInputValue(el.id + "_geolocate", {
              coords: event.coords,
              time: new Date(),
            });
          });

          geolocate.on("trackuserlocationstart", function () {
            Shiny.setInputValue(el.id + "_geolocate_tracking", {
              status: "start",
              time: new Date(),
            });
          });

          geolocate.on("trackuserlocationend", function () {
            Shiny.setInputValue(el.id + "_geolocate_tracking", {
              status: "end",
              time: new Date(),
            });
          });

          geolocate.on("error", function (error) {
            if (error.error.code === 1) {
              Shiny.setInputValue(el.id + "_geolocate_error", {
                message: "Location permission denied",
                time: new Date(),
              });
            }
          });
        }
      } else if (message.type === "add_geocoder_control") {
        const geocoderOptions = {
          accessToken: mapboxgl.accessToken,
          mapboxgl: mapboxgl,
          ...message.options,
        };

        // Set default values if not provided
        if (!geocoderOptions.placeholder)
          geocoderOptions.placeholder = "Search";
        if (typeof geocoderOptions.collapsed === "undefined")
          geocoderOptions.collapsed = false;

        const geocoder = new MapboxGeocoder(geocoderOptions);

        map.addControl(geocoder, message.position || "top-right");
        map.controls.push(geocoder);

        // Handle geocoder results in Shiny mode
        geocoder.on("result", function (e) {
          Shiny.setInputValue(data.id + "_geocoder", {
            result: e.result,
            time: new Date(),
          });
        });
      } else if (message.type === "add_layers_control") {
        const layersControl = document.createElement("div");
        layersControl.id = message.control_id;
        layersControl.className = message.collapsible
          ? "layers-control collapsible"
          : "layers-control";
        layersControl.style.position = "absolute";

        // Set the position correctly
        const position = message.position || "top-left";
        if (position === "top-left") {
          layersControl.style.top = (message.margin_top || 10) + "px";
          layersControl.style.left = (message.margin_left || 10) + "px";
        } else if (position === "top-right") {
          layersControl.style.top = (message.margin_top || 10) + "px";
          layersControl.style.right = (message.margin_right || 10) + "px";
        } else if (position === "bottom-left") {
          layersControl.style.bottom = (message.margin_bottom || 30) + "px";
          layersControl.style.left = (message.margin_left || 10) + "px";
        } else if (position === "bottom-right") {
          layersControl.style.bottom = (message.margin_bottom || 40) + "px";
          layersControl.style.right = (message.margin_right || 10) + "px";
        }

        // Apply custom colors if provided
        if (message.custom_colors) {
          const colors = message.custom_colors;

          // Create a style element for custom colors
          const styleEl = document.createElement("style");
          let css = "";

          if (colors.background) {
            css += `#${message.control_id} { background-color: ${colors.background}; }\n`;
          }

          if (colors.text) {
            css += `#${message.control_id} a { color: ${colors.text}; }\n`;
          }

          if (colors.active) {
            css += `#${message.control_id} a.active { background-color: ${colors.active}; }\n`;
            css += `#${message.control_id} .toggle-button { background-color: ${colors.active}; }\n`;
          }

          if (colors.activeText) {
            css += `#${message.control_id} a.active { color: ${colors.activeText}; }\n`;
            css += `#${message.control_id} .toggle-button { color: ${colors.activeText}; }\n`;
          }

          if (colors.hover) {
            css += `#${message.control_id} a:hover { background-color: ${colors.hover}; }\n`;
            css += `#${message.control_id} .toggle-button:hover { background-color: ${colors.hover}; }\n`;
          }

          styleEl.textContent = css;
          document.head.appendChild(styleEl);
        }

        const layersList = document.createElement("div");
        layersList.className = "layers-list";
        layersControl.appendChild(layersList);

        let layers = message.layers || [];
        let layersConfig = message.layers_config;

        // If we have a layers_config, use that; otherwise fall back to original behavior
        if (layersConfig && Array.isArray(layersConfig)) {
          layersConfig.forEach((config, index) => {
            const link = document.createElement("a");
            // Ensure config.ids is always an array
            const layerIds = Array.isArray(config.ids)
              ? config.ids
              : [config.ids];
            link.id = layerIds.join("-");
            link.href = "#";
            link.textContent = config.label;
            link.setAttribute("data-layer-ids", JSON.stringify(layerIds));
            link.setAttribute("data-layer-type", config.type);

            // Check if the first layer's visibility is set to "none" initially
            const firstLayerId = layerIds[0];
            const initialVisibility = map.getLayoutProperty(
              firstLayerId,
              "visibility",
            );
            link.className = initialVisibility === "none" ? "" : "active";

            // Also hide any associated legends if the layer is initially hidden
            if (initialVisibility === "none") {
              layerIds.forEach((layerId) => {
                const associatedLegends = document.querySelectorAll(
                  `.mapboxgl-legend[data-layer-id="${layerId}"]`,
                );
                associatedLegends.forEach((legend) => {
                  legend.style.display = "none";
                });
              });
            }

            // Show or hide layer(s) when the toggle is clicked
            link.onclick = function (e) {
              e.preventDefault();
              e.stopPropagation();

              const layerIds = JSON.parse(this.getAttribute("data-layer-ids"));
              const firstLayerId = layerIds[0];
              const visibility = map.getLayoutProperty(
                firstLayerId,
                "visibility",
              );

              // Toggle visibility for all layer IDs in the group
              if (visibility === "visible") {
                layerIds.forEach((layerId) => {
                  map.setLayoutProperty(layerId, "visibility", "none");
                  // Hide associated legends
                  const associatedLegends = document.querySelectorAll(
                    `.mapboxgl-legend[data-layer-id="${layerId}"]`,
                  );
                  associatedLegends.forEach((legend) => {
                    legend.style.display = "none";
                  });
                });
                this.className = "";
              } else {
                layerIds.forEach((layerId) => {
                  map.setLayoutProperty(layerId, "visibility", "visible");
                  // Show associated legends
                  const associatedLegends = document.querySelectorAll(
                    `.mapboxgl-legend[data-layer-id="${layerId}"]`,
                  );
                  associatedLegends.forEach((legend) => {
                    legend.style.display = "";
                  });
                });
                this.className = "active";
              }
            };

            layersList.appendChild(link);
          });
        } else {
          // Fallback to original behavior for simple layer arrays
          // Ensure layers is always an array
          if (!Array.isArray(layers)) {
            layers = [layers];
          }

          layers.forEach((layerId, index) => {
            const link = document.createElement("a");
            link.id = layerId;
            link.href = "#";
            link.textContent = layerId;

            // Check if the layer visibility is set to "none" initially
            const initialVisibility = map.getLayoutProperty(
              layerId,
              "visibility",
            );
            link.className = initialVisibility === "none" ? "" : "active";

            // Also hide any associated legends if the layer is initially hidden
            if (initialVisibility === "none") {
              const associatedLegends = document.querySelectorAll(
                `.mapboxgl-legend[data-layer-id="${layerId}"]`,
              );
              associatedLegends.forEach((legend) => {
                legend.style.display = "none";
              });
            }

            link.onclick = function (e) {
              const clickedLayer = this.textContent;
              e.preventDefault();
              e.stopPropagation();

              const visibility = map.getLayoutProperty(
                clickedLayer,
                "visibility",
              );

              if (visibility === "visible") {
                map.setLayoutProperty(clickedLayer, "visibility", "none");
                this.className = "";

                // Hide associated legends
                const associatedLegends = document.querySelectorAll(
                  `.mapboxgl-legend[data-layer-id="${clickedLayer}"]`,
                );
                associatedLegends.forEach((legend) => {
                  legend.style.display = "none";
                });
              } else {
                this.className = "active";
                map.setLayoutProperty(clickedLayer, "visibility", "visible");

                // Show associated legends
                const associatedLegends = document.querySelectorAll(
                  `.mapboxgl-legend[data-layer-id="${clickedLayer}"]`,
                );
                associatedLegends.forEach((legend) => {
                  legend.style.display = "";
                });
              }
            };

            layersList.appendChild(link);
          });
        }

        if (message.collapsible) {
          const toggleButton = document.createElement("div");
          toggleButton.className = "toggle-button";

          // Use stacked layers icon instead of text if requested
          if (message.use_icon) {
            // Add icon-only class to the control for compact styling
            layersControl.classList.add("icon-only");

            // More GIS-like layers stack icon
            toggleButton.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                            <polygon points="12 2 2 7 12 12 22 7 12 2"></polygon>
                            <polyline points="2 17 12 22 22 17"></polyline>
                            <polyline points="2 12 12 17 22 12"></polyline>
                        </svg>`;
            toggleButton.style.display = "flex";
            toggleButton.style.alignItems = "center";
            toggleButton.style.justifyContent = "center";
          } else {
            toggleButton.textContent = "Layers";
          }

          toggleButton.onclick = function () {
            layersControl.classList.toggle("open");
          };
          layersControl.insertBefore(toggleButton, layersList);
        }

        const mapContainer = document.getElementById(data.id);
        if (mapContainer) {
          mapContainer.appendChild(layersControl);
        } else {
          console.error(`Cannot find map container with ID ${data.id}`);
        }
      } else if (message.type === "clear_legend") {
        if (message.ids && Array.isArray(message.ids)) {
          message.ids.forEach((id) => {
            const legend = document.querySelector(
              `#${data.id} div[id="${id}"]`,
            );
            if (legend) {
              legend.remove();
            }
            // Remove from legend state
            delete layerState.legends[id];
          });
        } else if (message.ids) {
          const legend = document.querySelector(
            `#${data.id} div[id="${message.ids}"]`,
          );
          if (legend) {
            legend.remove();
          }
          // Remove from legend state
          delete layerState.legends[message.ids];
        } else {
          // Remove all legend elements
          const existingLegends = document.querySelectorAll(
            `#${data.id} .mapboxgl-legend`,
          );
          existingLegends.forEach((legend) => {
            legend.remove();
          });

          // Clean up any legend styles associated with this map
          const legendStyles = document.querySelectorAll(
            `style[data-mapgl-legend-css="${data.id}"]`,
          );
          legendStyles.forEach((style) => {
            style.remove();
          });

          // Clear all legend state
          layerState.legends = {};
        }
      } else if (message.type === "add_custom_control") {
        const controlOptions = message.options;
        const customControlContainer = document.createElement("div");

        if (controlOptions.className) {
          customControlContainer.className = controlOptions.className;
        } else {
          customControlContainer.className =
            "mapboxgl-ctrl mapboxgl-ctrl-group";
        }

        customControlContainer.innerHTML = controlOptions.html;

        const customControl = {
          onAdd: function () {
            return customControlContainer;
          },
          onRemove: function () {
            if (customControlContainer.parentNode) {
              customControlContainer.parentNode.removeChild(
                customControlContainer,
              );
            }
          },
        };

        map.addControl(customControl, controlOptions.position || "top-right");
        map.controls.push({ type: message.control_id, control: customControl });
      } else if (message.type === "clear_controls") {
        // If no specific controls specified, clear all
        if (!message.controls || message.controls.length === 0) {
          map.controls.forEach((controlObj) => {
            if (controlObj.control) {
              map.removeControl(controlObj.control);
            }
          });
          map.controls = [];

          const layersControl = document.querySelector(
            `#${data.id} .layers-control`,
          );
          if (layersControl) {
            layersControl.remove();
          }

          // Remove globe minimap if it exists
          const globeMinimap = document.querySelector(
            ".mapboxgl-ctrl-globe-minimap",
          );
          if (globeMinimap) {
            globeMinimap.remove();
          }
        } else {
          // Clear specific controls
          const controlsToRemove = Array.isArray(message.controls)
            ? message.controls
            : [message.controls];

          map.controls = map.controls.filter((controlObj) => {
            if (controlsToRemove.includes(controlObj.type)) {
              if (controlObj.control) {
                map.removeControl(controlObj.control);
              }
              return false; // Remove from array
            }
            return true; // Keep in array
          });

          // Handle special controls that aren't in the controls array
          controlsToRemove.forEach((controlType) => {
            if (controlType === "layers") {
              const layersControl = document.querySelector(
                `#${data.id} .layers-control`,
              );
              if (layersControl) {
                layersControl.remove();
              }
            } else if (controlType === "globe_minimap") {
              const globeMinimap = document.querySelector(
                ".mapboxgl-ctrl-globe-minimap",
              );
              if (globeMinimap) {
                globeMinimap.remove();
              }
            }
          });
        }
      } else if (message.type === "move_layer") {
        if (map.getLayer(message.layer)) {
          if (message.before) {
            map.moveLayer(message.layer, message.before);
          } else {
            map.moveLayer(message.layer);
          }
        } else {
          console.error("Layer not found:", message.layer);
        }
      } else if (message.type === "add_image") {
        if (Array.isArray(message.images)) {
          message.images.forEach(function (imageInfo) {
            map.loadImage(imageInfo.url, function (error, image) {
              if (error) {
                console.error("Error loading image:", error);
                return;
              }
              if (!map.hasImage(imageInfo.id)) {
                map.addImage(imageInfo.id, image, imageInfo.options);
              }
            });
          });
        } else if (message.url) {
          map.loadImage(message.url, function (error, image) {
            if (error) {
              console.error("Error loading image:", error);
              return;
            }
            if (!map.hasImage(message.imageId)) {
              map.addImage(message.imageId, image, message.options);
            }
          });
        } else {
          console.error("Invalid image data:", message);
        }
      } else if (message.type === "set_tooltip") {
        const layerId = message.layer;
        const newTooltipProperty = message.tooltip;

        // Track tooltip state
        layerState.tooltips[layerId] = newTooltipProperty;

        // If there's an active tooltip open, remove it first
        if (window._activeTooltip) {
          window._activeTooltip.remove();
          delete window._activeTooltip;
        }

        // Remove old handlers if any
        if (window._mapboxHandlers && window._mapboxHandlers[layerId]) {
          const handlers = window._mapboxHandlers[layerId];
          if (handlers.mousemove) {
            map.off("mousemove", layerId, handlers.mousemove);
          }
          if (handlers.mouseleave) {
            map.off("mouseleave", layerId, handlers.mouseleave);
          }
          delete window._mapboxHandlers[layerId];
        }

        // Create a new tooltip popup
        const tooltip = new mapboxgl.Popup({
          closeButton: false,
          closeOnClick: false,
          maxWidth: '400px',
        });

        // Define new handlers referencing the updated tooltip property
        const mouseMoveHandler = function (e) {
          onMouseMoveTooltip(e, map, tooltip, newTooltipProperty);
        };
        const mouseLeaveHandler = function () {
          onMouseLeaveTooltip(map, tooltip);
        };

        // Add the new event handlers
        map.on("mousemove", layerId, mouseMoveHandler);
        map.on("mouseleave", layerId, mouseLeaveHandler);

        // Store these handlers so we can remove/update them in the future
        if (!window._mapboxHandlers) {
          window._mapboxHandlers = {};
        }
        window._mapboxHandlers[layerId] = {
          mousemove: mouseMoveHandler,
          mouseleave: mouseLeaveHandler,
        };
      } else if (message.type === "set_popup") {
        const layerId = message.layer;
        const newPopupProperty = message.popup;

        // Track popup state
        layerState.popups[layerId] = newPopupProperty;

        // Remove any existing popup for this layer
        if (window._mapboxPopups && window._mapboxPopups[layerId]) {
          window._mapboxPopups[layerId].remove();
          delete window._mapboxPopups[layerId];
        }

        // Remove old click handler if any
        if (
          window._mapboxClickHandlers &&
          window._mapboxClickHandlers[layerId]
        ) {
          map.off("click", layerId, window._mapboxClickHandlers[layerId]);
          delete window._mapboxClickHandlers[layerId];
        }

        // Remove old hover handlers for cursor change
        map.off("mouseenter", layerId);
        map.off("mouseleave", layerId);

        // Create new click handler
        const clickHandler = function (e) {
          onClickPopup(e, map, newPopupProperty, layerId);
        };

        // Add the new event handler
        map.on("click", layerId, clickHandler);

        // Change cursor to pointer when hovering over the layer
        map.on("mouseenter", layerId, function () {
          map.getCanvas().style.cursor = "pointer";
        });

        // Change cursor back to default when leaving the layer
        map.on("mouseleave", layerId, function () {
          map.getCanvas().style.cursor = "";
        });

        // Store handler reference
        if (!window._mapboxClickHandlers) {
          window._mapboxClickHandlers = {};
        }
        window._mapboxClickHandlers[layerId] = clickHandler;
      } else if (message.type === "set_source") {
        const layerId = message.layer;
        const newData = message.source;
        const layerObject = map.getLayer(layerId);

        if (!layerObject) {
          console.error("Layer not found: ", layerId);
          return;
        }

        const sourceId = layerObject.source;
        const sourceObject = map.getSource(sourceId);

        if (!sourceObject) {
          console.error("Source not found: ", sourceId);
          return;
        }

        // Update the geojson data
        sourceObject.setData(newData);
      } else if (message.type === "set_rain") {
        if (message.remove) {
          map.setRain(null);
        } else if (message.rain) {
          map.setRain(message.rain);
        }
      } else if (message.type === "set_snow") {
        if (message.remove) {
          map.setSnow(null);
        } else if (message.snow) {
          map.setSnow(message.snow);
        }
      } else if (message.type === "set_projection") {
        const projection = message.projection;
        map.setProjection(projection);
      } else if (message.type === "add_globe_minimap") {
        const globeMinimapOptions = {
          globeSize: message.options.globe_size || 100,
          landColor: message.options.land_color || "#404040",
          waterColor: message.options.water_color || "#090909",
          markerColor: message.options.marker_color || "#1da1f2",
          markerSize: message.options.marker_size || 2,
        };
        const globeMinimap = new GlobeMinimap(globeMinimapOptions);
        map.addControl(globeMinimap, message.position || "bottom-left");
        map.controls.push(globeMinimap);
      }
    }
  });
}
