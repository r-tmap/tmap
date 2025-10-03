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

HTMLWidgets.widget({
  name: "mapboxgl_compare",

  type: "output",

  factory: function (el, width, height) {
    // Store maps and compare object to allow access during Shiny updates
    let beforeMap, afterMap, compareControl, draw;

    return {
      renderValue: function (x) {
        if (typeof mapboxgl === "undefined") {
          console.error("Mapbox GL JS is not loaded.");
          return;
        }
        if (typeof mapboxgl.Compare === "undefined") {
          console.error("Mapbox GL Compare plugin is not loaded.");
          return;
        }
        
        // Register PMTiles source type if available
        if (typeof MapboxPmTilesSource !== "undefined" && typeof pmtiles !== "undefined") {
          try {
            mapboxgl.Style.setSourceType(PMTILES_SOURCE_TYPE, MapboxPmTilesSource);
            console.log("PMTiles support enabled for Mapbox GL JS Compare");
          } catch (e) {
            console.warn("Failed to register PMTiles source type:", e);
          }
        }

        // Create container divs for the maps
        const beforeContainerId = `${el.id}-before`;
        const afterContainerId = `${el.id}-after`;

        // Different HTML structure based on mode
        if (x.mode === "sync") {
          // Side-by-side sync mode
          const containerStyle =
            x.orientation === "horizontal"
              ? `display: flex; flex-direction: column; width: 100%; height: 100%;`
              : `display: flex; flex-direction: row; width: 100%; height: 100%;`;

          const mapStyle =
            x.orientation === "horizontal"
              ? `width: 100%; height: 50%; position: relative;`
              : `width: 50%; height: 100%; position: relative;`;

          el.innerHTML = `
                      <div style="${containerStyle}">
                        <div id="${beforeContainerId}" class="map" style="${mapStyle}"></div>
                        <div id="${afterContainerId}" class="map" style="${mapStyle}"></div>
                      </div>
                    `;
        } else {
          // Default swipe mode
          el.innerHTML = `
                      <div id="${beforeContainerId}" class="map" style="width: 100%; height: 100%; position: absolute;"></div>
                      <div id="${afterContainerId}" class="map" style="width: 100%; height: 100%; position: absolute;"></div>
                    `;
        }

        beforeMap = new mapboxgl.Map({
          container: beforeContainerId,
          style: x.map1.style,
          center: x.map1.center,
          zoom: x.map1.zoom,
          bearing: x.map1.bearing,
          pitch: x.map1.pitch,
          projection: x.map1.projection,
          accessToken: x.map1.access_token,
          ...x.map1.additional_params,
        });

        afterMap = new mapboxgl.Map({
          container: afterContainerId,
          style: x.map2.style,
          center: x.map2.center,
          zoom: x.map2.zoom,
          bearing: x.map2.bearing,
          pitch: x.map2.pitch,
          projection: x.map2.projection,
          accessToken: x.map2.access_token,
          ...x.map2.additional_params,
        });

        // Set the global access token
        mapboxgl.accessToken = x.map1.access_token;

        if (x.mode === "swipe") {
          // Only create the swiper in swipe mode
          compareControl = new mapboxgl.Compare(
            beforeMap,
            afterMap,
            `#${el.id}`,
            {
              mousemove: x.mousemove,
              orientation: x.orientation,
            },
          );

          // Apply custom swiper color if provided
          if (x.swiper_color) {
            const swiperSelector =
              x.orientation === "vertical"
                ? ".mapboxgl-compare .compare-swiper-vertical"
                : ".mapboxgl-compare .compare-swiper-horizontal";

            const styleEl = document.createElement("style");
            styleEl.innerHTML = `${swiperSelector} { background-color: ${x.swiper_color}; }`;
            document.head.appendChild(styleEl);
          }
        } else {
          // For sync mode, we directly leverage the sync-move module's approach

          // Function to synchronize maps as seen in the mapbox-gl-sync-move module
          const syncMaps = () => {
            // Array of maps to sync
            const maps = [beforeMap, afterMap];
            // Array of move event handlers
            const moveHandlers = [];

            // Setup the sync between maps
            maps.forEach((map, index) => {
              // Create a handler for each map that syncs all other maps
              moveHandlers[index] = (e) => {
                // Disable all move events temporarily
                maps.forEach((m, i) => {
                  m.off("move", moveHandlers[i]);
                });

                // Get the state from the map that triggered the event
                const center = map.getCenter();
                const zoom = map.getZoom();
                const bearing = map.getBearing();
                const pitch = map.getPitch();

                // Apply this state to all other maps
                maps
                  .filter((m, i) => i !== index)
                  .forEach((m) => {
                    m.jumpTo({
                      center: center,
                      zoom: zoom,
                      bearing: bearing,
                      pitch: pitch,
                    });
                  });

                // Re-enable move events
                maps.forEach((m, i) => {
                  m.on("move", moveHandlers[i]);
                });
              };

              // Add the move handler to each map
              map.on("move", moveHandlers[index]);
            });
          };

          // Initialize the sync
          syncMaps();
        }

        // Ensure both maps resize correctly
        beforeMap.on("load", function () {
          beforeMap.resize();
          applyMapModifications(beforeMap, x.map1);

          // Setup Shiny event handlers for the before map
          if (HTMLWidgets.shinyMode) {
            setupShinyEvents(beforeMap, el.id, "before");
          }
        });

        afterMap.on("load", function () {
          afterMap.resize();
          applyMapModifications(afterMap, x.map2);

          // Setup Shiny event handlers for the after map
          if (HTMLWidgets.shinyMode) {
            setupShinyEvents(afterMap, el.id, "after");
          }
          
          // Add compare-level legends after both maps are loaded
          if (x.compare_legends && Array.isArray(x.compare_legends)) {
            x.compare_legends.forEach(function(legendInfo) {
              // Add CSS
              const legendCss = document.createElement("style");
              legendCss.innerHTML = legendInfo.css;
              legendCss.setAttribute("data-mapgl-legend-css", el.id);
              document.head.appendChild(legendCss);
              
              // Create legend element
              const legend = document.createElement("div");
              legend.innerHTML = legendInfo.html;
              legend.classList.add("mapboxgl-legend");
              
              // Append to the appropriate container based on target
              if (legendInfo.target === "compare") {
                // Append to the main compare container
                el.appendChild(legend);
              } else if (legendInfo.target === "before") {
                // Append to the before map container
                beforeMap.getContainer().appendChild(legend);
              } else if (legendInfo.target === "after") {
                // Append to the after map container
                afterMap.getContainer().appendChild(legend);
              }
            });
          }
        });

        // Handle Shiny messages
        if (HTMLWidgets.shinyMode) {
          Shiny.addCustomMessageHandler(
            "mapboxgl-compare-proxy",
            function (data) {
              if (data.id !== el.id) return;

              // Get the message and determine which map to target
              var message = data.message;
              var map = message.map === "before" ? beforeMap : afterMap;

              if (!map) return;

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

              // Process the message based on type
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
                      onClickPopup(
                        e,
                        map,
                        message.layer.popup,
                        message.layer.id,
                      );
                    };

                    // Store these handler references so we can remove them later if needed
                    if (!window._mapboxClickHandlers) {
                      window._mapboxClickHandlers = {};
                    }
                    window._mapboxClickHandlers[message.layer.id] =
                      clickHandler;

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
                      onMouseMoveTooltip(
                        e,
                        map,
                        tooltip,
                        message.layer.tooltip,
                      );
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
                            featureState.sourceLayer =
                              message.layer.source_layer;
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
                  console.error(
                    "Failed to add layer via proxy: ",
                    message.layer,
                    e,
                  );
                }
              } else if (message.type === "remove_layer") {
                // If there's an active tooltip, remove it first
                if (window._activeTooltip) {
                  window._activeTooltip.remove();
                  delete window._activeTooltip;
                }

                // If there's an active popup for this layer, remove it
                if (
                  window._mapboxPopups &&
                  window._mapboxPopups[message.layer_id]
                ) {
                  window._mapboxPopups[message.layer_id].remove();
                  delete window._mapboxPopups[message.layer_id];
                }

                if (map.getLayer(message.layer_id)) {
                  // Remove tooltip handlers
                  if (
                    window._mapboxHandlers &&
                    window._mapboxHandlers[message.layer_id]
                  ) {
                    const handlers = window._mapboxHandlers[message.layer_id];
                    if (handlers.mousemove) {
                      map.off(
                        "mousemove",
                        message.layer_id,
                        handlers.mousemove,
                      );
                    }
                    if (handlers.mouseleave) {
                      map.off(
                        "mouseleave",
                        message.layer_id,
                        handlers.mouseleave,
                      );
                    }
                    // Clean up the reference
                    delete window._mapboxHandlers[message.layer_id];
                  }

                  // Remove click handlers for popups
                  if (
                    window._mapboxClickHandlers &&
                    window._mapboxClickHandlers[message.layer_id]
                  ) {
                    map.off(
                      "click",
                      message.layer_id,
                      window._mapboxClickHandlers[message.layer_id],
                    );
                    delete window._mapboxClickHandlers[message.layer_id];
                  }

                  // Remove the layer
                  map.removeLayer(message.layer_id);
                }
                if (map.getSource(message.layer_id)) {
                  map.removeSource(message.layer_id);
                }

                // Clean up tracked layer state
                const mapId = map.getContainer().id;
                if (window._mapglLayerState && window._mapglLayerState[mapId]) {
                  const layerState = window._mapglLayerState[mapId];
                  delete layerState.filters[message.layer_id];
                  delete layerState.paintProperties[message.layer_id];
                  delete layerState.layoutProperties[message.layer_id];
                  delete layerState.tooltips[message.layer_id];
                  delete layerState.popups[message.layer_id];
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
                map.setLayoutProperty(
                  message.layer,
                  message.name,
                  message.value,
                );
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
              } else if (message.type === "add_legend") {
                if (!message.add) {
                  const existingLegends = document.querySelectorAll(
                    `#${data.id} .mapboxgl-legend`,
                  );
                  existingLegends.forEach((legend) => legend.remove());

                  // Clean up any existing legend styles that might have been added
                  const legendStyles = document.querySelectorAll(
                    `style[data-mapgl-legend-css="${data.id}"]`,
                  );
                  legendStyles.forEach((style) => style.remove());
                }

                const legendCss = document.createElement("style");
                legendCss.innerHTML = message.legend_css;
                legendCss.setAttribute("data-mapgl-legend-css", data.id); // Mark this style for later cleanup
                document.head.appendChild(legendCss);

                const legend = document.createElement("div");
                legend.innerHTML = message.html;
                legend.classList.add("mapboxgl-legend");
                
                // Append legend to the correct map container
                const targetContainer = map.getContainer();
                targetContainer.appendChild(legend);
              } else if (message.type === "set_config_property") {
                map.setConfigProperty(
                  message.importId,
                  message.configName,
                  message.value,
                );
              } else if (message.type === "set_style") {
                // Save the current view state
                const center = map.getCenter();
                const zoom = map.getZoom();
                const bearing = map.getBearing();
                const pitch = map.getPitch();

                // Apply the new style
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
                          map.setFilter(
                            layerId,
                            savedLayerState.filters[layerId],
                          );
                        }
                      }

                      // Restore paint properties
                      for (const layerId in savedLayerState.paintProperties) {
                        if (map.getLayer(layerId)) {
                          const properties =
                            savedLayerState.paintProperties[layerId];
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
                              map.setPaintProperty(
                                layerId,
                                propertyName,
                                savedValue,
                              );
                            }
                          }
                        }
                      }

                      // Restore layout properties
                      for (const layerId in savedLayerState.layoutProperties) {
                        if (map.getLayer(layerId)) {
                          const properties =
                            savedLayerState.layoutProperties[layerId];
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
                          const tooltipProperty =
                            savedLayerState.tooltips[layerId];

                          // Remove existing tooltip handlers first
                          map.off("mousemove", layerId);
                          map.off("mouseleave", layerId);

                          const tooltip = new mapboxgl.Popup({
                            closeButton: false,
                            closeOnClick: false,
                            maxWidth: '400px',
                          });

                          map.on("mousemove", layerId, function (e) {
                            onMouseMoveTooltip(
                              e,
                              map,
                              tooltip,
                              tooltipProperty,
                            );
                          });

                          map.on("mouseleave", layerId, function () {
                            onMouseLeaveTooltip(map, tooltip);
                          });
                        }
                      }

                      // Restore popups
                      for (const layerId in savedLayerState.popups) {
                        if (map.getLayer(layerId)) {
                          const popupProperty = savedLayerState.popups[layerId];

                          // Remove existing popup handlers first
                          if (
                            window._mapboxClickHandlers &&
                            window._mapboxClickHandlers[layerId]
                          ) {
                            map.off(
                              "click",
                              layerId,
                              window._mapboxClickHandlers[layerId],
                            );
                            delete window._mapboxClickHandlers[layerId];
                          }

                          const clickHandler = function (e) {
                            onClickPopup(e, map, popupProperty, layerId);
                          };

                          map.on("click", layerId, clickHandler);

                          if (!window._mapboxClickHandlers) {
                            window._mapboxClickHandlers = {};
                          }
                          window._mapboxClickHandlers[layerId] = clickHandler;
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

                // Restore the view state after the style has loaded
                map.once("style.load", function () {
                  map.jumpTo({
                    center: center,
                    zoom: zoom,
                    bearing: bearing,
                    pitch: pitch,
                  });

                  // Re-apply map modifications
                  if (map === beforeMap) {
                    applyMapModifications(map, x.map1);
                  } else {
                    applyMapModifications(map, x.map2);
                  }
                });
              } else if (message.type === "add_navigation_control") {
                const nav = new mapboxgl.NavigationControl({
                  showCompass: message.options.show_compass,
                  showZoom: message.options.show_zoom,
                  visualizePitch: message.options.visualize_pitch,
                });
                map.addControl(nav, message.position);

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

                // Initialize controls array if it doesn't exist
                if (!map.controls) {
                  map.controls = [];
                }
                map.controls.push({ type: "navigation", control: nav });
              } else if (message.type === "add_custom_control") {
                const controlOptions = message.options;
                const customControlContainer = document.createElement("div");
                customControlContainer.innerHTML = controlOptions.html;
                customControlContainer.className = "mapboxgl-ctrl";
                if (controlOptions.className) {
                  customControlContainer.className += " " + controlOptions.className;
                }

                // Create the custom control object
                const customControl = {
                  onAdd: function(map) {
                    return customControlContainer;
                  },
                  onRemove: function() {
                    if (customControlContainer.parentNode) {
                      customControlContainer.parentNode.removeChild(customControlContainer);
                    }
                  }
                };

                map.addControl(customControl, message.position);

                // Initialize controls array if it doesn't exist
                if (!map.controls) {
                  map.controls = [];
                }

                // Store control with proper type
                map.controls.push({ type: message.control_id, control: customControl });
              } else if (message.type === "add_reset_control") {
                const resetControl = document.createElement("button");
                resetControl.className =
                  "mapboxgl-ctrl-icon mapboxgl-ctrl-reset";
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

                const resetControlObj = {
                  onAdd: function () {
                    return resetContainer;
                  },
                  onRemove: function () {
                    resetContainer.parentNode.removeChild(resetContainer);
                  },
                };

                map.addControl(resetControlObj, message.position);

                // Initialize controls array if it doesn't exist
                if (!map.controls) {
                  map.controls = [];
                }
                map.controls.push({ type: "reset", control: resetControlObj });
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

                const drawControl = new MapboxDraw(drawOptions);
                map.addControl(drawControl, message.position);

                // Initialize controls array if it doesn't exist
                if (!map.controls) {
                  map.controls = [];
                }
                map.controls.push({ type: "draw", control: drawControl });

                // Store draw control on map for feature click detection
                map._mapgl_draw = drawControl;

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
                    const polygonButton = map.getContainer().querySelector('.mapbox-gl-draw_polygon');
                    if (polygonButton) {
                      polygonButton.title = 'Freehand polygon tool (p)';
                    }
                  }, 100);
                }

                // Add initial features if provided
                if (message.source) {
                  addSourceFeaturesToDraw(draw, message.source, map);
                }

                // Add event listeners
                map.on("draw.create", updateDrawnFeatures);
                map.on("draw.delete", updateDrawnFeatures);
                map.on("draw.update", updateDrawnFeatures);

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
                  if (!document.querySelector('#mapgl-draw-download-styles')) {
                    const style = document.createElement('style');
                    style.id = 'mapgl-draw-download-styles';
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
                    const drawButtons = map.getContainer().querySelector('.mapboxgl-ctrl-group:has(.mapbox-gl-draw_polygon)');
                    
                    if (drawButtons) {
                      // Create download button
                      const downloadBtn = document.createElement('button');
                      downloadBtn.className = 'mapbox-gl-draw_download';
                      downloadBtn.title = 'Download drawn features as GeoJSON';
                      
                      // Add SVG download icon
                      downloadBtn.innerHTML = `
                        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
                          <path d="M19 9h-4V3H9v6H5l7 7 7-7zM5 18v2h14v-2H5z"/>
                        </svg>
                      `;
                      
                      downloadBtn.addEventListener('click', () => {
                        // Get all drawn features
                        const data = map._mapgl_draw ? map._mapgl_draw.getAll() : null;
                        
                        if (data.features.length === 0) {
                          alert('No features to download. Please draw something first!');
                          return;
                        }
                        
                        // Convert to string with nice formatting
                        const dataStr = JSON.stringify(data, null, 2);
                        
                        // Create blob and download
                        const blob = new Blob([dataStr], { type: 'application/json' });
                        const url = URL.createObjectURL(blob);
                        
                        const a = document.createElement('a');
                        a.href = url;
                        a.download = `${message.download_filename || 'drawn-features'}.geojson`;
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
                if (map._mapgl_draw) {
                  const features = map._mapgl_draw ? map._mapgl_draw.getAll() : null;
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
                if (draw) {
                  if (map._mapgl_draw) map._mapgl_draw.deleteAll();
                  // Update the drawn features
                  updateDrawnFeatures();
                }
              } else if (message.type === "add_features_to_draw") {
                if (draw) {
                  if (message.data.clear_existing) {
                    if (map._mapgl_draw) map._mapgl_draw.deleteAll();
                  }
                  addSourceFeaturesToDraw(draw, message.data.source, map);
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
                      new mapboxgl.Popup({
                        offset: 25,
                        maxWidth: '400px',
                      }).setHTML(marker.popup),
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

                // Initialize controls array if it doesn't exist
                if (!map.controls) {
                  map.controls = [];
                }
                map.controls.push({ type: "fullscreen", control: fullscreen });
              } else if (message.type === "add_scale_control") {
                const scaleControl = new mapboxgl.ScaleControl({
                  maxWidth: message.options.maxWidth,
                  unit: message.options.unit,
                });
                map.addControl(scaleControl, message.options.position);

                // Initialize controls array if it doesn't exist
                if (!map.controls) {
                  map.controls = [];
                }
                map.controls.push({ type: "scale", control: scaleControl });
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

                if (HTMLWidgets.shinyMode) {
                  geolocate.on("geolocate", function (event) {
                    Shiny.setInputValue(data.id + "_geolocate", {
                      coords: event.coords,
                      time: new Date(),
                    });
                  });

                  geolocate.on("trackuserlocationstart", function () {
                    Shiny.setInputValue(data.id + "_geolocate_tracking", {
                      status: "start",
                      time: new Date(),
                    });
                  });

                  geolocate.on("trackuserlocationend", function () {
                    Shiny.setInputValue(data.id + "_geolocate_tracking", {
                      status: "end",
                      time: new Date(),
                    });
                  });

                  geolocate.on("error", function (error) {
                    if (error.error.code === 1) {
                      Shiny.setInputValue(data.id + "_geolocate_error", {
                        message: "Location permission denied",
                        time: new Date(),
                      });
                    }
                  });
                }

                // Initialize controls array if it doesn't exist
                if (!map.controls) {
                  map.controls = [];
                }
                map.controls.push({ type: "geolocate", control: geolocate });
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

                // Initialize controls array if it doesn't exist
                if (!map.controls) {
                  map.controls = [];
                }
                map.controls.push({ type: "geocoder", control: geocoder });

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

                // Handle use_icon parameter
                let className = message.collapsible
                  ? "layers-control collapsible"
                  : "layers-control";

                if (message.use_icon) {
                  className += " icon-only";
                }

                layersControl.className = className;
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
                    css += `.layers-control { background-color: ${colors.background} !important; }`;
                  }
                  if (colors.text) {
                    css += `.layers-control a { color: ${colors.text} !important; }`;
                  }
                  if (colors.activeBackground) {
                    css += `.layers-control a.active { background-color: ${colors.activeBackground} !important; }`;
                  }
                  if (colors.activeText) {
                    css += `.layers-control a.active { color: ${colors.activeText} !important; }`;
                  }
                  if (colors.hoverBackground) {
                    css += `.layers-control a:hover { background-color: ${colors.hoverBackground} !important; }`;
                  }
                  if (colors.hoverText) {
                    css += `.layers-control a:hover { color: ${colors.hoverText} !important; }`;
                  }
                  if (colors.toggleButtonBackground) {
                    css += `.layers-control .toggle-button { background-color: ${colors.toggleButtonBackground}
                  !important; }`;
                  }
                  if (colors.toggleButtonText) {
                    css += `.layers-control .toggle-button { color: ${colors.toggleButtonText} !important; }`;
                  }

                  styleEl.innerHTML = css;
                  document.head.appendChild(styleEl);
                }

                document.getElementById(data.id).appendChild(layersControl);

                const layersList = document.createElement("div");
                layersList.className = "layers-list";
                layersControl.appendChild(layersList);

                // Fetch layers to be included in the control
                let layers =
                  message.layers ||
                  map.getStyle().layers.map((layer) => layer.id);

                layers.forEach((layerId, index) => {
                  const link = document.createElement("a");
                  link.id = layerId;
                  link.href = "#";
                  link.textContent = layerId;
                  link.className = "active";

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
                    } else {
                      this.className = "active";
                      map.setLayoutProperty(
                        clickedLayer,
                        "visibility",
                        "visible",
                      );
                    }
                  };

                  layersList.appendChild(link);
                });

                // Handle collapsible behavior
                if (message.collapsible) {
                  const toggleButton = document.createElement("div");
                  toggleButton.className = "toggle-button";
                  toggleButton.textContent = "Layers";
                  toggleButton.onclick = function () {
                    layersControl.classList.toggle("open");
                  };
                  layersControl.insertBefore(toggleButton, layersList);
                }

                // Initialize controls array if it doesn't exist
                if (!map.controls) {
                  map.controls = [];
                }
                map.controls.push({ type: "layers", control: layersControl });
              } else if (message.type === "add_globe_minimap") {
                // Add the globe minimap control
                const minimap = new MapboxGlobeMinimap({
                  center: map.getCenter(),
                  zoom: map.getZoom(),
                  bearing: map.getBearing(),
                  pitch: map.getPitch(),
                  globeSize: message.globe_size,
                  landColor: message.land_color,
                  waterColor: message.water_color,
                  markerColor: message.marker_color,
                  markerSize: message.marker_size,
                });

                map.addControl(minimap, message.position);

                // Initialize controls array if it doesn't exist
                if (!map.controls) {
                  map.controls = [];
                }
                map.controls.push({ type: "globe_minimap", control: minimap });
              } else if (message.type === "set_rain") {
                if (message.rain) {
                  map.setRain(message.rain);
                } else {
                  map.setRain(null);
                }
              } else if (message.type === "set_snow") {
                if (message.snow) {
                  map.setSnow(message.snow);
                } else {
                  map.setSnow(null);
                }
              } else if (message.type === "set_projection") {
                map.setProjection(message.projection);
              } else if (message.type === "set_source") {
                if (map.getLayer(message.layer)) {
                  const sourceId = map.getLayer(message.layer).source;
                  map.getSource(sourceId).setData(JSON.parse(message.source));
                }
              } else if (message.type === "set_tooltip") {
                // Track tooltip state
                layerState.tooltips[message.layer] = message.tooltip;

                if (map.getLayer(message.layer)) {
                  // Remove any existing tooltip handlers
                  map.off("mousemove", message.layer);
                  map.off("mouseleave", message.layer);

                  const tooltip = new mapboxgl.Popup({
                    closeButton: false,
                    closeOnClick: false,
                    maxWidth: '400px',
                  });

                  map.on("mousemove", message.layer, function (e) {
                    map.getCanvas().style.cursor = "pointer";
                    if (e.features.length > 0) {
                      const description =
                        e.features[0].properties[message.tooltip];
                      tooltip
                        .setLngLat(e.lngLat)
                        .setHTML(description)
                        .addTo(map);
                    }
                  });

                  map.on("mouseleave", message.layer, function () {
                    map.getCanvas().style.cursor = "";
                    tooltip.remove();
                  });
                }
              } else if (message.type === "set_popup") {
                // Track popup state
                layerState.popups[message.layer] = message.popup;

                if (map.getLayer(message.layer)) {
                  // Remove any existing popup click handlers for this layer
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

                  // Remove any existing popup for this layer
                  if (
                    window._mapboxPopups &&
                    window._mapboxPopups[message.layer]
                  ) {
                    window._mapboxPopups[message.layer].remove();
                    delete window._mapboxPopups[message.layer];
                  }

                  // Create new click handler for popup
                  const clickHandler = function (e) {
                    onClickPopup(e, map, message.popup, message.layer);
                  };

                  // Store handler reference
                  if (!window._mapboxClickHandlers) {
                    window._mapboxClickHandlers = {};
                  }
                  window._mapboxClickHandlers[message.layer] = clickHandler;

                  // Add click handler
                  map.on("click", message.layer, clickHandler);

                  // Change cursor to pointer when hovering over the layer
                  map.on("mouseenter", message.layer, function () {
                    map.getCanvas().style.cursor = "pointer";
                  });

                  // Change cursor back to default when leaving the layer
                  map.on("mouseleave", message.layer, function () {
                    map.getCanvas().style.cursor = "";
                  });
                }
              } else if (message.type === "move_layer") {
                if (map.getLayer(message.layer)) {
                  if (message.before) {
                    map.moveLayer(message.layer, message.before);
                  } else {
                    map.moveLayer(message.layer);
                  }
                }
              } else if (message.type === "set_opacity") {
                // Set opacity for all fill layers
                const style = map.getStyle();
                if (style && style.layers) {
                  style.layers.forEach(function (layer) {
                    if (layer.type === "fill" && map.getLayer(layer.id)) {
                      map.setPaintProperty(
                        layer.id,
                        "fill-opacity",
                        message.opacity,
                      );
                    }
                  });
                }
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
                  JSON.stringify(featureCollection)
                );
              } else if (message.type === "clear_controls") {
                // Handle clear_controls for compare widgets
                if (!message.controls || message.controls.length === 0) {
                  // Clear all controls
                  map.controls.forEach((controlObj) => {
                    if (controlObj.control) {
                      map.removeControl(controlObj.control);
                    }
                  });
                  map.controls = [];
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
                }
              }
            },
          );
        }

        function setupShinyEvents(map, parentId, mapType) {
          // Set view state on move end
          map.on("moveend", function () {
            const center = map.getCenter();
            const zoom = map.getZoom();
            const bearing = map.getBearing();
            const pitch = map.getPitch();

            if (window.Shiny) {
              Shiny.setInputValue(parentId + "_" + mapType + "_view", {
                center: [center.lng, center.lat],
                zoom: zoom,
                bearing: bearing,
                pitch: pitch,
              });
            }
          });

          // Send clicked point coordinates to Shiny
          map.on("click", function (e) {
            // Check if this map's draw control is active and in a drawing mode
            let isDrawing = false;
            if (map._mapgl_draw && map._mapgl_draw.getMode) {
              const mode = map._mapgl_draw.getMode();
              isDrawing = mode === 'draw_point' ||
                         mode === 'draw_line_string' ||
                         mode === 'draw_polygon';
            }

            // Only process feature clicks if not actively drawing
            if (!isDrawing) {
              const features = map.queryRenderedFeatures(e.point);
              // Filter out draw layers
              const nonDrawFeatures = features.filter(feature =>
                !feature.layer.id.includes('gl-draw') &&
                !feature.source.includes('gl-draw')
              );

              if (nonDrawFeatures.length > 0) {
                const feature = nonDrawFeatures[0];
                if (window.Shiny) {
                  Shiny.setInputValue(parentId + "_" + mapType + "_feature_click", {
                    id: feature.id,
                    properties: feature.properties,
                    layer: feature.layer.id,
                    lng: e.lngLat.lng,
                    lat: e.lngLat.lat,
                    time: Date.now(),
                  });
                }
              } else {
                if (window.Shiny) {
                  Shiny.setInputValue(parentId + "_" + mapType + "_feature_click", null);
                }
              }
            }

            // Always send regular click event
            if (window.Shiny) {
              Shiny.setInputValue(parentId + "_" + mapType + "_click", {
                lng: e.lngLat.lng,
                lat: e.lngLat.lat,
                time: Date.now(),
              });
            }
          });

          // Add hover events if enabled for this map
          const mapConfig = (mapType === "before") ? x.map1 : x.map2;
          if (mapConfig.hover_events && mapConfig.hover_events.enabled) {
            map.on("mousemove", function (e) {
              if (window.Shiny) {
                // Feature hover events
                if (mapConfig.hover_events.features) {
                  const options = mapConfig.hover_events.layer_id
                    ? { layers: Array.isArray(mapConfig.hover_events.layer_id) 
                        ? mapConfig.hover_events.layer_id 
                        : mapConfig.hover_events.layer_id.split(',').map(id => id.trim()) }
                    : undefined;
                  const features = map.queryRenderedFeatures(e.point, options);

                  if(features.length > 0) {
                    const feature = features[0];
                    Shiny.setInputValue(
                      parentId + "_" + mapType + "_feature_hover",
                      {
                        id: feature.id,
                        properties: feature.properties,
                        layer: feature.layer.id,
                        lng: e.lngLat.lng,
                        lat: e.lngLat.lat,
                        time: new Date(),
                      }
                    );
                  } else {
                    Shiny.setInputValue(
                      parentId + "_" + mapType + "_feature_hover",
                      null
                    );
                  }
                }

                // Coordinate hover events
                if (mapConfig.hover_events.coordinates) {
                  Shiny.setInputValue(
                    parentId + "_" + mapType + "_hover",
                    {
                      lng: e.lngLat.lng,
                      lat: e.lngLat.lat,
                      time: new Date(),
                    }
                  );
                }
              }
            });
          }
        }

        function applyMapModifications(map, mapData) {
          // Initialize controls array if it doesn't exist
          if (!map.controls) {
            map.controls = [];
          }
          // Note: tooltip handlers are already defined at the top of the file

          // Set config properties if provided
          if (mapData.config_properties) {
            mapData.config_properties.forEach(function (config) {
              map.setConfigProperty(
                config.importId,
                config.configName,
                config.value,
              );
            });
          }

          // Process H3J sources if provided
          if (mapData.h3j_sources) {
            mapData.h3j_sources.forEach(async function (source) {
              await map.addH3JSource(source.id, {
                data: source.url,
              });
            });
          }

          if (mapData.markers) {
            if (!window.mapboxglMarkers) {
              window.mapboxglMarkers = [];
            }
            mapData.markers.forEach(function (marker) {
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
                  new mapboxgl.Popup({ offset: 25, maxWidth: '400px' }).setText(marker.popup),
                );
              }

              const markerId = marker.id;
              if (markerId) {
                const lngLat = mapMarker.getLngLat();
                if (HTMLWidgets.shinyMode) {
                  Shiny.setInputValue(el.id + "_marker_" + markerId, {
                    id: markerId,
                    lng: lngLat.lng,
                    lat: lngLat.lat,
                  });
                }

                mapMarker.on("dragend", function () {
                  const lngLat = mapMarker.getLngLat();
                  if (HTMLWidgets.shinyMode) {
                    Shiny.setInputValue(el.id + "_marker_" + markerId, {
                      id: markerId,
                      lng: lngLat.lng,
                      lat: lngLat.lat,
                    });
                  }
                });
              }

              window.mapboxglMarkers.push(mapMarker);
            });
          }

          // Add sources if provided
          if (mapData.sources) {
            mapData.sources.forEach(function (source) {
              if (source.type === "vector") {
                const sourceConfig = {
                  type: "vector",
                };
                // Add url or tiles
                if (source.url) {
                  sourceConfig.url = source.url;
                }
                if (source.tiles) {
                  sourceConfig.tiles = source.tiles;
                }
                if (source.promoteId) {
                  sourceConfig.promoteId = source.promoteId;
                }
                map.addSource(source.id, sourceConfig);
              } else if (source.type === "geojson") {
                const geojsonData = source.data;
                map.addSource(source.id, {
                  type: "geojson",
                  data: geojsonData,
                  generateId: true,
                });
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
              }
            });
          }

          // Add layers if provided
          if (mapData.layers) {
            mapData.layers.forEach(function (layer) {
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

                if (layer.before_id) {
                  map.addLayer(layerConfig, layer.before_id);
                } else {
                  map.addLayer(layerConfig);
                }

                // Add popups or tooltips if provided
                if (layer.popup) {
                  map.on("click", layer.id, function (e) {
                    const description = e.features[0].properties[layer.popup];

                    new mapboxgl.Popup({ maxWidth: '400px' })
                      .setLngLat(e.lngLat)
                      .setHTML(description)
                      .addTo(map);
                  });
                }

                if (layer.tooltip) {
                  const tooltip = new mapboxgl.Popup({
                    closeButton: false,
                    closeOnClick: false,
                    maxWidth: '400px',
                  });

                  // Create a reference to the mousemove handler function
                  const mouseMoveHandler = function (e) {
                    onMouseMoveTooltip(e, map, tooltip, layer.tooltip);
                  };

                  // Create a reference to the mouseleave handler function
                  const mouseLeaveHandler = function () {
                    onMouseLeaveTooltip(map, tooltip);
                  };

                  // Attach the named handler references
                  map.on("mousemove", layer.id, mouseMoveHandler);
                  map.on("mouseleave", layer.id, mouseLeaveHandler);

                  // Store these handler references
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
                        map.setFeatureState(
                          {
                            source:
                              typeof layer.source === "string"
                                ? layer.source
                                : layer.id,
                            id: hoveredFeatureId,
                          },
                          { hover: false },
                        );
                      }
                      hoveredFeatureId = e.features[0].id;
                      map.setFeatureState(
                        {
                          source:
                            typeof layer.source === "string"
                              ? layer.source
                              : layer.id,
                          id: hoveredFeatureId,
                        },
                        { hover: true },
                      );
                    }
                  });

                  map.on("mouseleave", layer.id, function () {
                    if (hoveredFeatureId !== null) {
                      map.setFeatureState(
                        {
                          source:
                            typeof layer.source === "string"
                              ? layer.source
                              : layer.id,
                          id: hoveredFeatureId,
                        },
                        { hover: false },
                      );
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

          // Set terrain if provided
          if (mapData.terrain) {
            map.setTerrain({
              source: mapData.terrain.source,
              exaggeration: mapData.terrain.exaggeration,
            });
          }

          // Set fog
          if (mapData.fog) {
            map.setFog(mapData.fog);
          }

          // Set rain effect if provided
          if (mapData.rain) {
            map.setRain(mapData.rain);
          }

          // Set snow effect if provided
          if (mapData.snow) {
            map.setSnow(mapData.snow);
          }

          if (mapData.fitBounds) {
            map.fitBounds(mapData.fitBounds.bounds, mapData.fitBounds.options);
          }
          if (mapData.flyTo) {
            map.flyTo(mapData.flyTo);
          }
          if (mapData.easeTo) {
            map.easeTo(mapData.easeTo);
          }
          if (mapData.setCenter) {
            map.setCenter(mapData.setCenter);
          }
          if (mapData.setZoom) {
            map.setZoom(mapData.setZoom);
          }

          // Apply moveLayer operations if provided
          if (mapData.moveLayer) {
            mapData.moveLayer.forEach(function (moveOp) {
              if (map.getLayer(moveOp.layer)) {
                if (moveOp.before) {
                  map.moveLayer(moveOp.layer, moveOp.before);
                } else {
                  map.moveLayer(moveOp.layer);
                }
              }
            });
          }

          if (mapData.jumpTo) {
            map.jumpTo(mapData.jumpTo);
          }

          // Add custom images if provided
          if (mapData.images && Array.isArray(mapData.images)) {
            mapData.images.forEach(async function (imageInfo) {
              try {
                const image = await map.loadImage(imageInfo.url);
                if (!map.hasImage(imageInfo.id)) {
                  map.addImage(imageInfo.id, image.data, imageInfo.options);
                }
              } catch (error) {
                console.error("Error loading image:", error);
              }
            });
          } else if (mapData.images) {
            console.error("mapData.images is not an array:", mapData.images);
          }

          // Remove existing legends only from this specific map container
          const mapContainer = map.getContainer();
          const existingLegends = mapContainer.querySelectorAll(".mapboxgl-legend");
          existingLegends.forEach((legend) => legend.remove());

          // Don't remove all legend styles globally - they might belong to other maps
          // Only remove styles when the entire widget is being recreated

          if (mapData.legend_html && mapData.legend_css) {
            const legendCss = document.createElement("style");
            legendCss.innerHTML = mapData.legend_css;
            legendCss.setAttribute("data-mapgl-legend-css", el.id); // Mark this style for later cleanup
            document.head.appendChild(legendCss);

            const legend = document.createElement("div");
            legend.innerHTML = mapData.legend_html;
            legend.classList.add("mapboxgl-legend");
            
            // Append legend to the correct map container instead of main container
            const mapContainer = map.getContainer();
            mapContainer.appendChild(legend);
          }

          // Add fullscreen control if enabled
          if (
            mapData.fullscreen_control &&
            mapData.fullscreen_control.enabled
          ) {
            const position = mapData.fullscreen_control.position || "top-right";
            map.addControl(new mapboxgl.FullscreenControl(), position);
          }

          // Add navigation control if enabled
          if (mapData.navigation_control) {
            const nav = new mapboxgl.NavigationControl({
              showCompass: mapData.navigation_control.show_compass,
              showZoom: mapData.navigation_control.show_zoom,
              visualizePitch: mapData.navigation_control.visualize_pitch,
            });
            map.addControl(nav, mapData.navigation_control.position);
          }

          // Add scale control if enabled
          if (mapData.scale_control) {
            const scaleControl = new mapboxgl.ScaleControl({
              maxWidth: mapData.scale_control.maxWidth,
              unit: mapData.scale_control.unit,
            });
            map.addControl(scaleControl, mapData.scale_control.position);
            map.controls.push(scaleControl);
          }

          // Add geolocate control if enabled
          if (mapData.geolocate_control) {
            const geolocate = new mapboxgl.GeolocateControl({
              positionOptions: mapData.geolocate_control.positionOptions,
              trackUserLocation: mapData.geolocate_control.trackUserLocation,
              showAccuracyCircle: mapData.geolocate_control.showAccuracyCircle,
              showUserLocation: mapData.geolocate_control.showUserLocation,
              showUserHeading: mapData.geolocate_control.showUserHeading,
              fitBoundsOptions: mapData.geolocate_control.fitBoundsOptions,
            });
            map.addControl(geolocate, mapData.geolocate_control.position);

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
                filter: [
                  "all",
                  ["==", "$type", "Point"],
                  ["==", "meta", "midpoint"],
                ],
                paint: {
                  "circle-radius": 3,
                  "circle-color": styling.active_color,
                },
              },
              // Vertex point halos
              {
                id: "gl-draw-vertex-halo-active",
                type: "circle",
                filter: [
                  "all",
                  ["==", "meta", "vertex"],
                  ["==", "$type", "Point"],
                ],
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
                filter: [
                  "all",
                  ["==", "meta", "vertex"],
                  ["==", "$type", "Point"],
                ],
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

          // Add geocoder control if enabled
          if (mapData.geocoder_control) {
            const geocoderOptions = {
              accessToken: mapboxgl.accessToken,
              mapboxgl: mapboxgl,
              ...mapData.geocoder_control,
            };

            // Set default values if not provided
            if (!geocoderOptions.placeholder)
              geocoderOptions.placeholder = "Search";
            if (typeof geocoderOptions.collapsed === "undefined")
              geocoderOptions.collapsed = false;

            const geocoder = new MapboxGeocoder(geocoderOptions);

            map.addControl(
              geocoder,
              mapData.geocoder_control.position || "top-right",
            );
            map.controls.push(geocoder);

            // Handle geocoder results in Shiny mode
            geocoder.on("result", function (e) {
              Shiny.setInputValue(data.id + "_geocoder", {
                result: e.result,
                time: new Date(),
              });
            });
          }

          // Add draw control if enabled
          if (mapData.draw_control) {
            if (mapData.draw_control && mapData.draw_control.enabled) {
              let drawOptions = mapData.draw_control.options || {};

              // Generate styles if styling parameters provided
              if (mapData.draw_control.styling) {
                const generatedStyles = generateDrawStyles(
                  mapData.draw_control.styling,
                );
                if (generatedStyles) {
                  drawOptions.styles = generatedStyles;
                }
              }

              if (mapData.draw_control.freehand) {
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
                            mapData.draw_control.simplify_freehand;
                          return state;
                        },
                      },
                    ),
                  }),
                  // defaultMode: 'draw_polygon' # Don't set the default yet
                });
              }

              // Add rectangle mode if enabled
              if (mapData.draw_control.rectangle) {
                if (!drawOptions.modes) {
                  drawOptions.modes = Object.assign({}, MapboxDraw.modes);
                }
                drawOptions.modes.draw_rectangle = MapboxDraw.modes.draw_rectangle;
              }

              // Add radius mode if enabled
              if (mapData.draw_control.radius) {
                if (!drawOptions.modes) {
                  drawOptions.modes = Object.assign({}, MapboxDraw.modes);
                }
                drawOptions.modes.draw_radius = MapboxDraw.modes.draw_radius;
              }

              const drawControl = new MapboxDraw(drawOptions);
              map.addControl(drawControl, mapData.draw_control.position);
              map.controls.push({ type: "draw", control: drawControl });

              // Store draw control on map for feature click detection
              map._mapgl_draw = drawControl;

              // Add custom mode buttons and styling
              setTimeout(() => {
                const drawControlGroup = map.getContainer().querySelector(".mapboxgl-ctrl-group");

                // Add rectangle styling and button
                if (mapData.draw_control.rectangle) {
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

                  if (drawControlGroup) {
                    const rectangleBtn = document.createElement("button");
                    rectangleBtn.className = "mapbox-gl-draw_rectangle";
                    rectangleBtn.title = "Rectangle tool";
                    rectangleBtn.type = "button";
                    rectangleBtn.onclick = function() {
                      drawControl.changeMode('draw_rectangle');
                      drawControlGroup.querySelectorAll('button').forEach(btn => btn.classList.remove('active'));
                      rectangleBtn.classList.add('active');
                    };
                    drawControlGroup.appendChild(rectangleBtn);
                  }
                }

                // Add radius styling and button
                if (mapData.draw_control.radius) {
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

                  if (drawControlGroup) {
                    const radiusBtn = document.createElement("button");
                    radiusBtn.className = "mapbox-gl-draw_radius";
                    radiusBtn.title = "Radius/Circle tool";
                    radiusBtn.type = "button";
                    radiusBtn.onclick = function() {
                      drawControl.changeMode('draw_radius');
                      drawControlGroup.querySelectorAll('button').forEach(btn => btn.classList.remove('active'));
                      radiusBtn.classList.add('active');
                    };
                    drawControlGroup.appendChild(radiusBtn);
                  }
                }
              }, 100);

              // Add initial features if provided
              if (mapData.draw_control.source) {
                addSourceFeaturesToDraw(drawControl, mapData.draw_control.source, map);
              }

              // Process any queued features
              if (mapData.draw_features_queue) {
                mapData.draw_features_queue.forEach(function (data) {
                  if (data.clear_existing) {
                    if (map._mapgl_draw) map._mapgl_draw.deleteAll();
                  }
                  addSourceFeaturesToDraw(draw, data.source, map);
                });
              }

              // Apply orientation styling
              if (mapData.draw_control.orientation === "horizontal") {
                const drawBar = map
                  .getContainer()
                  .querySelector(".mapboxgl-ctrl-group");
                if (drawBar) {
                  drawBar.style.display = "flex";
                  drawBar.style.flexDirection = "row";
                }
              }

              // Add download button if requested
              if (mapData.draw_control.download_button) {
                // Add CSS for download button if not already added
                if (!document.querySelector('#mapgl-draw-download-styles')) {
                  const style = document.createElement('style');
                  style.id = 'mapgl-draw-download-styles';
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
                  const drawButtons = map.getContainer().querySelector('.mapboxgl-ctrl-group:has(.mapbox-gl-draw_polygon)');
                  
                  if (drawButtons) {
                    // Create download button
                    const downloadBtn = document.createElement('button');
                    downloadBtn.className = 'mapbox-gl-draw_download';
                    downloadBtn.title = 'Download drawn features as GeoJSON';
                    
                    // Add SVG download icon
                    downloadBtn.innerHTML = `
                      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
                        <path d="M19 9h-4V3H9v6H5l7 7 7-7zM5 18v2h14v-2H5z"/>
                      </svg>
                    `;
                    
                    downloadBtn.addEventListener('click', () => {
                      // Get all drawn features
                      const data = map._mapgl_draw ? map._mapgl_draw.getAll() : null;
                      
                      if (data.features.length === 0) {
                        alert('No features to download. Please draw something first!');
                        return;
                      }
                      
                      // Convert to string with nice formatting
                      const dataStr = JSON.stringify(data, null, 2);
                      
                      // Create blob and download
                      const blob = new Blob([dataStr], { type: 'application/json' });
                      const url = URL.createObjectURL(blob);
                      
                      const a = document.createElement('a');
                      a.href = url;
                      a.download = `${mapData.draw_control.download_filename}.geojson`;
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

            // Helper function for updating drawn features
            function updateDrawnFeatures() {
              if (HTMLWidgets.shinyMode && map._mapgl_draw) {
                const features = map._mapgl_draw ? map._mapgl_draw.getAll() : null;
                Shiny.setInputValue(
                  el.id + "_drawn_features",
                  JSON.stringify(features),
                );
              }
            }

            // Add event listeners
            map.on("draw.create", updateDrawnFeatures);
            map.on("draw.delete", updateDrawnFeatures);
            map.on("draw.update", updateDrawnFeatures);
          }

          // Add reset control if enabled
          if (mapData.reset_control) {
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
              animate: mapData.reset_control.animate,
            };

            if (mapData.reset_control.duration) {
              initialView.duration = mapData.reset_control.duration;
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
              mapData.reset_control.position,
            );
          }

          // Add the layers control if provided
          if (mapData.layers_control) {
            const layersControl = document.createElement("div");
            layersControl.id = mapData.layers_control.control_id;

            // Handle use_icon parameter
            let className = mapData.layers_control.collapsible
              ? "layers-control collapsible"
              : "layers-control";

            layersControl.className = className;
            layersControl.style.position = "absolute";

            // Set the position correctly - fix position bug by using correct CSS positioning
            const position = mapData.layers_control.position || "top-left";
            if (position === "top-left") {
              layersControl.style.top = (mapData.layers_control.margin_top || 10) + "px";
              layersControl.style.left = (mapData.layers_control.margin_left || 10) + "px";
            } else if (position === "top-right") {
              layersControl.style.top = (mapData.layers_control.margin_top || 10) + "px";
              layersControl.style.right = (mapData.layers_control.margin_right || 10) + "px";
            } else if (position === "bottom-left") {
              layersControl.style.bottom = (mapData.layers_control.margin_bottom || 30) + "px";
              layersControl.style.left = (mapData.layers_control.margin_left || 10) + "px";
            } else if (position === "bottom-right") {
              layersControl.style.bottom = (mapData.layers_control.margin_bottom || 40) + "px";
              layersControl.style.right = (mapData.layers_control.margin_right || 10) + "px";
            }

            el.appendChild(layersControl);

            const layersList = document.createElement("div");
            layersList.className = "layers-list";
            layersControl.appendChild(layersList);

            // Fetch layers to be included in the control
            let layers =
              mapData.layers_control.layers ||
              map.getStyle().layers.map((layer) => layer.id);
            let layersConfig = mapData.layers_control.layers_config;

            // If we have a layers_config, use that; otherwise fall back to original behavior
            if (layersConfig && Array.isArray(layersConfig)) {
              layersConfig.forEach((config, index) => {
                const link = document.createElement("a");
                // Ensure config.ids is always an array
                const layerIds = Array.isArray(config.ids) ? config.ids : [config.ids];
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
                    layerIds.forEach(layerId => {
                      map.setLayoutProperty(layerId, "visibility", "none");
                    });
                    this.className = "";
                  } else {
                    layerIds.forEach(layerId => {
                      map.setLayoutProperty(layerId, "visibility", "visible");
                    });
                    this.className = "active";
                  }
                };

                layersList.appendChild(link);
              });
            } else {
              // Fallback to original behavior for simple layer arrays
              layers.forEach((layerId, index) => {
                const link = document.createElement("a");
                link.id = layerId;
                link.href = "#";
                link.textContent = layerId;
                link.className = "active";

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
                  } else {
                    this.className = "active";
                    map.setLayoutProperty(clickedLayer, "visibility", "visible");
                  }
                };

                layersList.appendChild(link);
              });
            }

            // Handle collapsible behavior
            if (mapData.layers_control.collapsible) {
              const toggleButton = document.createElement("div");
              toggleButton.className = "toggle-button";

              if (mapData.layers_control.use_icon) {
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
        }
      },

      resize: function (width, height) {
        // Code to handle resizing if necessary
      },
    };
  },
});
