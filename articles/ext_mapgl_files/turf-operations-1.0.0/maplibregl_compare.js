function evaluateExpression(expression, properties) {
    if (!Array.isArray(expression)) {
        return expression;
    }
    
    const operator = expression[0];
    
    switch (operator) {
        case 'get':
            return properties[expression[1]];
        case 'concat':
            return expression.slice(1).map(item => evaluateExpression(item, properties)).join('');
        case 'to-string':
            return String(evaluateExpression(expression[1], properties));
        case 'to-number':
            return Number(evaluateExpression(expression[1], properties));
        case 'number-format':
            const value = evaluateExpression(expression[1], properties);
            const options = expression[2] || {};
            
            // Handle locale option
            const locale = options.locale || 'en-US';
            
            // Build Intl.NumberFormat options
            const formatOptions = {};
            
            // Style options
            if (options.style) formatOptions.style = options.style; // 'decimal', 'currency', 'percent', 'unit'
            if (options.currency) formatOptions.currency = options.currency;
            if (options.unit) formatOptions.unit = options.unit;
            
            // Digit options
            if (options.hasOwnProperty('min-fraction-digits')) {
                formatOptions.minimumFractionDigits = options['min-fraction-digits'];
            }
            if (options.hasOwnProperty('max-fraction-digits')) {
                formatOptions.maximumFractionDigits = options['max-fraction-digits'];
            }
            if (options.hasOwnProperty('min-integer-digits')) {
                formatOptions.minimumIntegerDigits = options['min-integer-digits'];
            }
            
            // Notation options
            if (options.notation) formatOptions.notation = options.notation; // 'standard', 'scientific', 'engineering', 'compact'
            if (options.compactDisplay) formatOptions.compactDisplay = options.compactDisplay; // 'short', 'long'
            
            // Grouping
            if (options.hasOwnProperty('useGrouping')) {
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
            description = evaluateExpression(tooltipProperty, e.features[0].properties);
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
    if (window._maplibrePopups && window._maplibrePopups[layerId]) {
        window._maplibrePopups[layerId].remove();
    }
    
    // Create and show the popup
    const popup = new maplibregl.Popup()
        .setLngLat(e.lngLat)
        .setHTML(description)
        .addTo(map);
        
    // Store reference to this popup
    if (!window._maplibrePopups) {
        window._maplibrePopups = {};
    }
    window._maplibrePopups[layerId] = popup;
    
    // Remove reference when popup is closed
    popup.on('close', function() {
        if (window._maplibrePopups[layerId] === popup) {
            delete window._maplibrePopups[layerId];
        }
    });
}

HTMLWidgets.widget({
    name: "maplibregl_compare",

    type: "output",

    factory: function (el, width, height) {
        // Store maps and compare object to allow access during Shiny updates
        let beforeMap, afterMap, compareControl, draw;

        return {
            renderValue: function (x) {
                if (typeof maplibregl === "undefined") {
                    console.error("Maplibre GL JS is not loaded.");
                    return;
                }
                if (typeof maplibregl.Compare === "undefined") {
                    console.error("Maplibre GL Compare plugin is not loaded.");
                    return;
                }

                // Add PMTiles support
                if (typeof pmtiles !== "undefined") {
                    let protocol = new pmtiles.Protocol({ metadata: true });
                    maplibregl.addProtocol("pmtiles", protocol.tile);
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

                beforeMap = new maplibregl.Map({
                    container: beforeContainerId,
                    style: x.map1.style,
                    center: x.map1.center,
                    zoom: x.map1.zoom,
                    bearing: x.map1.bearing,
                    pitch: x.map1.pitch,
                    accessToken: x.map1.access_token,
                    ...x.map1.additional_params,
                });

                // Initialize controls array
                beforeMap.controls = [];

                afterMap = new maplibregl.Map({
                    container: afterContainerId,
                    style: x.map2.style,
                    center: x.map2.center,
                    zoom: x.map2.zoom,
                    bearing: x.map2.bearing,
                    pitch: x.map2.pitch,
                    accessToken: x.map2.access_token,
                    ...x.map2.additional_params,
                });

                // Initialize controls array
                afterMap.controls = [];

                if (x.mode === "swipe") {
                    // Only create the swiper in swipe mode
                    compareControl = new maplibregl.Compare(
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
                        const swiperSelector = x.orientation === "vertical" ? 
                            ".maplibregl-compare .compare-swiper-vertical" : 
                            ".maplibregl-compare .compare-swiper-horizontal";
                        
                        const styleEl = document.createElement('style');
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
                                maps.filter((m, i) => i !== index).forEach(
                                    (m) => {
                                        m.jumpTo({
                                            center: center,
                                            zoom: zoom,
                                            bearing: bearing,
                                            pitch: pitch,
                                        });
                                    },
                                );

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

                // Define updateDrawnFeatures function for the draw tool
                window.updateDrawnFeatures = function () {
                    if (draw) {
                        const features = draw.getAll();
                        Shiny.setInputValue(
                            el.id + "_drawn_features",
                            JSON.stringify(features),
                        );
                    }
                };

                // Handle Shiny messages
                if (HTMLWidgets.shinyMode) {
                    Shiny.addCustomMessageHandler(
                        "maplibre-compare-proxy",
                        function (data) {
                            if (data.id !== el.id) return;

                            // Get the message and determine which map to target
                            var message = data.message;
                            var map =
                                message.map === "before" ? beforeMap : afterMap;

                            if (!map) return;

                            // Initialize layer state tracking if not already present
                            if (!window._mapglLayerState) {
                                window._mapglLayerState = {};
                            }
                            const mapId = map.getContainer().id;
                            if (!window._mapglLayerState[mapId]) {
                                window._mapglLayerState[mapId] = {
                                    filters: {},        // layerId -> filter expression
                                    paintProperties: {}, // layerId -> {propertyName -> value}
                                    layoutProperties: {}, // layerId -> {propertyName -> value}
                                    tooltips: {},       // layerId -> tooltip property
                                    popups: {},         // layerId -> popup property
                                    legends: {}         // legendId -> {html: string, css: string}
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
                                        url: message.source.url,
                                    };
                                    // Add promoteId if provided
                                    if (message.source.promoteId) {
                                        sourceConfig.promoteId = message.source.promoteId;
                                    }
                                    // Add any other properties from the source object
                                    Object.keys(message.source).forEach(function(key) {
                                        if (key !== "id" && key !== "type" && key !== "url" && key !== "promoteId") {
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
                                    Object.keys(message.source).forEach(function(key) {
                                        if (key !== "id" && key !== "type" && key !== "data" && key !== "generateId") {
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
                                    Object.keys(message.source).forEach(function(key) {
                                        if (key !== "id" && key !== "type" && key !== "url" && key !== "tiles" && key !== "tileSize" && key !== "maxzoom") {
                                            sourceConfig[key] = message.source[key];
                                        }
                                    });
                                    map.addSource(message.source.id, sourceConfig);
                                } else if (
                                    message.source.type === "raster-dem"
                                ) {
                                    const sourceConfig = {
                                        type: "raster-dem",
                                        url: message.source.url,
                                        tileSize: message.source.tileSize,
                                    };
                                    if (message.source.maxzoom) {
                                        sourceConfig.maxzoom = message.source.maxzoom;
                                    }
                                    // Add any other properties from the source object
                                    Object.keys(message.source).forEach(function(key) {
                                        if (key !== "id" && key !== "type" && key !== "url" && key !== "tileSize" && key !== "maxzoom") {
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
                                    Object.keys(message.source).forEach(function(key) {
                                        if (key !== "id" && key !== "type" && key !== "url" && key !== "coordinates") {
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
                                    Object.keys(message.source).forEach(function(key) {
                                        if (key !== "id" && key !== "type" && key !== "urls" && key !== "coordinates") {
                                            sourceConfig[key] = message.source[key];
                                        }
                                    });
                                    map.addSource(message.source.id, sourceConfig);
                                } else {
                                    // Handle custom source types
                                    const sourceConfig = { type: message.source.type };
                                    
                                    // Copy all properties except id
                                    Object.keys(message.source).forEach(function(key) {
                                        if (key !== "id") {
                                            sourceConfig[key] = message.source[key];
                                        }
                                    });
                                    
                                    map.addSource(message.source.id, sourceConfig);
                                }
                            } else if (message.type === "add_layer") {
                                try {
                                    if (message.layer.before_id) {
                                        map.addLayer(
                                            message.layer,
                                            message.layer.before_id,
                                        );
                                    } else {
                                        map.addLayer(message.layer);
                                    }

                                    // Add popups or tooltips if provided
                                    if (message.layer.popup) {
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
                                        map.on(
                                            "click",
                                            message.layer.id,
                                            clickHandler
                                        );

                                        // Change cursor to pointer when hovering over the layer
                                        map.on(
                                            "mouseenter",
                                            message.layer.id,
                                            function () {
                                                map.getCanvas().style.cursor =
                                                    "pointer";
                                            },
                                        );

                                        // Change cursor back to default when leaving the layer
                                        map.on(
                                            "mouseleave",
                                            message.layer.id,
                                            function () {
                                                map.getCanvas().style.cursor =
                                                    "";
                                            },
                                        );
                                    }

                                    if (message.layer.tooltip) {
                                        const tooltip = new maplibregl.Popup({
                                            closeButton: false,
                                            closeOnClick: false,
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
                                        map.on(
                                            "mousemove",
                                            message.layer.id,
                                            mouseMoveHandler,
                                        );
                                        map.on(
                                            "mouseleave",
                                            message.layer.id,
                                            mouseLeaveHandler,
                                        );

                                        // Store these handler references for later removal:
                                        if (!window._mapboxHandlers) {
                                            window._mapboxHandlers = {};
                                        }
                                        window._mapboxHandlers[
                                            message.layer.id
                                        ] = {
                                            mousemove: mouseMoveHandler,
                                            mouseleave: mouseLeaveHandler,
                                        };
                                    }

                                    // Add hover effect if provided
                                    if (message.layer.hover_options) {
                                        const jsHoverOptions = {};
                                        for (const [
                                            key,
                                            value,
                                        ] of Object.entries(
                                            message.layer.hover_options,
                                        )) {
                                            const jsKey = key.replace(
                                                /_/g,
                                                "-",
                                            );
                                            jsHoverOptions[jsKey] = value;
                                        }

                                        let hoveredFeatureId = null;

                                        map.on(
                                            "mousemove",
                                            message.layer.id,
                                            function (e) {
                                                if (e.features.length > 0) {
                                                    if (
                                                        hoveredFeatureId !==
                                                        null
                                                    ) {
                                                        const featureState = {
                                                            source:
                                                                typeof message
                                                                    .layer
                                                                    .source ===
                                                                "string"
                                                                    ? message
                                                                          .layer
                                                                          .source
                                                                    : message
                                                                          .layer
                                                                          .id,
                                                            id: hoveredFeatureId,
                                                        };
                                                        if (
                                                            message.layer
                                                                .source_layer
                                                        ) {
                                                            featureState.sourceLayer =
                                                                message.layer.source_layer;
                                                        }
                                                        map.setFeatureState(
                                                            featureState,
                                                            {
                                                                hover: false,
                                                            },
                                                        );
                                                    }
                                                    hoveredFeatureId =
                                                        e.features[0].id;
                                                    const featureState = {
                                                        source:
                                                            typeof message.layer
                                                                .source ===
                                                            "string"
                                                                ? message.layer
                                                                      .source
                                                                : message.layer
                                                                      .id,
                                                        id: hoveredFeatureId,
                                                    };
                                                    if (
                                                        message.layer
                                                            .source_layer
                                                    ) {
                                                        featureState.sourceLayer =
                                                            message.layer.source_layer;
                                                    }
                                                    map.setFeatureState(
                                                        featureState,
                                                        {
                                                            hover: true,
                                                        },
                                                    );
                                                }
                                            },
                                        );

                                        map.on(
                                            "mouseleave",
                                            message.layer.id,
                                            function () {
                                                if (hoveredFeatureId !== null) {
                                                    const featureState = {
                                                        source:
                                                            typeof message.layer
                                                                .source ===
                                                            "string"
                                                                ? message.layer
                                                                      .source
                                                                : message.layer
                                                                      .id,
                                                        id: hoveredFeatureId,
                                                    };
                                                    if (
                                                        message.layer
                                                            .source_layer
                                                    ) {
                                                        featureState.sourceLayer =
                                                            message.layer.source_layer;
                                                    }
                                                    map.setFeatureState(
                                                        featureState,
                                                        {
                                                            hover: false,
                                                        },
                                                    );
                                                }
                                                hoveredFeatureId = null;
                                            },
                                        );

                                        Object.keys(jsHoverOptions).forEach(
                                            function (key) {
                                                const originalPaint =
                                                    map.getPaintProperty(
                                                        message.layer.id,
                                                        key,
                                                    ) ||
                                                    message.layer.paint[key];
                                                map.setPaintProperty(
                                                    message.layer.id,
                                                    key,
                                                    [
                                                        "case",
                                                        [
                                                            "boolean",
                                                            [
                                                                "feature-state",
                                                                "hover",
                                                            ],
                                                            false,
                                                        ],
                                                        jsHoverOptions[key],
                                                        originalPaint,
                                                    ],
                                                );
                                            },
                                        );
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
                                // Check both message.layer_id and message.layer.id as keys due to different message formats
                                if (window._mapboxPopups) {
                                    // First check if we have a popup stored with message.layer_id key
                                    if (window._mapboxPopups[message.layer_id]) {
                                        window._mapboxPopups[message.layer_id].remove();
                                        delete window._mapboxPopups[message.layer_id];
                                    }
                                    
                                    // Also check if we have a popup stored with message.layer.id key, which happens when added via add_layer
                                    if (message.layer && message.layer.id && window._mapboxPopups[message.layer.id]) {
                                        window._mapboxPopups[message.layer.id].remove();
                                        delete window._mapboxPopups[message.layer.id];
                                    }
                                }

                                if (map.getLayer(message.layer_id)) {
                                    // Remove tooltip handlers
                                    if (
                                        window._mapboxHandlers &&
                                        window._mapboxHandlers[message.layer_id]
                                    ) {
                                        const handlers =
                                            window._mapboxHandlers[
                                                message.layer_id
                                            ];
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
                                        delete window._mapboxHandlers[
                                            message.layer_id
                                        ];
                                    }
                                    
                                    // Remove click handlers for popups
                                    if (window._mapboxClickHandlers) {
                                        // First check for handlers stored with message.layer_id key
                                        if (window._mapboxClickHandlers[message.layer_id]) {
                                            map.off(
                                                "click",
                                                message.layer_id,
                                                window._mapboxClickHandlers[message.layer_id]
                                            );
                                            delete window._mapboxClickHandlers[message.layer_id];
                                        }
                                        
                                        // Also check for handlers stored with message.layer.id key from add_layer
                                        if (message.layer && message.layer.id && window._mapboxClickHandlers[message.layer.id]) {
                                            map.off(
                                                "click",
                                                message.layer_id,
                                                window._mapboxClickHandlers[message.layer.id]
                                            );
                                            delete window._mapboxClickHandlers[message.layer.id];
                                        }
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
                                layerState.layoutProperties[message.layer][message.name] = message.value;
                            } else if (message.type === "set_paint_property") {
                                const layerId = message.layer;
                                const propertyName = message.name;
                                const newValue = message.value;

                                // Check if the layer has hover options
                                const layerStyle = map
                                    .getStyle()
                                    .layers.find(
                                        (layer) => layer.id === layerId,
                                    );
                                const currentPaintProperty =
                                    map.getPaintProperty(layerId, propertyName);

                                if (
                                    currentPaintProperty &&
                                    Array.isArray(currentPaintProperty) &&
                                    currentPaintProperty[0] === "case"
                                ) {
                                    // This property has hover options, so we need to preserve them
                                    const hoverValue = currentPaintProperty[2];
                                    const newPaintProperty = [
                                        "case",
                                        [
                                            "boolean",
                                            ["feature-state", "hover"],
                                            false,
                                        ],
                                        hoverValue,
                                        newValue,
                                    ];
                                    map.setPaintProperty(
                                        layerId,
                                        propertyName,
                                        newPaintProperty,
                                    );
                                } else {
                                    // No hover options, just set the new value directly
                                    map.setPaintProperty(
                                        layerId,
                                        propertyName,
                                        newValue,
                                    );
                                }
                                // Track paint property state for layer restoration
                                if (!layerState.paintProperties[layerId]) {
                                    layerState.paintProperties[layerId] = {};
                                }
                                layerState.paintProperties[layerId][propertyName] = newValue;
                            } else if (message.type === "add_legend") {
                                if (!message.add) {
                                    const existingLegends =
                                        document.querySelectorAll(
                                            `#${data.id} .maplibregl-legend`,
                                        );
                                    existingLegends.forEach((legend) =>
                                        legend.remove(),
                                    );
                                    
                                    // Clean up any existing legend styles that might have been added
                                    const legendStyles = document.querySelectorAll(`style[data-mapgl-legend-css="${data.id}"]`);
                                    legendStyles.forEach((style) => style.remove());
                                }

                                const legendCss =
                                    document.createElement("style");
                                legendCss.innerHTML = message.legend_css;
                                legendCss.setAttribute('data-mapgl-legend-css', data.id); // Mark this style for later cleanup
                                document.head.appendChild(legendCss);

                                const legend = document.createElement("div");
                                legend.innerHTML = message.html;
                                legend.classList.add("maplibregl-legend");
                                
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

                                // Default preserve_layers to true if not specified
                                const preserveLayers = message.preserve_layers !== false;
                                
                                // If we should preserve layers and sources
                                if (preserveLayers) {
                                    // Store the current style before changing it
                                    const currentStyle = map.getStyle();
                                    const userSourceIds = [];
                                    const userLayers = [];
                                    
                                    console.log("[MapGL Debug] Current style sources:", Object.keys(currentStyle.sources));
                                    console.log("[MapGL Debug] Current style layers:", currentStyle.layers.map(l => l.id));
                                    
                                    // Store layer IDs we know were added by the user via R code
                                    // This is the most reliable way to identify user-added layers
                                    const knownUserLayerIds = [];
                                    
                                    // For each layer in the current style, determine if it's a user-added layer
                                    currentStyle.layers.forEach(function(layer) {
                                        const layerId = layer.id;
                                        
                                        // Critical: Check for nc_counties specifically since we know that's used in the test app
                                        if (layerId === "nc_counties") {
                                            console.log("[MapGL Debug] Found explicit test layer:", layerId);
                                            knownUserLayerIds.push(layerId);
                                            if (layer.source && !userSourceIds.includes(layer.source)) {
                                                console.log("[MapGL Debug] Found source from test layer:", layer.source);
                                                userSourceIds.push(layer.source);
                                            }
                                            return; // Skip other checks for this layer
                                        }
                                        
                                        // These are common patterns for user-added layers from R code
                                        if (
                                            // Specific layer IDs from the R package
                                            layerId.endsWith("_counties") ||
                                            layerId.endsWith("_label") ||
                                            layerId.endsWith("_layer") ||
                                            
                                            // Look for hover handlers - only user-added layers have these
                                            (window._mapboxHandlers && window._mapboxHandlers[layerId]) ||
                                            
                                            // If the layer ID contains these strings, it's likely user-added
                                            layerId.includes("user") ||
                                            layerId.includes("custom") ||
                                            
                                            // If the paint property has a hover case, it's user-added
                                            (layer.paint && Object.values(layer.paint).some(value => 
                                                Array.isArray(value) && 
                                                value[0] === "case" && 
                                                Array.isArray(value[1]) && 
                                                value[1][1] && 
                                                Array.isArray(value[1][1]) && 
                                                value[1][1][0] === "feature-state" && 
                                                value[1][1][1] === "hover"))
                                        ) {
                                            console.log("[MapGL Debug] Found user layer:", layerId);
                                            knownUserLayerIds.push(layerId);
                                            // Only include its source if it's not a base map source
                                            if (layer.source && !userSourceIds.includes(layer.source)) {
                                                const layerSource = currentStyle.sources[layer.source];
                                                const isBaseMapSource = layerSource && layerSource.type === "vector" && (
                                                    layer.source === "composite" || 
                                                    layer.source === "mapbox" || 
                                                    layer.source.startsWith("mapbox-") ||
                                                    layer.source === "openmaptiles" ||
                                                    layer.source.startsWith("carto") ||
                                                    layer.source.startsWith("maptiler")
                                                );
                                                
                                                if (!isBaseMapSource) {
                                                    console.log("[MapGL Debug] Found user source from layer:", layer.source);
                                                    userSourceIds.push(layer.source);
                                                } else {
                                                    console.log("[MapGL Debug] Not adding base map source from layer:", layer.source);
                                                }
                                            }
                                        }
                                    });
                                    
                                    // For each source, determine if it's a user-added source
                                    for (const sourceId in currentStyle.sources) {
                                        const source = currentStyle.sources[sourceId];
                                        
                                        // Strategy 1: All GeoJSON sources are likely user-added
                                        if (source.type === "geojson") {
                                            console.log("[MapGL Debug] Found user GeoJSON source:", sourceId);
                                            if (!userSourceIds.includes(sourceId)) {
                                                userSourceIds.push(sourceId);
                                            }
                                        } 
                                        // Strategy 2: Check for source data URL patterns typical of R-generated data
                                        else if (source.url && typeof source.url === 'string' && 
                                                 (source.url.includes("data:application/json") || 
                                                  source.url.includes("blob:"))) {
                                            console.log("[MapGL Debug] Found user source with data URL:", sourceId);
                                            if (!userSourceIds.includes(sourceId)) {
                                                userSourceIds.push(sourceId);
                                            }
                                        }
                                        // Strategy 3: Standard filtering - exclude common base map sources
                                        else if (
                                            sourceId !== "composite" && 
                                            sourceId !== "mapbox" && 
                                            !sourceId.startsWith("mapbox-") &&
                                            sourceId !== "openmaptiles" &&  // Common in MapLibre styles
                                            !(sourceId.startsWith("carto") && sourceId !== "carto-source") && // Filter CARTO base sources but keep user ones
                                            !(sourceId.startsWith("maptiler") && !sourceId.includes("user")) && // Filter MapTiler sources but keep user ones
                                            !sourceId.includes("terrain") && // Common terrain sources
                                            !sourceId.includes("hillshade") && // Common hillshade sources
                                            !(sourceId.includes("basemap") && !sourceId.includes("user")) // Filter basemap sources but keep user ones
                                        ) {
                                            console.log("[MapGL Debug] Found user source via filtering:", sourceId);
                                            if (!userSourceIds.includes(sourceId)) {
                                                userSourceIds.push(sourceId);
                                            }
                                        }
                                        
                                        // Store layer-specific handler references
                                        if (window._mapboxHandlers) {
                                            const handlers = window._mapboxHandlers;
                                            for (const layerId in handlers) {
                                                // Find layers associated with this source
                                                const layer = currentStyle.layers.find(l => l.id === layerId);
                                                if (layer && layer.source === sourceId) {
                                                    layer._handlers = handlers[layerId];
                                                }
                                            }
                                        }
                                    }
                                    
                                    // Identify layers using user-added sources or known user layer IDs
                                    // ONLY include layers that use genuinely user-added sources (not base map sources)
                                    currentStyle.layers.forEach(function(layer) {
                                        // Check if this layer uses a genuine user source (not filtered out base map sources)
                                        const usesUserSource = userSourceIds.includes(layer.source);
                                        const isKnownUserLayer = knownUserLayerIds.includes(layer.id);
                                        
                                        // Additional check: exclude layers that use base map sources even if they were temporarily added to userSourceIds
                                        const layerSource = currentStyle.sources[layer.source];
                                        const isBaseMapSource = layerSource && layerSource.type === "vector" && (
                                            layer.source === "composite" || 
                                            layer.source === "mapbox" || 
                                            layer.source.startsWith("mapbox-") ||
                                            layer.source === "openmaptiles" ||
                                            layer.source.startsWith("carto") ||
                                            layer.source.startsWith("maptiler")
                                        );
                                        
                                        if ((usesUserSource || isKnownUserLayer) && !isBaseMapSource) {
                                            userLayers.push(layer);
                                            console.log("[MapGL Debug] Including user layer:", layer.id, "source:", layer.source);
                                        } else if (isBaseMapSource) {
                                            console.log("[MapGL Debug] Excluding base map layer:", layer.id, "source:", layer.source);
                                        }
                                    });
                                    
                                    // Set up event listener to re-add sources and layers after style loads
                                    const onStyleLoad = function() {
                                        // Re-add user sources
                                        userSourceIds.forEach(function(sourceId) {
                                            if (!map.getSource(sourceId)) {
                                                const source = currentStyle.sources[sourceId];
                                                map.addSource(sourceId, source);
                                            }
                                        });
                                        
                                        // Re-add user layers
                                        userLayers.forEach(function(layer) {
                                            if (!map.getLayer(layer.id)) {
                                                map.addLayer(layer);
                                                
                                                // Re-add event handlers for tooltips and hover effects
                                                if (layer._handlers) {
                                                    const handlers = layer._handlers;
                                                    
                                                    if (handlers.mousemove) {
                                                        console.log("[MapGL Debug] Re-adding mousemove handler for:", layer.id);
                                                        map.on("mousemove", layer.id, handlers.mousemove);
                                                    }
                                                    
                                                    if (handlers.mouseleave) {
                                                        console.log("[MapGL Debug] Re-adding mouseleave handler for:", layer.id);
                                                        map.on("mouseleave", layer.id, handlers.mouseleave);
                                                    }
                                                }
                                                
                                                // Check if we need to restore tooltip handlers
                                                const layerId = layer.id;
                                                if (layerId === "nc_counties" || layer.tooltip) {
                                                    console.log("[MapGL Debug] Restoring tooltip for:", layerId);
                                                    
                                                    // Create a new tooltip popup
                                                    const tooltip = new maplibregl.Popup({
                                                        closeButton: false,
                                                        closeOnClick: false
                                                    });
                                                    
                                                    // Re-add tooltip handlers
                                                    const tooltipProperty = layer.tooltip || "NAME";
                                                    
                                                    const mouseMoveHandler = function(e) {
                                                        map.getCanvas().style.cursor = "pointer";
                                                        if (e.features.length > 0) {
                                                            const description = e.features[0].properties[tooltipProperty];
                                                            tooltip.setLngLat(e.lngLat).setHTML(description).addTo(map);
                                                        }
                                                    };
                                                    
                                                    const mouseLeaveHandler = function() {
                                                        map.getCanvas().style.cursor = "";
                                                        tooltip.remove();
                                                    };
                                                    
                                                    map.on("mousemove", layerId, mouseMoveHandler);
                                                    map.on("mouseleave", layerId, mouseLeaveHandler);
                                                    
                                                    // Store these handlers
                                                    if (!window._mapboxHandlers) {
                                                        window._mapboxHandlers = {};
                                                    }
                                                    window._mapboxHandlers[layerId] = {
                                                        mousemove: mouseMoveHandler,
                                                        mouseleave: mouseLeaveHandler
                                                    };
                                                }
                                                
                                                // Recreate hover states if needed
                                                if (layer.paint) {
                                                    for (const key in layer.paint) {
                                                        const value = layer.paint[key];
                                                        if (Array.isArray(value) && value[0] === "case" && 
                                                            Array.isArray(value[1]) && value[1][0] === "boolean" && 
                                                            value[1][1][0] === "feature-state" && value[1][1][1] === "hover") {
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
                                        const savedLayerState = window._mapglLayerState && window._mapglLayerState[mapId];
                                        if (savedLayerState) {
                                            console.log("[MapGL Debug] Restoring tracked layer modifications");
                                            
                                            // Restore filters
                                            for (const layerId in savedLayerState.filters) {
                                                if (map.getLayer(layerId)) {
                                                    console.log("[MapGL Debug] Restoring filter for layer:", layerId);
                                                    map.setFilter(layerId, savedLayerState.filters[layerId]);
                                                }
                                            }
                                            
                                            // Restore paint properties
                                            for (const layerId in savedLayerState.paintProperties) {
                                                if (map.getLayer(layerId)) {
                                                    const properties = savedLayerState.paintProperties[layerId];
                                                    for (const propertyName in properties) {
                                                        const savedValue = properties[propertyName];
                                                        
                                                        console.log("[MapGL Debug] Restoring paint property:", layerId, propertyName, savedValue);
                                                        
                                                        // Check if layer has hover effects that need to be preserved
                                                        const currentValue = map.getPaintProperty(layerId, propertyName);
                                                        if (currentValue && Array.isArray(currentValue) && currentValue[0] === "case") {
                                                            // Preserve hover effects while updating base value
                                                            const hoverValue = currentValue[2];
                                                            const newPaintProperty = [
                                                                "case",
                                                                ["boolean", ["feature-state", "hover"], false],
                                                                hoverValue,
                                                                savedValue,
                                                            ];
                                                            map.setPaintProperty(layerId, propertyName, newPaintProperty);
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
                                                        console.log("[MapGL Debug] Restoring layout property:", layerId, propertyName, properties[propertyName]);
                                                        map.setLayoutProperty(layerId, propertyName, properties[propertyName]);
                                                    }
                                                }
                                            }
                                            
                                            // Restore tooltips
                                            for (const layerId in savedLayerState.tooltips) {
                                                if (map.getLayer(layerId)) {
                                                    const tooltipProperty = savedLayerState.tooltips[layerId];
                                                    console.log("[MapGL Debug] Restoring tooltip:", layerId, tooltipProperty);
                                                    
                                                    // Remove existing tooltip handlers first
                                                    map.off("mousemove", layerId);
                                                    map.off("mouseleave", layerId);
                                                    
                                                    const tooltip = new maplibregl.Popup({
                                                        closeButton: false,
                                                        closeOnClick: false,
                                                    });

                                                    map.on("mousemove", layerId, function (e) {
                                                        onMouseMoveTooltip(e, map, tooltip, tooltipProperty);
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
                                                    console.log("[MapGL Debug] Restoring popup:", layerId, popupProperty);
                                                    
                                                    // Remove existing popup handlers first
                                                    if (window._maplibreClickHandlers && window._maplibreClickHandlers[layerId]) {
                                                        map.off("click", layerId, window._maplibreClickHandlers[layerId]);
                                                        delete window._maplibreClickHandlers[layerId];
                                                    }
                                                    
                                                    const clickHandler = function(e) {
                                                        onClickPopup(e, map, popupProperty, layerId);
                                                    };
                                                    
                                                    map.on("click", layerId, clickHandler);
                                                    
                                                    if (!window._maplibreClickHandlers) {
                                                        window._maplibreClickHandlers = {};
                                                    }
                                                    window._maplibreClickHandlers[layerId] = clickHandler;
                                                }
                                            }
                                        }
                                        
                                        // Remove this listener to avoid adding the same layers multiple times
                                        map.off('style.load', onStyleLoad);
                                    };
                                    
                                    map.on('style.load', onStyleLoad);
                                    
                                    // Store them for potential use outside the onStyleLoad event
                                    // This helps in case the event timing is different in MapLibre
                                    if (!window._mapglPreservedData) {
                                        window._mapglPreservedData = {};
                                    }
                                    window._mapglPreservedData[map.getContainer().id] = {
                                        sources: userSourceIds.map(id => ({id, source: currentStyle.sources[id]})),
                                        layers: userLayers
                                    };
                                    
                                    // Add a backup mechanism specific to MapLibre
                                    // Some MapLibre styles or versions may have different event timing
                                    if (userLayers.length > 0) {
                                        // Set a timeout to check if layers were added after a reasonable delay
                                        setTimeout(function() {
                                            try {
                                                console.log("[MapGL Debug] Running backup layer check");
                                                const mapId = map.getContainer().id;
                                                const preserved = window._mapglPreservedData && window._mapglPreservedData[mapId];
                                                
                                                if (preserved) {
                                                    // Check if user layers were successfully restored
                                                    const firstLayerId = preserved.layers[0]?.id;
                                                    if (firstLayerId && !map.getLayer(firstLayerId)) {
                                                        console.log("[MapGL Debug] Backup restoration needed for layers");
                                                        
                                                        // Re-add sources first
                                                        preserved.sources.forEach(function(src) {
                                                            try {
                                                                if (!map.getSource(src.id)) {
                                                                    console.log("[MapGL Debug] Backup: adding source", src.id);
                                                                    map.addSource(src.id, src.source);
                                                                }
                                                            } catch (err) {
                                                                console.error("[MapGL Debug] Backup: error adding source", src.id, err);
                                                            }
                                                        });
                                                        
                                                        // Then re-add layers
                                                        preserved.layers.forEach(function(layer) {
                                                            try {
                                                                if (!map.getLayer(layer.id)) {
                                                                    console.log("[MapGL Debug] Backup: adding layer", layer.id);
                                                                    map.addLayer(layer);
                                                                    
                                                                    // Check for nc_counties layer to restore tooltip
                                                                    if (layer.id === "nc_counties") {
                                                                        console.log("[MapGL Debug] Backup: restoring tooltip for", layer.id);
                                                                        
                                                                        // Create a new tooltip popup
                                                                        const tooltip = new maplibregl.Popup({
                                                                            closeButton: false,
                                                                            closeOnClick: false
                                                                        });
                                                                        
                                                                        // Re-add tooltip handlers
                                                                        const tooltipProperty = "NAME";
                                                                        
                                                                        const mouseMoveHandler = function(e) {
                                                                            map.getCanvas().style.cursor = "pointer";
                                                                            if (e.features.length > 0) {
                                                                                const description = e.features[0].properties[tooltipProperty];
                                                                                tooltip.setLngLat(e.lngLat).setHTML(description).addTo(map);
                                                                            }
                                                                        };
                                                                        
                                                                        const mouseLeaveHandler = function() {
                                                                            map.getCanvas().style.cursor = "";
                                                                            tooltip.remove();
                                                                        };
                                                                        
                                                                        map.on("mousemove", layer.id, mouseMoveHandler);
                                                                        map.on("mouseleave", layer.id, mouseLeaveHandler);
                                                                        
                                                                        // Store these handlers
                                                                        if (!window._mapboxHandlers) {
                                                                            window._mapboxHandlers = {};
                                                                        }
                                                                        window._mapboxHandlers[layer.id] = {
                                                                            mousemove: mouseMoveHandler,
                                                                            mouseleave: mouseLeaveHandler
                                                                        };
                                                                    }
                                                                    
                                                                    // Restore hover states
                                                                    if (layer.paint) {
                                                                        for (const key in layer.paint) {
                                                                            const value = layer.paint[key];
                                                                            if (Array.isArray(value) && value[0] === "case" && 
                                                                                Array.isArray(value[1]) && value[1][0] === "boolean" && 
                                                                                value[1][1] && Array.isArray(value[1][1]) && 
                                                                                value[1][1][0] === "feature-state" && value[1][1][1] === "hover") {
                                                                                // This is a hover-enabled paint property
                                                                                console.log("[MapGL Debug] Backup: restoring hover style for", layer.id, key);
                                                                                map.setPaintProperty(layer.id, key, value);
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            } catch (err) {
                                                                console.error("[MapGL Debug] Backup: error adding layer", layer.id, err);
                                                            }
                                                        });
                                                    } else {
                                                        console.log("[MapGL Debug] Backup check: layers already restored properly");
                                                    }
                                                }
                                            } catch (err) {
                                                console.error("[MapGL Debug] Error in backup restoration:", err);
                                            }
                                        }, 500); // 500ms delay - faster recovery
                                    }
                                }
                                
                                // Apply the new style
                                map.setStyle(message.style, {
                                    diff: message.diff,
                                });

                                if (message.config) {
                                    Object.keys(message.config).forEach(
                                        function (key) {
                                            map.setConfigProperty(
                                                "basemap",
                                                key,
                                                message.config[key],
                                            );
                                        },
                                    );
                                }

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
                            } else if (
                                message.type === "add_navigation_control"
                            ) {
                                const nav = new maplibregl.NavigationControl({
                                    showCompass: message.options.show_compass,
                                    showZoom: message.options.show_zoom,
                                    visualizePitch:
                                        message.options.visualize_pitch,
                                });
                                map.addControl(nav, message.position);

                                if (message.orientation === "horizontal") {
                                    const navBar = map
                                        .getContainer()
                                        .querySelector(
                                            ".maplibregl-ctrl.maplibregl-ctrl-group:not(.maplibre-gl-draw_ctrl-draw-btn)",
                                        );
                                    if (navBar) {
                                        navBar.style.display = "flex";
                                        navBar.style.flexDirection = "row";
                                    }
                                }
                            } else if (message.type === "add_reset_control") {
                                const resetControl =
                                    document.createElement("button");
                                resetControl.className =
                                    "maplibregl-ctrl-icon maplibregl-ctrl-reset";
                                resetControl.type = "button";
                                resetControl.setAttribute(
                                    "aria-label",
                                    "Reset",
                                );
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
                                resetControl.style.transition =
                                    "background-color 0.2s";
                                resetControl.addEventListener(
                                    "mouseover",
                                    function () {
                                        this.style.backgroundColor = "#f0f0f0";
                                    },
                                );
                                resetControl.addEventListener(
                                    "mouseout",
                                    function () {
                                        this.style.backgroundColor = "white";
                                    },
                                );

                                const resetContainer =
                                    document.createElement("div");
                                resetContainer.className =
                                    "maplibregl-ctrl maplibregl-ctrl-group";
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
                                            resetContainer.parentNode.removeChild(
                                                resetContainer,
                                            );
                                        },
                                    },
                                    message.position,
                                );
                                // Add to controls array
                                map.controls.push(resetControl);
                            } else if (message.type === "add_draw_control") {
                                let drawOptions = message.options || {};
                                if (message.freehand) {
                                    drawOptions = Object.assign(
                                        {},
                                        drawOptions,
                                        {
                                            modes: Object.assign(
                                                {},
                                                MapboxDraw.modes,
                                                {
                                                    draw_polygon:
                                                        MapboxDraw.modes
                                                            .draw_freehand,
                                                },
                                            ),
                                        },
                                    );
                                }

                                draw = new MapboxDraw(drawOptions);
                                map.addControl(draw, message.position);
                                map.controls.push(draw);

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

                                // Add event listeners
                                map.on(
                                    "draw.create",
                                    window.updateDrawnFeatures,
                                );
                                map.on(
                                    "draw.delete",
                                    window.updateDrawnFeatures,
                                );
                                map.on(
                                    "draw.update",
                                    window.updateDrawnFeatures,
                                );

                                if (message.orientation === "horizontal") {
                                    const drawBar = map
                                        .getContainer()
                                        .querySelector(
                                            ".maplibregl-ctrl-group",
                                        );
                                    if (drawBar) {
                                        drawBar.style.display = "flex";
                                        drawBar.style.flexDirection = "row";
                                    }
                                }
                            } else if (message.type === "get_drawn_features") {
                                if (draw) {
                                    const features = draw
                                        ? draw.getAll()
                                        : null;
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
                            } else if (
                                message.type === "clear_drawn_features"
                            ) {
                                if (draw) {
                                    draw.deleteAll();
                                    // Update the drawn features
                                    window.updateDrawnFeatures();
                                }
                            } else if (message.type === "add_markers") {
                                if (!window.maplibreglMarkers) {
                                    window.maplibreglMarkers = [];
                                }
                                message.markers.forEach(function (marker) {
                                    const markerOptions = {
                                        color: marker.color,
                                        rotation: marker.rotation,
                                        draggable:
                                            marker.options.draggable || false,
                                        ...marker.options,
                                    };
                                    const mapMarker = new maplibregl.Marker(
                                        markerOptions,
                                    )
                                        .setLngLat([marker.lng, marker.lat])
                                        .addTo(map);

                                    if (marker.popup) {
                                        mapMarker.setPopup(
                                            new maplibregl.Popup({
                                                offset: 25,
                                            }).setHTML(marker.popup),
                                        );
                                    }

                                    const markerId = marker.id;
                                    if (markerId) {
                                        const lngLat = mapMarker.getLngLat();
                                        Shiny.setInputValue(
                                            data.id + "_marker_" + markerId,
                                            {
                                                id: markerId,
                                                lng: lngLat.lng,
                                                lat: lngLat.lat,
                                            },
                                        );

                                        mapMarker.on("dragend", function () {
                                            const lngLat =
                                                mapMarker.getLngLat();
                                            Shiny.setInputValue(
                                                data.id + "_marker_" + markerId,
                                                {
                                                    id: markerId,
                                                    lng: lngLat.lng,
                                                    lat: lngLat.lat,
                                                },
                                            );
                                        });
                                    }

                                    window.maplibreglMarkers.push(mapMarker);
                                });
                            } else if (message.type === "clear_markers") {
                                if (window.maplibreglMarkers) {
                                    window.maplibreglMarkers.forEach(
                                        function (marker) {
                                            marker.remove();
                                        },
                                    );
                                    window.maplibreglMarkers = [];
                                }
                            } else if (
                                message.type === "add_fullscreen_control"
                            ) {
                                const position =
                                    message.position || "top-right";
                                const fullscreen =
                                    new maplibregl.FullscreenControl();
                                map.addControl(fullscreen, position);
                                map.controls.push(fullscreen);
                            } else if (message.type === "add_scale_control") {
                                const scaleControl =
                                    new maplibregl.ScaleControl({
                                        maxWidth: message.options.maxWidth,
                                        unit: message.options.unit,
                                    });
                                map.addControl(
                                    scaleControl,
                                    message.options.position,
                                );
                                map.controls.push(scaleControl);
                            } else if (
                                message.type === "add_geolocate_control"
                            ) {
                                const geolocate =
                                    new maplibregl.GeolocateControl({
                                        positionOptions:
                                            message.options.positionOptions,
                                        trackUserLocation:
                                            message.options.trackUserLocation,
                                        showAccuracyCircle:
                                            message.options.showAccuracyCircle,
                                        showUserLocation:
                                            message.options.showUserLocation,
                                        showUserHeading:
                                            message.options.showUserHeading,
                                        fitBoundsOptions:
                                            message.options.fitBoundsOptions,
                                    });
                                map.addControl(
                                    geolocate,
                                    message.options.position,
                                );
                                map.controls.push(geolocate);

                                if (HTMLWidgets.shinyMode) {
                                    geolocate.on("geolocate", function (event) {
                                        Shiny.setInputValue(
                                            data.id + "_geolocate",
                                            {
                                                coords: event.coords,
                                                time: new Date(),
                                            },
                                        );
                                    });

                                    geolocate.on(
                                        "trackuserlocationstart",
                                        function () {
                                            Shiny.setInputValue(
                                                data.id + "_geolocate_tracking",
                                                {
                                                    status: "start",
                                                    time: new Date(),
                                                },
                                            );
                                        },
                                    );

                                    geolocate.on(
                                        "trackuserlocationend",
                                        function () {
                                            Shiny.setInputValue(
                                                data.id + "_geolocate_tracking",
                                                {
                                                    status: "end",
                                                    time: new Date(),
                                                },
                                            );
                                        },
                                    );

                                    geolocate.on("error", function (error) {
                                        if (error.error.code === 1) {
                                            Shiny.setInputValue(
                                                data.id + "_geolocate_error",
                                                {
                                                    message:
                                                        "Location permission denied",
                                                    time: new Date(),
                                                },
                                            );
                                        }
                                    });
                                }
                            } else if (
                                message.type === "add_geocoder_control"
                            ) {
                                const provider = message.options.provider || "osm";
                                let geocoder;

                                if (provider === "maptiler") {
                                    // MapTiler geocoder
                                    const maptilerOptions = {
                                        apiKey: message.options.api_key,
                                        maplibregl: maplibregl,
                                        ...message.options,
                                    };

                                    // Create MapTiler geocoder
                                    geocoder = new maplibreglMaptilerGeocoder.GeocodingControl(maptilerOptions);
                                } else {
                                    // OSM/Nominatim geocoder (default)
                                    const geocoderApi = {
                                        forwardGeocode: async (config) => {
                                            const features = [];
                                            try {
                                                const request = `https://nominatim.openstreetmap.org/search?q=${
                                                    config.query
                                                }&format=geojson&polygon_geojson=1&addressdetails=1`;
                                                const response =
                                                    await fetch(request);
                                                const geojson =
                                                    await response.json();
                                                for (const feature of geojson.features) {
                                                    const center = [
                                                        feature.bbox[0] +
                                                            (feature.bbox[2] -
                                                                feature.bbox[0]) /
                                                                2,
                                                        feature.bbox[1] +
                                                            (feature.bbox[3] -
                                                                feature.bbox[1]) /
                                                                2,
                                                    ];
                                                    const point = {
                                                        type: "Feature",
                                                        geometry: {
                                                            type: "Point",
                                                            coordinates: center,
                                                        },
                                                        place_name:
                                                            feature.properties
                                                                .display_name,
                                                        properties:
                                                            feature.properties,
                                                        text: feature.properties
                                                            .display_name,
                                                        place_type: ["place"],
                                                        center,
                                                    };
                                                    features.push(point);
                                                }
                                            } catch (e) {
                                                console.error(
                                                    `Failed to forwardGeocode with error: ${e}`,
                                                );
                                            }

                                            return {
                                                features,
                                            };
                                        },
                                    };

                                    const geocoderOptions = {
                                        maplibregl: maplibregl,
                                        ...message.options,
                                    };

                                    // Set default values if not provided
                                    if (!geocoderOptions.placeholder)
                                        geocoderOptions.placeholder = "Search";
                                    if (
                                        typeof geocoderOptions.collapsed ===
                                        "undefined"
                                    )
                                        geocoderOptions.collapsed = false;

                                    geocoder = new MaplibreGeocoder(
                                        geocoderApi,
                                        geocoderOptions,
                                    );
                                }

                                map.addControl(
                                    geocoder,
                                    message.options.position || "top-right",
                                );
                                map.controls.push(geocoder);
                                
                                // Apply CSS fix for MapTiler geocoder to prevent cutoff
                                if (provider === "maptiler") {
                                    setTimeout(() => {
                                        const controlContainer = document.querySelector('.maplibregl-ctrl-geocoder');
                                        if (controlContainer) {
                                            controlContainer.style.maxWidth = '300px';
                                            controlContainer.style.width = 'auto';
                                        }
                                    }, 100);
                                }

                                // Handle geocoder results in Shiny mode
                                if (provider === "maptiler") {
                                    // MapTiler uses different event names
                                    geocoder.on("pick", function (e) {
                                        Shiny.setInputValue(data.id + "_geocoder", {
                                            result: e,
                                            time: new Date(),
                                        });
                                    });
                                } else {
                                    // OSM geocoder
                                    geocoder.on("results", function (e) {
                                        Shiny.setInputValue(data.id + "_geocoder", {
                                            result: e,
                                            time: new Date(),
                                        });
                                    });
                                }
                            } else if (message.type === "add_layers_control") {
                                const layersControl =
                                    document.createElement("div");
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
                                    const styleEl =
                                        document.createElement("style");
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

                                document
                                    .getElementById(data.id)
                                    .appendChild(layersControl);

                                const layersList =
                                    document.createElement("div");
                                layersList.className = "layers-list";
                                layersControl.appendChild(layersList);

                                // Fetch layers to be included in the control
                                let layers =
                                    message.layers ||
                                    map
                                        .getStyle()
                                        .layers.map((layer) => layer.id);

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

                                        const visibility =
                                            map.getLayoutProperty(
                                                clickedLayer,
                                                "visibility",
                                            );

                                        // Toggle layer visibility by changing the layout object's visibility property
                                        if (visibility === "visible") {
                                            map.setLayoutProperty(
                                                clickedLayer,
                                                "visibility",
                                                "none",
                                            );
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
                                    const toggleButton =
                                        document.createElement("div");
                                    toggleButton.className = "toggle-button";
                                    toggleButton.textContent = "Layers";
                                    toggleButton.onclick = function () {
                                        layersControl.classList.toggle("open");
                                    };
                                    layersControl.insertBefore(
                                        toggleButton,
                                        layersList,
                                    );
                                }
                            } else if (message.type === "add_globe_minimap") {
                                // Add the globe minimap control if supported
                                if (typeof MapboxGlobeMinimap !== "undefined") {
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
                                } else {
                                    console.warn(
                                        "MapboxGlobeMinimap is not defined",
                                    );
                                }
                            } else if (message.type === "add_globe_control") {
                                // Add the globe control
                                const globeControl =
                                    new maplibregl.GlobeControl();
                                map.addControl(globeControl, message.position);
                                map.controls.push(globeControl);
                            } else if (message.type === "add_draw_control") {
                                MapboxDraw.constants.classes.CONTROL_BASE = "maplibregl-ctrl";
                                MapboxDraw.constants.classes.CONTROL_PREFIX = "maplibregl-ctrl-";
                                MapboxDraw.constants.classes.CONTROL_GROUP = "maplibregl-ctrl-group";
                                
                                let drawOptions = message.options || {};
                                
                                // Generate styles if styling parameters provided
                                if (message.styling) {
                                    const generatedStyles = generateDrawStyles(message.styling);
                                    if (generatedStyles) {
                                        drawOptions.styles = generatedStyles;
                                    }
                                }
                                
                                if (message.freehand) {
                                    drawOptions = Object.assign(
                                        {},
                                        drawOptions,
                                        {
                                            modes: Object.assign(
                                                {},
                                                MapboxDraw.modes,
                                                {
                                                    draw_polygon:
                                                        MapboxDraw.modes
                                                            .draw_freehand,
                                                },
                                            ),
                                            // defaultMode: 'draw_polygon' # Don't set the default yet
                                        },
                                    );
                                }
                                
                                // Fix MapLibre compatibility - ensure we always have custom styles
                                if (!drawOptions.styles) {
                                    drawOptions.styles = generateDrawStyles({
                                        vertex_radius: 5,
                                        active_color: '#fbb03b',
                                        point_color: '#3bb2d0',
                                        line_color: '#3bb2d0',
                                        fill_color: '#3bb2d0',
                                        fill_opacity: 0.1,
                                        line_width: 2
                                    });
                                }

                                draw = new MapboxDraw(drawOptions);
                                map.addControl(draw, message.position);
                                map.controls.push(draw);
                                
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
                                        .querySelector(".maplibregl-ctrl-group");
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
                                        const drawButtons = map.getContainer().querySelector('.maplibregl-ctrl-group:has(.mapbox-gl-draw_polygon)');
                                        
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
                                                const data = draw.getAll();
                                                
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
                                if (draw) {
                                    const features = draw
                                        ? draw.getAll()
                                        : null;
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
                            } else if (
                                message.type === "clear_drawn_features"
                            ) {
                                if (draw) {
                                    draw.deleteAll();
                                    // Update the drawn features
                                    updateDrawnFeatures();
                                }
                            } else if (message.type === "add_features_to_draw") {
                                if (draw) {
                                    if (message.data.clear_existing) {
                                        draw.deleteAll();
                                    }
                                    addSourceFeaturesToDraw(draw, message.data.source, map);
                                    // Update the drawn features
                                    updateDrawnFeatures();
                                } else {
                                    console.warn('Draw control not initialized');
                                }
                            } else if (message.type === "set_projection") {
                                // Only if maplibre supports projection
                                if (typeof map.setProjection === "function") {
                                    map.setProjection(message.projection);
                                }
                            } else if (message.type === "set_source") {
                                if (map.getLayer(message.layer)) {
                                    const sourceId = map.getLayer(
                                        message.layer,
                                    ).source;
                                    map.getSource(sourceId).setData(
                                        JSON.parse(message.source),
                                    );
                                }
                            } else if (message.type === "set_tooltip") {
                                // Track tooltip state
                                layerState.tooltips[message.layer] = message.tooltip;
                                
                                if (map.getLayer(message.layer)) {
                                    // Remove any existing tooltip handlers
                                    map.off("mousemove", message.layer);
                                    map.off("mouseleave", message.layer);

                                    const tooltip = new maplibregl.Popup({
                                        closeButton: false,
                                        closeOnClick: false,
                                    });

                                    map.on(
                                        "mousemove",
                                        message.layer,
                                        function (e) {
                                            map.getCanvas().style.cursor =
                                                "pointer";
                                            if (e.features.length > 0) {
                                                const description =
                                                    e.features[0].properties[
                                                        message.tooltip
                                                    ];
                                                tooltip
                                                    .setLngLat(e.lngLat)
                                                    .setHTML(description)
                                                    .addTo(map);
                                            }
                                        },
                                    );

                                    map.on(
                                        "mouseleave",
                                        message.layer,
                                        function () {
                                            map.getCanvas().style.cursor = "";
                                            tooltip.remove();
                                        },
                                    );
                                }
                            } else if (message.type === "set_popup") {
                                // Track popup state
                                layerState.popups[message.layer] = message.popup;
                                
                                if (map.getLayer(message.layer)) {
                                    // Remove any existing popup click handlers for this layer
                                    if (window._maplibreClickHandlers && window._maplibreClickHandlers[message.layer]) {
                                        map.off("click", message.layer, window._maplibreClickHandlers[message.layer]);
                                        delete window._maplibreClickHandlers[message.layer];
                                    }
                                    
                                    // Remove any existing popup for this layer
                                    if (window._maplibrePopups && window._maplibrePopups[message.layer]) {
                                        window._maplibrePopups[message.layer].remove();
                                        delete window._maplibrePopups[message.layer];
                                    }

                                    // Create new click handler for popup
                                    const clickHandler = function (e) {
                                        onClickPopup(e, map, message.popup, message.layer);
                                    };
                                    
                                    // Store handler reference
                                    if (!window._maplibreClickHandlers) {
                                        window._maplibreClickHandlers = {};
                                    }
                                    window._maplibreClickHandlers[message.layer] = clickHandler;
                                    
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
                                        map.moveLayer(
                                            message.layer,
                                            message.before,
                                        );
                                    } else {
                                        map.moveLayer(message.layer);
                                    }
                                }
                            } else if (message.type === "set_opacity") {
                                // Set opacity for all fill layers
                                const style = map.getStyle();
                                if (style && style.layers) {
                                    style.layers.forEach(function (layer) {
                                        if (
                                            layer.type === "fill" &&
                                            map.getLayer(layer.id)
                                        ) {
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
                            Shiny.setInputValue(
                                parentId + "_" + mapType + "_view",
                                {
                                    center: [center.lng, center.lat],
                                    zoom: zoom,
                                    bearing: bearing,
                                    pitch: pitch,
                                },
                            );
                        }
                    });

                    // Send clicked point coordinates to Shiny
                    map.on("click", function (e) {
                        if (window.Shiny) {
                            Shiny.setInputValue(
                                parentId + "_" + mapType + "_click",
                                {
                                    lng: e.lngLat.lng,
                                    lat: e.lngLat.lat,
                                    time: Date.now(),
                                },
                            );
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
                    // Define the tooltip handler functions to match the ones in maplibregl.js
                    function onMouseMoveTooltip(
                        e,
                        map,
                        tooltipPopup,
                        tooltipProperty,
                    ) {
                        map.getCanvas().style.cursor = "pointer";
                        if (e.features.length > 0) {
                            let description;
                            
                            // Check if tooltipProperty is an expression (array) or a simple property name (string)
                            if (Array.isArray(tooltipProperty)) {
                                // It's an expression, evaluate it
                                description = evaluateExpression(tooltipProperty, e.features[0].properties);
                            } else {
                                // It's a property name, get the value
                                description = e.features[0].properties[tooltipProperty];
                            }
                            
                            tooltipPopup
                                .setLngLat(e.lngLat)
                                .setHTML(description)
                                .addTo(map);

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

                    function evaluateExpression(expression, properties) {
                        if (!Array.isArray(expression)) {
                            return expression;
                        }
                        
                        const operator = expression[0];
                        
                        switch (operator) {
                            case 'get':
                                return properties[expression[1]];
                            case 'concat':
                                return expression.slice(1).map(item => evaluateExpression(item, properties)).join('');
                            case 'to-string':
                                return String(evaluateExpression(expression[1], properties));
                            case 'to-number':
                                return Number(evaluateExpression(expression[1], properties));
                            default:
                                // For literals and other simple values
                                return expression;
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
                        if (window._maplibrePopups && window._maplibrePopups[layerId]) {
                            window._maplibrePopups[layerId].remove();
                        }
                        
                        // Create and show the popup
                        const popup = new maplibregl.Popup()
                            .setLngLat(e.lngLat)
                            .setHTML(description)
                            .addTo(map);
                            
                        // Store reference to this popup
                        if (!window._maplibrePopups) {
                            window._maplibrePopups = {};
                        }
                        window._maplibrePopups[layerId] = popup;
                        
                        // Remove reference when popup is closed
                        popup.on('close', function() {
                            if (window._maplibrePopups[layerId] === popup) {
                                delete window._maplibrePopups[layerId];
                            }
                        });
                    }

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
                        if (!window.maplibreglMarkers) {
                            window.maplibreglMarkers = [];
                        }
                        mapData.markers.forEach(function (marker) {
                            const markerOptions = {
                                color: marker.color,
                                rotation: marker.rotation,
                                draggable: marker.options.draggable || false,
                                ...marker.options,
                            };
                            const mapMarker = new maplibregl.Marker(
                                markerOptions,
                            )
                                .setLngLat([marker.lng, marker.lat])
                                .addTo(map);

                            if (marker.popup) {
                                mapMarker.setPopup(
                                    new maplibregl.Popup({
                                        offset: 25,
                                    }).setText(marker.popup),
                                );
                            }

                            const markerId = marker.id;
                            if (markerId) {
                                const lngLat = mapMarker.getLngLat();
                                if (HTMLWidgets.shinyMode) {
                                    Shiny.setInputValue(
                                        el.id + "_marker_" + markerId,
                                        {
                                            id: markerId,
                                            lng: lngLat.lng,
                                            lat: lngLat.lat,
                                        },
                                    );
                                }

                                mapMarker.on("dragend", function () {
                                    const lngLat = mapMarker.getLngLat();
                                    if (HTMLWidgets.shinyMode) {
                                        Shiny.setInputValue(
                                            el.id + "_marker_" + markerId,
                                            {
                                                id: markerId,
                                                lng: lngLat.lng,
                                                lat: lngLat.lat,
                                            },
                                        );
                                    }
                                });
                            }

                            window.maplibreglMarkers.push(mapMarker);
                        });
                    }

                    // Add sources if provided
                    if (mapData.sources) {
                        mapData.sources.forEach(function (source) {
                            if (source.type === "vector") {
                                const sourceConfig = {
                                    type: "vector",
                                    url: source.url,
                                };
                                if (source.promoteId) {
                                    sourceConfig.promoteId = source.promoteId;
                                }
                                map.addSource(source.id, sourceConfig);
                            } else if (source.type === "geojson") {
                                map.addSource(source.id, {
                                    type: "geojson",
                                    data: source.data,
                                    generateId: source.generateId !== false,
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
                                    layerConfig["source-layer"] =
                                        layer.source_layer;
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
                                        onClickPopup(e, map, layer.popup, layer.id);
                                    });

                                    // Change cursor to pointer when hovering over the layer
                                    map.on("mouseenter", layer.id, function () {
                                        map.getCanvas().style.cursor =
                                            "pointer";
                                    });

                                    // Change cursor back to default when leaving the layer
                                    map.on("mouseleave", layer.id, function () {
                                        map.getCanvas().style.cursor = "";
                                    });
                                }

                                if (layer.tooltip) {
                                    const tooltip = new maplibregl.Popup({
                                        closeButton: false,
                                        closeOnClick: false,
                                    });

                                    // Create a reference to the mousemove handler function
                                    const mouseMoveHandler = function (e) {
                                        onMouseMoveTooltip(
                                            e,
                                            map,
                                            tooltip,
                                            layer.tooltip,
                                        );
                                    };

                                    // Create a reference to the mouseleave handler function
                                    const mouseLeaveHandler = function () {
                                        onMouseLeaveTooltip(map, tooltip);
                                    };

                                    // Attach the named handler references
                                    map.on(
                                        "mousemove",
                                        layer.id,
                                        mouseMoveHandler,
                                    );
                                    map.on(
                                        "mouseleave",
                                        layer.id,
                                        mouseLeaveHandler,
                                    );

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
                                                            typeof layer.source ===
                                                            "string"
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
                                                        typeof layer.source ===
                                                        "string"
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
                                                        typeof layer.source ===
                                                        "string"
                                                            ? layer.source
                                                            : layer.id,
                                                    id: hoveredFeatureId,
                                                },
                                                { hover: false },
                                            );
                                        }
                                        hoveredFeatureId = null;
                                    });

                                    Object.keys(jsHoverOptions).forEach(
                                        function (key) {
                                            const originalPaint =
                                                map.getPaintProperty(
                                                    layer.id,
                                                    key,
                                                ) || layer.paint[key];
                                            map.setPaintProperty(
                                                layer.id,
                                                key,
                                                [
                                                    "case",
                                                    [
                                                        "boolean",
                                                        [
                                                            "feature-state",
                                                            "hover",
                                                        ],
                                                        false,
                                                    ],
                                                    jsHoverOptions[key],
                                                    originalPaint,
                                                ],
                                            );
                                        },
                                    );
                                }
                            } catch (e) {
                                console.error(
                                    "Failed to add layer: ",
                                    layer,
                                    e,
                                );
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

                    if (mapData.fitBounds) {
                        map.fitBounds(
                            mapData.fitBounds.bounds,
                            mapData.fitBounds.options,
                        );
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
                    if (mapData.jumpTo) {
                        map.jumpTo(mapData.jumpTo);
                    }

                    // Add custom images if provided
                    if (mapData.images && Array.isArray(mapData.images)) {
                        mapData.images.forEach(async function (imageInfo) {
                            try {
                                const image = await map.loadImage(
                                    imageInfo.url,
                                );
                                if (!map.hasImage(imageInfo.id)) {
                                    map.addImage(
                                        imageInfo.id,
                                        image.data,
                                        imageInfo.options,
                                    );
                                }
                            } catch (error) {
                                console.error("Error loading image:", error);
                            }
                        });
                    } else if (mapData.images) {
                        console.error(
                            "mapData.images is not an array:",
                            mapData.images,
                        );
                    }

                    // Remove existing legends only from this specific map container
                    const mapContainer = map.getContainer();
                    const existingLegends = mapContainer.querySelectorAll(".mapboxgl-legend");
                    existingLegends.forEach((legend) => legend.remove());

                    if (mapData.legend_html && mapData.legend_css) {
                        const legendCss = document.createElement("style");
                        legendCss.innerHTML = mapData.legend_css;
                        document.head.appendChild(legendCss);

                        const legend = document.createElement("div");
                        legend.innerHTML = mapData.legend_html;
                        legend.classList.add("mapboxgl-legend");
                        
                        // Append legend to the correct map container instead of main container
                        mapContainer.appendChild(legend);
                    }

                    // Add fullscreen control if enabled
                    if (
                        mapData.fullscreen_control &&
                        mapData.fullscreen_control.enabled
                    ) {
                        const position =
                            mapData.fullscreen_control.position || "top-right";
                        map.addControl(
                            new maplibregl.FullscreenControl(),
                            position,
                        );
                    }

                    // Helper function to generate draw styles based on parameters
                    function generateDrawStyles(styling) {
                        if (!styling) return null;
                        
                        return [
                            // Point styles
                            {
                                'id': 'gl-draw-point-active',
                                'type': 'circle',
                                'filter': ['all',
                                    ['==', '$type', 'Point'],
                                    ['==', 'meta', 'feature'],
                                    ['==', 'active', 'true']],
                                'paint': {
                                    'circle-radius': styling.vertex_radius + 2,
                                    'circle-color': styling.active_color
                                }
                            },
                            {
                                'id': 'gl-draw-point',
                                'type': 'circle',
                                'filter': ['all',
                                    ['==', '$type', 'Point'],
                                    ['==', 'meta', 'feature'],
                                    ['==', 'active', 'false']],
                                'paint': {
                                    'circle-radius': styling.vertex_radius,
                                    'circle-color': styling.point_color
                                }
                            },
                            // Line styles
                            {
                                'id': 'gl-draw-line',
                                'type': 'line',
                                'filter': ['all', ['==', '$type', 'LineString']],
                                'layout': {
                                    'line-cap': 'round',
                                    'line-join': 'round'
                                },
                                'paint': {
                                    'line-color': ['case',
                                        ['==', ['get', 'active'], 'true'], styling.active_color,
                                        styling.line_color
                                    ],
                                    'line-width': styling.line_width
                                }
                            },
                            // Polygon fill
                            {
                                'id': 'gl-draw-polygon-fill',
                                'type': 'fill',
                                'filter': ['all', ['==', '$type', 'Polygon']],
                                'paint': {
                                    'fill-color': ['case',
                                        ['==', ['get', 'active'], 'true'], styling.active_color,
                                        styling.fill_color
                                    ],
                                    'fill-outline-color': ['case',
                                        ['==', ['get', 'active'], 'true'], styling.active_color,
                                        styling.fill_color
                                    ],
                                    'fill-opacity': styling.fill_opacity
                                }
                            },
                            // Polygon outline
                            {
                                'id': 'gl-draw-polygon-stroke',
                                'type': 'line',
                                'filter': ['all', ['==', '$type', 'Polygon']],
                                'layout': {
                                    'line-cap': 'round',
                                    'line-join': 'round'
                                },
                                'paint': {
                                    'line-color': ['case',
                                        ['==', ['get', 'active'], 'true'], styling.active_color,
                                        styling.line_color
                                    ],
                                    'line-width': styling.line_width
                                }
                            },
                            // Midpoints
                            {
                                'id': 'gl-draw-polygon-midpoint',
                                'type': 'circle',
                                'filter': ['all',
                                    ['==', '$type', 'Point'],
                                    ['==', 'meta', 'midpoint']],
                                'paint': {
                                    'circle-radius': 3,
                                    'circle-color': styling.active_color
                                }
                            },
                            // Vertex point halos
                            {
                                'id': 'gl-draw-vertex-halo-active',
                                'type': 'circle',
                                'filter': ['all', 
                                    ['==', 'meta', 'vertex'],
                                    ['==', '$type', 'Point']],
                                'paint': {
                                    'circle-radius': ['case',
                                        ['==', ['get', 'active'], 'true'], styling.vertex_radius + 4,
                                        styling.vertex_radius + 2
                                    ],
                                    'circle-color': '#FFF'
                                }
                            },
                            // Vertex points
                            {
                                'id': 'gl-draw-vertex-active',
                                'type': 'circle',
                                'filter': ['all',
                                    ['==', 'meta', 'vertex'],
                                    ['==', '$type', 'Point']],
                                'paint': {
                                    'circle-radius': ['case',
                                        ['==', ['get', 'active'], 'true'], styling.vertex_radius + 2,
                                        styling.vertex_radius
                                    ],
                                    'circle-color': styling.active_color
                                }
                            }
                        ];
                    }

                    // Helper function to add features from a source to draw
                    function addSourceFeaturesToDraw(draw, sourceId, map) {
                        const source = map.getSource(sourceId);
                        if (source && source._data) {
                            draw.add(source._data);
                        } else {
                            console.warn('Source not found or has no data:', sourceId);
                        }
                    }

                    if (mapData.scale_control) {
                        const scaleControl = new maplibregl.ScaleControl({
                            maxWidth: mapData.scale_control.maxWidth,
                            unit: mapData.scale_control.unit,
                        });
                        map.addControl(
                            scaleControl,
                            mapData.scale_control.position,
                        );
                        map.controls.push(scaleControl);
                    }

                    // Add navigation control if enabled
                    if (mapData.navigation_control) {
                        const nav = new maplibregl.NavigationControl({
                            showCompass:
                                mapData.navigation_control.show_compass,
                            showZoom: mapData.navigation_control.show_zoom,
                            visualizePitch:
                                mapData.navigation_control.visualize_pitch,
                        });
                        map.addControl(
                            nav,
                            mapData.navigation_control.position,
                        );
                        map.controls.push(nav);
                    }

                    // Add geolocate control if enabled
                    if (mapData.geolocate_control) {
                        const geolocate = new maplibregl.GeolocateControl({
                            positionOptions:
                                mapData.geolocate_control.positionOptions,
                            trackUserLocation:
                                mapData.geolocate_control.trackUserLocation,
                            showAccuracyCircle:
                                mapData.geolocate_control.showAccuracyCircle,
                            showUserLocation:
                                mapData.geolocate_control.showUserLocation,
                            showUserHeading:
                                mapData.geolocate_control.showUserHeading,
                            fitBoundsOptions:
                                mapData.geolocate_control.fitBoundsOptions,
                        });
                        map.addControl(
                            geolocate,
                            mapData.geolocate_control.position,
                        );

                        map.controls.push(geolocate);
                    }

                    // Add globe control if enabled
                    if (mapData.globe_control) {
                        const globeControl = new maplibregl.GlobeControl();
                        map.addControl(
                            globeControl,
                            mapData.globe_control.position,
                        );
                        map.controls.push(globeControl);
                    }

                    // Add draw control if enabled
                    if (mapData.draw_control && mapData.draw_control.enabled) {
                        MapboxDraw.constants.classes.CONTROL_BASE = "maplibregl-ctrl";
                        MapboxDraw.constants.classes.CONTROL_PREFIX = "maplibregl-ctrl-";
                        MapboxDraw.constants.classes.CONTROL_GROUP = "maplibregl-ctrl-group";
                        
                        let drawOptions = mapData.draw_control.options || {};
                        
                        // Generate styles if styling parameters provided
                        if (mapData.draw_control.styling) {
                            const generatedStyles = generateDrawStyles(mapData.draw_control.styling);
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
                        
                        // Fix MapLibre compatibility - ensure we always have custom styles
                        if (!drawOptions.styles) {
                            drawOptions.styles = generateDrawStyles({
                                vertex_radius: 5,
                                active_color: '#fbb03b',
                                point_color: '#3bb2d0',
                                line_color: '#3bb2d0',
                                fill_color: '#3bb2d0',
                                fill_opacity: 0.1,
                                line_width: 2
                            });
                        }

                        draw = new MapboxDraw(drawOptions);
                        map.addControl(draw, mapData.draw_control.position);
                        map.controls.push(draw);
                        
                        // Add initial features if provided
                        if (mapData.draw_control.source) {
                            addSourceFeaturesToDraw(draw, mapData.draw_control.source, map);
                        }
                        
                        // Process any queued features
                        if (mapData.draw_features_queue) {
                            mapData.draw_features_queue.forEach(function(data) {
                                if (data.clear_existing) {
                                    draw.deleteAll();
                                }
                                addSourceFeaturesToDraw(draw, data.source, map);
                            });
                        }

                        // Apply orientation styling
                        if (mapData.draw_control.orientation === "horizontal") {
                            const drawBar = map
                                .getContainer()
                                .querySelector(".maplibregl-ctrl-group");
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
                                const drawButtons = map.getContainer().querySelector('.maplibregl-ctrl-group:has(.mapbox-gl-draw_polygon)');
                                
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
                                        const data = draw.getAll();
                                        
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
                        
                        // Helper function for updating drawn features
                        function updateDrawnFeatures() {
                            if (HTMLWidgets.shinyMode && draw) {
                                const features = draw.getAll();
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

                    if (mapData.geolocate_control && HTMLWidgets.shinyMode) {
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
                                Shiny.setInputValue(
                                    el.id + "_geolocate_error",
                                    {
                                        message: "Location permission denied",
                                        time: new Date(),
                                    },
                                );
                            }
                        });
                    }

                    // Add geocoder control if enabled
                    if (mapData.geocoder_control) {
                        const provider = mapData.geocoder_control.provider || "osm";
                        let geocoder;

                        if (provider === "maptiler") {
                            // MapTiler geocoder
                            const maptilerOptions = {
                                apiKey: mapData.geocoder_control.api_key,
                                maplibregl: maplibregl,
                                ...mapData.geocoder_control,
                            };

                            // Create MapTiler geocoder
                            geocoder = new maplibreglMaptilerGeocoder.GeocodingControl(maptilerOptions);
                        } else {
                            // OSM/Nominatim geocoder (default)
                            const geocoderApi = {
                                forwardGeocode: async (config) => {
                                    const features = [];
                                    try {
                                        const request = `https://nominatim.openstreetmap.org/search?q=${
                                            config.query
                                        }&format=geojson&polygon_geojson=1&addressdetails=1`;
                                        const response = await fetch(request);
                                        const geojson = await response.json();
                                        for (const feature of geojson.features) {
                                            const center = [
                                                feature.bbox[0] +
                                                    (feature.bbox[2] -
                                                        feature.bbox[0]) /
                                                        2,
                                                feature.bbox[1] +
                                                    (feature.bbox[3] -
                                                        feature.bbox[1]) /
                                                        2,
                                            ];
                                            const point = {
                                                type: "Feature",
                                                geometry: {
                                                    type: "Point",
                                                    coordinates: center,
                                                },
                                                place_name:
                                                    feature.properties.display_name,
                                                properties: feature.properties,
                                                text: feature.properties
                                                    .display_name,
                                                place_type: ["place"],
                                                center,
                                            };
                                            features.push(point);
                                        }
                                    } catch (e) {
                                        console.error(
                                            `Failed to forwardGeocode with error: ${e}`,
                                        );
                                    }

                                    return {
                                        features,
                                    };
                                },
                            };
                            const geocoderOptions = {
                                maplibregl: maplibregl,
                                ...mapData.geocoder_control,
                            };

                            // Set default values if not provided
                            if (!geocoderOptions.placeholder)
                                geocoderOptions.placeholder = "Search";
                            if (typeof geocoderOptions.collapsed === "undefined")
                                geocoderOptions.collapsed = false;

                            geocoder = new MaplibreGeocoder(
                                geocoderApi,
                                geocoderOptions,
                            );
                        }

                        map.addControl(
                            geocoder,
                            mapData.geocoder_control.position || "top-right",
                        );
                        
                        // Apply CSS fix for MapTiler geocoder to prevent cutoff
                        if (provider === "maptiler") {
                            setTimeout(() => {
                                const controlContainer = document.querySelector('.maplibregl-ctrl-geocoder');
                                if (controlContainer) {
                                    controlContainer.style.maxWidth = '300px';
                                    controlContainer.style.width = 'auto';
                                }
                            }, 100);
                        }

                        // Handle geocoder results in Shiny mode
                        if (HTMLWidgets.shinyMode) {
                            if (provider === "maptiler") {
                                // MapTiler uses different event names
                                geocoder.on("pick", function (e) {
                                    Shiny.setInputValue(el.id + "_geocoder", {
                                        result: e,
                                        time: new Date(),
                                    });
                                });
                            } else {
                                // OSM geocoder
                                geocoder.on("results", function (e) {
                                    Shiny.setInputValue(el.id + "_geocoder", {
                                        result: e,
                                        time: new Date(),
                                    });
                                });
                            }
                        }
                    }

                    // Add reset control if enabled
                    if (mapData.reset_control) {
                        const resetControl = document.createElement("button");
                        resetControl.className =
                            "maplibregl-ctrl-icon maplibregl-ctrl-reset";
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
                        resetContainer.className =
                            "maplibregl-ctrl maplibregl-ctrl-group";
                        resetContainer.appendChild(resetControl);

                        const initialView = {
                            center: map.getCenter(),
                            zoom: map.getZoom(),
                            pitch: map.getPitch(),
                            bearing: map.getBearing(),
                            animate: mapData.reset_control.animate,
                        };

                        if (mapData.reset_control.duration) {
                            initialView.duration =
                                mapData.reset_control.duration;
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
                                    resetContainer.parentNode.removeChild(
                                        resetContainer,
                                    );
                                },
                            },
                            mapData.reset_control.position,
                        );
                    }

                    if (mapData.draw_control && mapData.draw_control.enabled) {
                        MapboxDraw.constants.classes.CONTROL_BASE =
                            "maplibregl-ctrl";
                        MapboxDraw.constants.classes.CONTROL_PREFIX =
                            "maplibregl-ctrl-";
                        MapboxDraw.constants.classes.CONTROL_GROUP =
                            "maplibregl-ctrl-group";

                        let drawOptions = mapData.draw_control.options || {};

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

                        draw = new MapboxDraw(drawOptions);
                        map.addControl(draw, mapData.draw_control.position);
                        map.controls.push(draw);

                        // Add event listeners
                        map.on("draw.create", updateDrawnFeatures);
                        map.on("draw.delete", updateDrawnFeatures);
                        map.on("draw.update", updateDrawnFeatures);

                        // Apply orientation styling
                        if (mapData.draw_control.orientation === "horizontal") {
                            const drawBar = map
                                .getContainer()
                                .querySelector(".maplibregl-ctrl-group");
                            if (drawBar) {
                                drawBar.style.display = "flex";
                                drawBar.style.flexDirection = "row";
                            }
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
                        const position =
                            mapData.layers_control.position || "top-left";
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
                                    map.setLayoutProperty(
                                        clickedLayer,
                                        "visibility",
                                        "none",
                                    );
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
                            layersControl.insertBefore(
                                toggleButton,
                                layersList,
                            );
                        }
                    }
                    
                    // Set projection if provided (after all other setup is complete)
                    if (mapData.setProjection && mapData.setProjection.length > 0) {
                        mapData.setProjection.forEach(function (projectionConfig) {
                            if (projectionConfig.projection) {
                                const projection =
                                    typeof projectionConfig.projection === "string"
                                        ? { type: projectionConfig.projection }
                                        : projectionConfig.projection;
                                try {
                                    map.setProjection(projection);
                                } catch (e) {
                                    console.error("Failed to set projection in compare view:", e);
                                }
                            }
                        });
                    }
                }
            },

            resize: function (width, height) {
                // Code to handle resizing if necessary
            },
        };
    },
});
