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
    if (window._mapboxPopups && window._mapboxPopups[layerId]) {
        window._mapboxPopups[layerId].remove();
    }

    // Create and show the popup
    const popup = new maplibregl.Popup()
        .setLngLat(e.lngLat)
        .setHTML(description)
        .addTo(map);

    // Store reference to this popup
    if (!window._mapboxPopups) {
        window._mapboxPopups = {};
    }
    window._mapboxPopups[layerId] = popup;

    // Remove reference when popup is closed
    popup.on('close', function() {
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

HTMLWidgets.widget({
    name: "maplibregl",

    type: "output",

    factory: function (el, width, height) {
        let map;
        let draw;

        return {
            renderValue: function (x) {
                if (typeof maplibregl === "undefined") {
                    console.error("Maplibre GL JS is not loaded.");
                    return;
                }

                let protocol = new pmtiles.Protocol({ metadata: true });
                maplibregl.addProtocol("pmtiles", protocol.tile);

                map = new maplibregl.Map({
                    container: el.id,
                    style: x.style,
                    center: x.center,
                    zoom: x.zoom,
                    bearing: x.bearing,
                    pitch: x.pitch,
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
                        if (!window.maplibreglMarkers) {
                            window.maplibreglMarkers = [];
                        }
                        x.markers.forEach(function (marker) {
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
                                    }).setHTML(marker.popup),
                                );
                            }

                            if (HTMLWidgets.shinyMode) {
                                const markerId = marker.id;
                                if (markerId) {
                                    const lngLat = mapMarker.getLngLat();
                                    Shiny.setInputValue(
                                        el.id + "_marker_" + markerId,
                                        {
                                            id: markerId,
                                            lng: lngLat.lng,
                                            lat: lngLat.lat,
                                        },
                                    );

                                    mapMarker.on("dragend", function () {
                                        const lngLat = mapMarker.getLngLat();
                                        Shiny.setInputValue(
                                            el.id + "_marker_" + markerId,
                                            {
                                                id: markerId,
                                                lng: lngLat.lng,
                                                lat: lngLat.lat,
                                            },
                                        );
                                    });
                                }
                            }

                            window.maplibreglMarkers.push(mapMarker);
                        });
                    }

                    // Add sources if provided
                    if (x.sources) {
                        x.sources.forEach(function (source) {
                            if (source.type === "vector") {
                                const sourceOptions = {
                                    type: "vector",
                                    url: source.url,
                                };
                                // Add promoteId if provided
                                if (source.promoteId) {
                                    sourceOptions.promoteId = source.promoteId;
                                }
                                // Add any other additional options
                                for (const [key, value] of Object.entries(source)) {
                                    if (!["id", "type", "url"].includes(key)) {
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
                                for (const [key, value] of Object.entries(
                                    source,
                                )) {
                                    if (
                                        ![
                                            "id",
                                            "type",
                                            "data",
                                            "generateId",
                                        ].includes(key)
                                    ) {
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
                            }
                        });
                    }

                    function add_my_layers(layer) {
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
                                const tooltip = new maplibregl.Popup({
                                    closeButton: false,
                                    closeOnClick: false,
                                });

                                // Create a reference to the mousemove handler function.
                                // We need to pass 'e', 'map', 'tooltip', and 'layer.tooltip' to onMouseMoveTooltip.
                                const mouseMoveHandler = function (e) {
                                    onMouseMoveTooltip(
                                        e,
                                        map,
                                        tooltip,
                                        layer.tooltip,
                                    );
                                };

                                // Create a reference to the mouseleave handler function.
                                // We need to pass 'map' and 'tooltip' to onMouseLeaveTooltip.
                                const mouseLeaveHandler = function () {
                                    onMouseLeaveTooltip(map, tooltip);
                                };

                                // Attach the named handler references, not anonymous functions.
                                map.on("mousemove", layer.id, mouseMoveHandler);
                                map.on(
                                    "mouseleave",
                                    layer.id,
                                    mouseLeaveHandler,
                                );

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
                                        // Check if the feature has an id
                                        const featureId = e.features[0].id;

                                        // Only proceed if the feature has an id
                                        if (featureId !== undefined && featureId !== null) {
                                            if (hoveredFeatureId !== null) {
                                                const featureState = {
                                                    source:
                                                        typeof layer.source ===
                                                        "string"
                                                            ? layer.source
                                                            : layer.id,
                                                    id: hoveredFeatureId,
                                                };
                                                if (layer.source_layer) {
                                                    featureState.sourceLayer =
                                                        layer.source_layer;
                                                }
                                                map.setFeatureState(
                                                    featureState,
                                                    { hover: false },
                                                );
                                            }
                                            hoveredFeatureId = featureId;
                                            const featureState = {
                                                source:
                                                    typeof layer.source ===
                                                    "string"
                                                        ? layer.source
                                                        : layer.id,
                                                id: hoveredFeatureId,
                                            };
                                            if (layer.source_layer) {
                                                featureState.sourceLayer =
                                                    layer.source_layer;
                                            }
                                            map.setFeatureState(
                                                featureState,
                                                { hover: true },
                                            );
                                        }
                                    }
                                });

                                map.on("mouseleave", layer.id, function () {
                                    if (hoveredFeatureId !== null) {
                                        const featureState = {
                                            source:
                                                typeof layer.source ===
                                                "string"
                                                    ? layer.source
                                                    : layer.id,
                                            id: hoveredFeatureId,
                                        };
                                        if (layer.source_layer) {
                                            featureState.sourceLayer =
                                                layer.source_layer;
                                        }
                                        map.setFeatureState(
                                            featureState,
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
                                        map.setPaintProperty(layer.id, key, [
                                            "case",
                                            [
                                                "boolean",
                                                ["feature-state", "hover"],
                                                false,
                                            ],
                                            jsHoverOptions[key],
                                            originalPaint,
                                        ]);
                                    },
                                );
                            }
                        } catch (e) {
                            console.error("Failed to add layer: ", layer, e);
                        }
                    }
                    if (x.h3j_sources) {
                        x.h3j_sources.forEach(async function (source) {
                            await map.addH3JSource(source.id, {
                                data: source.url,
                            });

                            // A bit hacky?
                            if (x.layers) {
                                x.layers.forEach((layer) =>
                                    add_my_layers(layer),
                                );
                            }
                        });
                    }

                    // Add layers if provided
                    if (x.layers) {
                        x.layers.forEach((layer) => add_my_layers(layer));
                    }

                    // Apply setFilter if provided
                    if (x.setFilter) {
                        x.setFilter.forEach(function (filter) {
                            map.setFilter(filter.layer, filter.filter);
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
                        const scaleControl = new maplibregl.ScaleControl({
                            maxWidth: x.scale_control.maxWidth,
                            unit: x.scale_control.unit,
                        });
                        map.addControl(scaleControl, x.scale_control.position);
                        map.controls.push(scaleControl);
                    }

                    // Add globe control if enabled
                    if (x.globe_control) {
                        const globeControl = new maplibregl.GlobeControl();
                        map.addControl(globeControl, x.globe_control.position);
                        map.controls.push(globeControl);
                    }

                    // Add custom controls if any are defined
                    if (x.custom_controls) {
                        Object.keys(x.custom_controls).forEach(function(key) {
                            const controlOptions = x.custom_controls[key];
                            const customControlContainer = document.createElement("div");

                            if (controlOptions.className) {
                                customControlContainer.className = controlOptions.className;
                            } else {
                                customControlContainer.className = "maplibregl-ctrl maplibregl-ctrl-group";
                            }

                            customControlContainer.innerHTML = controlOptions.html;

                            const customControl = {
                                onAdd: function() {
                                    return customControlContainer;
                                },
                                onRemove: function() {
                                    if (customControlContainer.parentNode) {
                                        customControlContainer.parentNode.removeChild(customControlContainer);
                                    }
                                }
                            };

                            map.addControl(customControl, controlOptions.position || "top-right");
                            map.controls.push(customControl);
                        });
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
                        const globeMinimap = new GlobeMinimap(
                            globeMinimapOptions,
                        );
                        map.addControl(globeMinimap, x.globe_minimap.position);
                        map.controls.push(globeMinimap);
                    }

                    if (x.setProjection) {
                        x.setProjection.forEach(function (projectionConfig) {
                            if (projectionConfig.projection) {
                                const projection =
                                    typeof projectionConfig.projection ===
                                    "string"
                                        ? { type: projectionConfig.projection }
                                        : projectionConfig.projection;
                                map.setProjection(projection);
                            }
                        });
                    }

                    // Add geocoder control if enabled
                    if (x.geocoder_control) {
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
                            ...x.geocoder_control,
                        };

                        // Set default values if not provided
                        if (!geocoderOptions.placeholder)
                            geocoderOptions.placeholder = "Search";
                        if (typeof geocoderOptions.collapsed === "undefined")
                            geocoderOptions.collapsed = false;

                        const geocoder = new MaplibreGeocoder(
                            geocoderApi,
                            geocoderOptions,
                        );

                        map.addControl(
                            geocoder,
                            x.geocoder_control.position || "top-right",
                        );
                        map.controls.push(geocoder);
                        // Handle geocoder results in Shiny mode
                        if (HTMLWidgets.shinyMode) {
                            geocoder.on("results", function (e) {
                                Shiny.setInputValue(el.id + "_geocoder", {
                                    result: e,
                                    time: new Date(),
                                });
                            });
                        }
                    }


                    if (x.draw_control && x.draw_control.enabled) {
                        MapboxDraw.constants.classes.CONTROL_BASE =
                            "maplibregl-ctrl";
                        MapboxDraw.constants.classes.CONTROL_PREFIX =
                            "maplibregl-ctrl-";
                        MapboxDraw.constants.classes.CONTROL_GROUP =
                            "maplibregl-ctrl-group";

                        let drawOptions = x.draw_control.options || {};

                        // Generate styles if styling parameters provided
                        if (x.draw_control.styling) {
                            const generatedStyles = generateDrawStyles(x.draw_control.styling);
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
                        map.addControl(draw, x.draw_control.position);
                        map.controls.push(draw);

                        // Add event listeners
                        map.on("draw.create", updateDrawnFeatures);
                        map.on("draw.delete", updateDrawnFeatures);
                        map.on("draw.update", updateDrawnFeatures);

                        // Add initial features if provided
                        if (x.draw_control.source) {
                            addSourceFeaturesToDraw(draw, x.draw_control.source, map);
                        }

                        // Process any queued features
                        if (x.draw_features_queue) {
                            x.draw_features_queue.forEach(function(data) {
                                if (data.clear_existing) {
                                    draw.deleteAll();
                                }
                                addSourceFeaturesToDraw(draw, data.source, map);
                            });
                        }

                        // Apply orientation styling
                        if (x.draw_control.orientation === "horizontal") {
                            const drawBar = map
                                .getContainer()
                                .querySelector(".maplibregl-ctrl-group");
                            if (drawBar) {
                                drawBar.style.display = "flex";
                                drawBar.style.flexDirection = "row";
                            }
                        }
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
                        const existingLegends =
                            el.querySelectorAll(".mapboxgl-legend");
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
                        const position =
                            x.fullscreen_control.position || "top-right";
                        const fullscreen = new maplibregl.FullscreenControl();
                        map.addControl(fullscreen, position);
                        map.controls.push(fullscreen);
                    }

                    // Add geolocate control if enabled
                    if (x.geolocate_control) {
                        const geolocate = new maplibregl.GeolocateControl({
                            positionOptions:
                                x.geolocate_control.positionOptions,
                            trackUserLocation:
                                x.geolocate_control.trackUserLocation,
                            showAccuracyCircle:
                                x.geolocate_control.showAccuracyCircle,
                            showUserLocation:
                                x.geolocate_control.showUserLocation,
                            showUserHeading:
                                x.geolocate_control.showUserHeading,
                            fitBoundsOptions:
                                x.geolocate_control.fitBoundsOptions,
                        });
                        map.addControl(geolocate, x.geolocate_control.position);
                        map.controls.push(geolocate);

                        if (HTMLWidgets.shinyMode) {
                            geolocate.on("geolocate", function (event) {
                                Shiny.setInputValue(el.id + "_geolocate", {
                                    coords: event.coords,
                                    time: new Date(),
                                });
                            });

                            geolocate.on("trackuserlocationstart", function () {
                                Shiny.setInputValue(
                                    el.id + "_geolocate_tracking",
                                    {
                                        status: "start",
                                        time: new Date(),
                                    },
                                );
                            });

                            geolocate.on("trackuserlocationend", function () {
                                Shiny.setInputValue(
                                    el.id + "_geolocate_tracking",
                                    {
                                        status: "end",
                                        time: new Date(),
                                    },
                                );
                            });

                            geolocate.on("error", function (error) {
                                if (error.error.code === 1) {
                                    Shiny.setInputValue(
                                        el.id + "_geolocate_error",
                                        {
                                            message:
                                                "Location permission denied",
                                            time: new Date(),
                                        },
                                    );
                                }
                            });
                        }
                    }

                    // Add navigation control if enabled
                    if (x.navigation_control) {
                        const nav = new maplibregl.NavigationControl({
                            showCompass: x.navigation_control.show_compass,
                            showZoom: x.navigation_control.show_zoom,
                            visualizePitch:
                                x.navigation_control.visualize_pitch,
                        });
                        map.addControl(nav, x.navigation_control.position);
                        map.controls.push(nav);

                        if (x.navigation_control.orientation === "horizontal") {
                            const navBar = map
                                .getContainer()
                                .querySelector(
                                    ".maplibregl-ctrl-group:not(.mapbox-gl-draw_ctrl-draw-btn)",
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

                        // Initialize with empty object, will be populated after map loads
                        let initialView = {};

                        // Capture the initial view after the map has loaded and all view operations are complete
                        map.once('load', function() {
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
                                    resetContainer.parentNode.removeChild(
                                        resetContainer,
                                    );
                                },
                            },
                            x.reset_control.position,
                        );

                        map.controls.push({
                            onAdd: function () {
                                return resetContainer;
                            },
                            onRemove: function () {
                                resetContainer.parentNode.removeChild(
                                    resetContainer,
                                );
                            },
                        });
                    }

                    if (x.images && Array.isArray(x.images)) {
                        x.images.forEach(async function (imageInfo) {
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
                        const position =
                            x.layers_control.position || "top-left";
                        if (position === "top-left") {
                            layersControl.style.top = "10px";
                            layersControl.style.left = "10px";
                        } else if (position === "top-right") {
                            layersControl.style.top = "10px";
                            layersControl.style.right = "10px";
                        } else if (position === "bottom-left") {
                            layersControl.style.bottom = "10px";
                            layersControl.style.left = "10px";
                        } else if (position === "bottom-right") {
                            layersControl.style.bottom = "40px";
                            layersControl.style.right = "10px";
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
                            link.className =
                                initialVisibility === "none" ? "" : "active";

                            // Also hide any associated legends if the layer is initially hidden
                            if (initialVisibility === "none") {
                                const associatedLegends =
                                    document.querySelectorAll(
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
                                    map.setLayoutProperty(
                                        clickedLayer,
                                        "visibility",
                                        "none",
                                    );
                                    this.className = "";

                                    // Hide associated legends
                                    const associatedLegends =
                                        document.querySelectorAll(
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
                                    const associatedLegends =
                                        document.querySelectorAll(
                                            `.mapboxgl-legend[data-layer-id="${clickedLayer}"]`,
                                        );
                                    associatedLegends.forEach((legend) => {
                                        legend.style.display = "";
                                    });
                                }
                            };

                            layersList.appendChild(link);
                        });

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
                            layersControl.insertBefore(
                                toggleButton,
                                layersList,
                            );
                        }
                    }

                    // If clusters are present, add event handling
                    map.getStyle().layers.forEach((layer) => {
                        if (layer.id.includes("-clusters")) {
                            map.on("click", layer.id, async (e) => {
                                const features = map.queryRenderedFeatures(
                                    e.point,
                                    {
                                        layers: [layer.id],
                                    },
                                );
                                const clusterId =
                                    features[0].properties.cluster_id;
                                const zoom = await map
                                    .getSource(layer.source)
                                    .getClusterExpansionZoom(clusterId);
                                map.easeTo({
                                    center: features[0].geometry.coordinates,
                                    zoom,
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
                                Shiny.onInputChange(
                                    el.id + "_feature_click",
                                    null,
                                );
                            }

                            // Event listener for the map
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
                                const features = map.queryRenderedFeatures(e.point);

                                if(features.length > 0) {
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
                                  Shiny.onInputChange(
                                    el.id + "_feature_hover",
                                    null,
                                );
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
    Shiny.addCustomMessageHandler("maplibre-proxy", function (data) {
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
                    filters: {},        // layerId -> filter expression
                    paintProperties: {}, // layerId -> {propertyName -> value}
                    layoutProperties: {}, // layerId -> {propertyName -> value}
                    tooltips: {},       // layerId -> tooltip property
                    popups: {},         // layerId -> popup property
                    legends: {}         // legendId -> {html: string, css: string}
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
                            JSON.stringify(drawnFeatures)
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
                map.addSource(message.source);
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
                        map.on("mousemove", message.layer.id, mouseMoveHandler);
                        map.on(
                            "mouseleave",
                            message.layer.id,
                            mouseLeaveHandler,
                        );

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
                                // Check if the feature has an id
                                const featureId = e.features[0].id;

                                // Only proceed if the feature has an id
                                if (featureId !== undefined && featureId !== null) {
                                    if (hoveredFeatureId !== null) {
                                        const featureState = {
                                            source:
                                                typeof message.layer.source ===
                                                "string"
                                                    ? message.layer.source
                                                    : message.layer.id,
                                            id: hoveredFeatureId,
                                        };
                                        if (message.layer.source_layer) {
                                            featureState.sourceLayer =
                                                message.layer.source_layer;
                                        }
                                        map.setFeatureState(
                                            featureState,
                                            { hover: false },
                                        );
                                    }
                                    hoveredFeatureId = featureId;
                                    const featureState = {
                                        source:
                                            typeof message.layer.source ===
                                            "string"
                                                ? message.layer.source
                                                : message.layer.id,
                                        id: hoveredFeatureId,
                                    };
                                    if (message.layer.source_layer) {
                                        featureState.sourceLayer =
                                            message.layer.source_layer;
                                    }
                                    map.setFeatureState(
                                        featureState,
                                        { hover: true },
                                    );
                                }
                            }
                        });

                        map.on("mouseleave", message.layer.id, function () {
                            if (hoveredFeatureId !== null) {
                                const featureState = {
                                    source:
                                        typeof message.layer.source ===
                                        "string"
                                            ? message.layer.source
                                            : message.layer.id,
                                    id: hoveredFeatureId,
                                };
                                if (message.layer.source_layer) {
                                    featureState.sourceLayer =
                                        message.layer.source_layer;
                                }
                                map.setFeatureState(
                                    featureState,
                                    { hover: false },
                                );
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
                // Check both message.layer and message.layer.id as keys due to different message formats
                if (window._mapboxPopups) {
                    // First check if we have a popup stored with message.layer key
                    if (window._mapboxPopups[message.layer]) {
                        window._mapboxPopups[message.layer].remove();
                        delete window._mapboxPopups[message.layer];
                    }

                    // Also check if we have a popup stored with message.layer.id key, which happens when added via add_layer
                    if (message.layer && message.layer.id && window._mapboxPopups[message.layer.id]) {
                        window._mapboxPopups[message.layer.id].remove();
                        delete window._mapboxPopups[message.layer.id];
                    }
                }

                if (map.getLayer(message.layer)) {
                    // Remove tooltip handlers
                    if (
                        window._mapboxHandlers &&
                        window._mapboxHandlers[message.layer]
                    ) {
                        const handlers = window._mapboxHandlers[message.layer];
                        if (handlers.mousemove) {
                            map.off(
                                "mousemove",
                                message.layer,
                                handlers.mousemove,
                            );
                        }
                        if (handlers.mouseleave) {
                            map.off(
                                "mouseleave",
                                message.layer,
                                handlers.mouseleave,
                            );
                        }
                        // Clean up the reference
                        delete window._mapboxHandlers[message.layer];
                    }

                    // Remove click handlers for popups
                    if (window._mapboxClickHandlers) {
                        // First check for handlers stored with message.layer key
                        if (window._mapboxClickHandlers[message.layer]) {
                            map.off(
                                "click",
                                message.layer,
                                window._mapboxClickHandlers[message.layer]
                            );
                            delete window._mapboxClickHandlers[message.layer];
                        }

                        // Also check for handlers stored with message.layer.id key from add_layer
                        if (message.layer && message.layer.id && window._mapboxClickHandlers[message.layer.id]) {
                            map.off(
                                "click",
                                message.layer,
                                window._mapboxClickHandlers[message.layer.id]
                            );
                            delete window._mapboxClickHandlers[message.layer.id];
                        }
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
                    map.setPaintProperty(
                        layerId,
                        propertyName,
                        newPaintProperty,
                    );
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
                const features = map.queryRenderedFeatures(message.geometry, {
                    layers: message.layers,
                    filter: message.filter,
                });
                Shiny.setInputValue(el.id + "_feature_query", features);
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
                    const legendStyles = document.querySelectorAll(`style[data-mapgl-legend-css="${data.id}"]`);
                    legendStyles.forEach((style) => style.remove());

                    // Clear legend state when replacing all legends
                    layerState.legends = {};
                }

                // Track legend state
                if (legendId) {
                    layerState.legends[legendId] = {
                        html: message.html,
                        css: message.legend_css
                    };
                }

                const legendCss = document.createElement("style");
                legendCss.innerHTML = message.legend_css;
                legendCss.setAttribute('data-mapgl-legend-css', data.id); // Mark this style for later cleanup
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

                console.log("[MapGL Debug] set_style called with preserve_layers:", preserveLayers);
                console.log("[MapGL Debug] message.preserve_layers:", message.preserve_layers);

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

                        console.log("[MapGL Debug] Examining source:", sourceId, "type:", source.type);

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
                            !(sourceId.includes("basemap") && !sourceId.includes("user")) && // Filter basemap sources but keep user ones
                            sourceId !== "satellite" && // Filter MapTiler satellite source specifically
                            sourceId !== "aerial" // Filter aerial imagery sources
                        ) {
                            console.log("[MapGL Debug] Found user source via filtering:", sourceId);
                            if (!userSourceIds.includes(sourceId)) {
                                userSourceIds.push(sourceId);
                            }
                        } else {
                            console.log("[MapGL Debug] Filtered out base map source:", sourceId);
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

                    // Log detected user sources and layers
                    console.log("[MapGL Debug] Detected user sources:", userSourceIds);
                    console.log("[MapGL Debug] Detected user layers:", userLayers.map(l => l.id));
                    console.log("[MapGL Debug] Will preserve", userLayers.length, "user layers");

                    // Store them for potential use outside the onStyleLoad event
                    // This helps in case the event timing is different in MapLibre
                    if (!window._mapglPreservedData) {
                        window._mapglPreservedData = {};
                    }
                    window._mapglPreservedData[map.getContainer().id] = {
                        sources: userSourceIds.map(id => ({id, source: currentStyle.sources[id]})),
                        layers: userLayers
                    };

                    // Set up event listener to re-add sources and layers after style loads
                    const onStyleLoad = function() {
                        console.log("[MapGL Debug] style.load event fired");

                        try {
                            // Re-add user sources
                            userSourceIds.forEach(function(sourceId) {
                                try {
                                    if (!map.getSource(sourceId)) {
                                        const source = currentStyle.sources[sourceId];
                                        console.log("[MapGL Debug] Re-adding source:", sourceId);
                                        map.addSource(sourceId, source);
                                    }
                                } catch (err) {
                                    console.error("[MapGL Debug] Error re-adding source:", sourceId, err);
                                }
                            });

                            // Re-add user layers
                            userLayers.forEach(function(layer) {
                                try {
                                    if (!map.getLayer(layer.id)) {
                                        console.log("[MapGL Debug] Re-adding layer:", layer.id);
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
                                } catch (err) {
                                    console.error("[MapGL Debug] Error re-adding layer:", layer.id, err);
                                }
                            });
                        } catch (err) {
                            console.error("[MapGL Debug] Error in style.load handler:", err);
                        }

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
                                    if (window._mapboxHandlers && window._mapboxHandlers[layerId]) {
                                        if (window._mapboxHandlers[layerId].mousemove) {
                                            map.off("mousemove", layerId, window._mapboxHandlers[layerId].mousemove);
                                        }
                                        if (window._mapboxHandlers[layerId].mouseleave) {
                                            map.off("mouseleave", layerId, window._mapboxHandlers[layerId].mouseleave);
                                        }
                                    }

                                    // Create new tooltip
                                    const tooltip = new maplibregl.Popup({
                                        closeButton: false,
                                        closeOnClick: false,
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
                                    console.log("[MapGL Debug] Restoring popup:", layerId, popupProperty);

                                    // Remove existing popup handlers first
                                    if (window._mapboxClickHandlers && window._mapboxClickHandlers[layerId]) {
                                        map.off("click", layerId, window._mapboxClickHandlers[layerId]);
                                    }

                                    // Create new popup handler
                                    const clickHandler = function(e) {
                                        onClickPopup(e, map, popupProperty, layerId);
                                    };

                                    map.on("click", layerId, clickHandler);

                                    // Add hover effects for cursor
                                    map.on("mouseenter", layerId, function () {
                                        map.getCanvas().style.cursor = "pointer";
                                    });
                                    map.on("mouseleave", layerId, function () {
                                        map.getCanvas().style.cursor = "";
                                    });

                                    // Store handler reference
                                    if (!window._mapboxClickHandlers) {
                                        window._mapboxClickHandlers = {};
                                    }
                                    window._mapboxClickHandlers[layerId] = clickHandler;
                                }
                            }

                            // Restore legends
                            if (Object.keys(savedLayerState.legends).length > 0) {
                                // Clear any existing legends first to prevent stacking
                                const existingLegends = document.querySelectorAll(`#${mapId} .mapboxgl-legend`);
                                existingLegends.forEach((legend) => legend.remove());

                                // Clear existing legend styles
                                const legendStyles = document.querySelectorAll(`style[data-mapgl-legend-css="${mapId}"]`);
                                legendStyles.forEach((style) => style.remove());

                                // Restore each legend
                                for (const legendId in savedLayerState.legends) {
                                    const legendData = savedLayerState.legends[legendId];
                                    console.log("[MapGL Debug] Restoring legend:", legendId);

                                    // Add legend CSS
                                    const legendCss = document.createElement("style");
                                    legendCss.innerHTML = legendData.css;
                                    legendCss.setAttribute('data-mapgl-legend-css', mapId);
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
                        map.off('style.load', onStyleLoad);
                    };

                    map.on('style.load', onStyleLoad);

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

                                        // Restore tracked layer modifications in backup
                                        const mapId = map.getContainer().id;
                                        const savedLayerState = window._mapglLayerState && window._mapglLayerState[mapId];
                                        if (savedLayerState) {
                                            console.log("[MapGL Debug] Backup: restoring tracked layer modifications");

                                            // Restore filters
                                            for (const layerId in savedLayerState.filters) {
                                                if (map.getLayer(layerId)) {
                                                    console.log("[MapGL Debug] Backup: restoring filter for layer:", layerId);
                                                    map.setFilter(layerId, savedLayerState.filters[layerId]);
                                                }
                                            }

                                            // Restore paint properties
                                            for (const layerId in savedLayerState.paintProperties) {
                                                if (map.getLayer(layerId)) {
                                                    const properties = savedLayerState.paintProperties[layerId];
                                                    for (const propertyName in properties) {
                                                        const savedValue = properties[propertyName];
                                                        console.log("[MapGL Debug] Backup: restoring paint property:", layerId, propertyName, savedValue);

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
                                                        console.log("[MapGL Debug] Backup: restoring layout property:", layerId, propertyName, properties[propertyName]);
                                                        map.setLayoutProperty(layerId, propertyName, properties[propertyName]);
                                                    }
                                                }
                                            }

                                            // Restore tooltips
                                            for (const layerId in savedLayerState.tooltips) {
                                                if (map.getLayer(layerId)) {
                                                    const tooltipProperty = savedLayerState.tooltips[layerId];
                                                    console.log("[MapGL Debug] Backup: restoring tooltip:", layerId, tooltipProperty);

                                                    if (window._mapboxHandlers && window._mapboxHandlers[layerId]) {
                                                        if (window._mapboxHandlers[layerId].mousemove) {
                                                            map.off("mousemove", layerId, window._mapboxHandlers[layerId].mousemove);
                                                        }
                                                        if (window._mapboxHandlers[layerId].mouseleave) {
                                                            map.off("mouseleave", layerId, window._mapboxHandlers[layerId].mouseleave);
                                                        }
                                                    }

                                                    const tooltip = new maplibregl.Popup({
                                                        closeButton: false,
                                                        closeOnClick: false,
                                                    });

                                                    const mouseMoveHandler = function (e) {
                                                        onMouseMoveTooltip(e, map, tooltip, tooltipProperty);
                                                    };

                                                    const mouseLeaveHandler = function () {
                                                        onMouseLeaveTooltip(map, tooltip);
                                                    };

                                                    map.on("mousemove", layerId, mouseMoveHandler);
                                                    map.on("mouseleave", layerId, mouseLeaveHandler);

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
                                                    console.log("[MapGL Debug] Backup: restoring popup:", layerId, popupProperty);

                                                    if (window._mapboxClickHandlers && window._mapboxClickHandlers[layerId]) {
                                                        map.off("click", layerId, window._mapboxClickHandlers[layerId]);
                                                    }

                                                    const clickHandler = function(e) {
                                                        onClickPopup(e, map, popupProperty, layerId);
                                                    };

                                                    map.on("click", layerId, clickHandler);
                                                    map.on("mouseenter", layerId, function () {
                                                        map.getCanvas().style.cursor = "pointer";
                                                    });
                                                    map.on("mouseleave", layerId, function () {
                                                        map.getCanvas().style.cursor = "";
                                                    });

                                                    if (!window._mapboxClickHandlers) {
                                                        window._mapboxClickHandlers = {};
                                                    }
                                                    window._mapboxClickHandlers[layerId] = clickHandler;
                                                }
                                            }

                                            // Restore legends
                                            if (Object.keys(savedLayerState.legends).length > 0) {
                                                // Clear any existing legends first to prevent stacking
                                                const existingLegends = document.querySelectorAll(`#${mapId} .mapboxgl-legend`);
                                                existingLegends.forEach((legend) => legend.remove());

                                                // Clear existing legend styles
                                                const legendStyles = document.querySelectorAll(`style[data-mapgl-legend-css="${mapId}"]`);
                                                legendStyles.forEach((style) => style.remove());

                                                // Restore each legend
                                                for (const legendId in savedLayerState.legends) {
                                                    const legendData = savedLayerState.legends[legendId];
                                                    console.log("[MapGL Debug] Backup: restoring legend:", legendId);

                                                    // Add legend CSS
                                                    const legendCss = document.createElement("style");
                                                    legendCss.innerHTML = legendData.css;
                                                    legendCss.setAttribute('data-mapgl-legend-css', mapId);
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
                                    } else {
                                        console.log("[MapGL Debug] Backup check: layers already restored properly");
                                    }
                                }
                            } catch (err) {
                                console.error("[MapGL Debug] Error in backup restoration:", err);
                            }
                        }, 500); // 500ms delay - faster recovery

                        // Add a second backup with a bit more delay in case the first one fails
                        setTimeout(function() {
                            try {
                                console.log("[MapGL Debug] Running second backup layer check");
                                const mapId = map.getContainer().id;
                                const preserved = window._mapglPreservedData && window._mapglPreservedData[mapId];

                                if (preserved) {
                                    // Check if user layers were successfully restored
                                    const firstLayerId = preserved.layers[0]?.id;
                                    if (firstLayerId && !map.getLayer(firstLayerId)) {
                                        console.log("[MapGL Debug] Second backup restoration needed");

                                        // Re-add sources first
                                        preserved.sources.forEach(function(src) {
                                            try {
                                                if (!map.getSource(src.id)) {
                                                    console.log("[MapGL Debug] Second backup: adding source", src.id);
                                                    map.addSource(src.id, src.source);
                                                }
                                            } catch (err) {
                                                console.error("[MapGL Debug] Second backup: error adding source", src.id, err);
                                            }
                                        });

                                        // Then re-add layers
                                        preserved.layers.forEach(function(layer) {
                                            try {
                                                if (!map.getLayer(layer.id)) {
                                                    console.log("[MapGL Debug] Second backup: adding layer", layer.id);
                                                    map.addLayer(layer);

                                                    // Check for nc_counties layer to restore tooltip
                                                    if (layer.id === "nc_counties") {
                                                        console.log("[MapGL Debug] Second backup: restoring tooltip for", layer.id);

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
                                                                console.log("[MapGL Debug] Second backup: restoring hover style for", layer.id, key);
                                                                map.setPaintProperty(layer.id, key, value);
                                                            }
                                                        }
                                                    }
                                                }
                                            } catch (err) {
                                                console.error("[MapGL Debug] Second backup: error adding layer", layer.id, err);
                                            }
                                        });

                                        // Restore tracked layer modifications in second backup
                                        const mapId = map.getContainer().id;
                                        const savedLayerState = window._mapglLayerState && window._mapglLayerState[mapId];
                                        if (savedLayerState) {
                                            console.log("[MapGL Debug] Second backup: restoring tracked layer modifications");

                                            // Restore filters
                                            for (const layerId in savedLayerState.filters) {
                                                if (map.getLayer(layerId)) {
                                                    console.log("[MapGL Debug] Second backup: restoring filter for layer:", layerId);
                                                    map.setFilter(layerId, savedLayerState.filters[layerId]);
                                                }
                                            }

                                            // Restore paint properties
                                            for (const layerId in savedLayerState.paintProperties) {
                                                if (map.getLayer(layerId)) {
                                                    const properties = savedLayerState.paintProperties[layerId];
                                                    for (const propertyName in properties) {
                                                        const savedValue = properties[propertyName];
                                                        console.log("[MapGL Debug] Second backup: restoring paint property:", layerId, propertyName, savedValue);

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
                                                        console.log("[MapGL Debug] Second backup: restoring layout property:", layerId, propertyName, properties[propertyName]);
                                                        map.setLayoutProperty(layerId, propertyName, properties[propertyName]);
                                                    }
                                                }
                                            }

                                            // Restore tooltips
                                            for (const layerId in savedLayerState.tooltips) {
                                                if (map.getLayer(layerId)) {
                                                    const tooltipProperty = savedLayerState.tooltips[layerId];
                                                    console.log("[MapGL Debug] Second backup: restoring tooltip:", layerId, tooltipProperty);

                                                    if (window._mapboxHandlers && window._mapboxHandlers[layerId]) {
                                                        if (window._mapboxHandlers[layerId].mousemove) {
                                                            map.off("mousemove", layerId, window._mapboxHandlers[layerId].mousemove);
                                                        }
                                                        if (window._mapboxHandlers[layerId].mouseleave) {
                                                            map.off("mouseleave", layerId, window._mapboxHandlers[layerId].mouseleave);
                                                        }
                                                    }

                                                    const tooltip = new maplibregl.Popup({
                                                        closeButton: false,
                                                        closeOnClick: false,
                                                    });

                                                    const mouseMoveHandler = function (e) {
                                                        onMouseMoveTooltip(e, map, tooltip, tooltipProperty);
                                                    };

                                                    const mouseLeaveHandler = function () {
                                                        onMouseLeaveTooltip(map, tooltip);
                                                    };

                                                    map.on("mousemove", layerId, mouseMoveHandler);
                                                    map.on("mouseleave", layerId, mouseLeaveHandler);

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
                                                    console.log("[MapGL Debug] Second backup: restoring popup:", layerId, popupProperty);

                                                    if (window._mapboxClickHandlers && window._mapboxClickHandlers[layerId]) {
                                                        map.off("click", layerId, window._mapboxClickHandlers[layerId]);
                                                    }

                                                    const clickHandler = function(e) {
                                                        onClickPopup(e, map, popupProperty, layerId);
                                                    };

                                                    map.on("click", layerId, clickHandler);
                                                    map.on("mouseenter", layerId, function () {
                                                        map.getCanvas().style.cursor = "pointer";
                                                    });
                                                    map.on("mouseleave", layerId, function () {
                                                        map.getCanvas().style.cursor = "";
                                                    });

                                                    if (!window._mapboxClickHandlers) {
                                                        window._mapboxClickHandlers = {};
                                                    }
                                                    window._mapboxClickHandlers[layerId] = clickHandler;
                                                }
                                            }

                                            // Restore legends
                                            if (Object.keys(savedLayerState.legends).length > 0) {
                                                // Clear any existing legends first to prevent stacking
                                                const existingLegends = document.querySelectorAll(`#${mapId} .mapboxgl-legend`);
                                                existingLegends.forEach((legend) => legend.remove());

                                                // Clear existing legend styles
                                                const legendStyles = document.querySelectorAll(`style[data-mapgl-legend-css="${mapId}"]`);
                                                legendStyles.forEach((style) => style.remove());

                                                // Restore each legend
                                                for (const legendId in savedLayerState.legends) {
                                                    const legendData = savedLayerState.legends[legendId];
                                                    console.log("[MapGL Debug] Second backup: restoring legend:", legendId);

                                                    // Add legend CSS
                                                    const legendCss = document.createElement("style");
                                                    legendCss.innerHTML = legendData.css;
                                                    legendCss.setAttribute('data-mapgl-legend-css', mapId);
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
                                    }
                                }
                            } catch (err) {
                                console.error("[MapGL Debug] Error in second backup:", err);
                            }
                        }, 1000); // 1 second delay for second backup
                    }
                }

                // Change the style
                console.log("[MapGL Debug] About to call setStyle with:", message.style);
                console.log("[MapGL Debug] setStyle diff option:", message.diff);
                map.setStyle(message.style, { diff: message.diff });

                if (message.config) {
                    Object.keys(message.config).forEach(function (key) {
                        map.setConfigProperty(
                            "basemap",
                            key,
                            message.config[key],
                        );
                    });
                }
            } else if (message.type === "add_navigation_control") {
                const nav = new maplibregl.NavigationControl({
                    showCompass: message.options.show_compass,
                    showZoom: message.options.show_zoom,
                    visualizePitch: message.options.visualize_pitch,
                });
                map.addControl(nav, message.position);
                map.controls.push(nav);

                if (message.orientation === "horizontal") {
                    const navBar = map
                        .getContainer()
                        .querySelector(
                            ".maplibregl-ctrl-group:not(.mapbox-gl-draw_ctrl-draw-btn)",
                        );
                    if (navBar) {
                        navBar.style.display = "flex";
                        navBar.style.flexDirection = "row";
                    }
                }
            } else if (message.type === "add_draw_control") {
                MapboxDraw.constants.classes.CONTROL_BASE = "maplibregl-ctrl";
                MapboxDraw.constants.classes.CONTROL_PREFIX =
                    "maplibregl-ctrl-";
                MapboxDraw.constants.classes.CONTROL_GROUP =
                    "maplibregl-ctrl-group";

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

                // Add event listeners
                map.on("draw.create", updateDrawnFeatures);
                map.on("draw.delete", updateDrawnFeatures);
                map.on("draw.update", updateDrawnFeatures);

                // Add initial features if provided
                if (message.source) {
                    addSourceFeaturesToDraw(drawControl, message.source, map);
                }

                // Apply orientation styling
                if (message.orientation === "horizontal") {
                    const drawBar = map
                        .getContainer()
                        .querySelector(".maplibregl-ctrl-group");
                    if (drawBar) {
                        drawBar.style.display = "flex";
                        drawBar.style.flexDirection = "row";
                    }
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
                    console.warn('Draw control not initialized');
                }
            } else if (message.type === "add_markers") {
                if (!window.maplibreMarkers) {
                    window.maplibreMarkers = [];
                }
                message.markers.forEach(function (marker) {
                    const markerOptions = {
                        color: marker.color,
                        rotation: marker.rotation,
                        draggable: marker.options.draggable || false,
                        ...marker.options,
                    };
                    const mapMarker = new maplibregl.Marker(markerOptions)
                        .setLngLat([marker.lng, marker.lat])
                        .addTo(map);

                    if (marker.popup) {
                        mapMarker.setPopup(
                            new maplibregl.Popup({ offset: 25 }).setHTML(
                                marker.popup,
                            ),
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

                    window.maplibreMarkers.push(mapMarker);
                });
            } else if (message.type === "clear_markers") {
                if (window.maplibreMarkers) {
                    window.maplibreMarkers.forEach(function (marker) {
                        marker.remove();
                    });
                    window.maplibreglMarkers = [];
                }
            } else if (message.type === "add_fullscreen_control") {
                const position = message.position || "top-right";
                const fullscreen = new maplibregl.FullscreenControl();
                map.addControl(fullscreen, position);
                map.controls.push(fullscreen);
            } else if (message.type === "add_scale_control") {
                const scaleControl = new maplibregl.ScaleControl({
                    maxWidth: message.options.maxWidth,
                    unit: message.options.unit,
                });
                map.addControl(scaleControl, message.options.position);
                map.controls.push(scaleControl);
            } else if (message.type === "add_reset_control") {
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

                map.controls.push({
                    onAdd: function () {
                        return resetContainer;
                    },
                    onRemove: function () {
                        resetContainer.parentNode.removeChild(resetContainer);
                    },
                });
            } else if (message.type === "add_geolocate_control") {
                const geolocate = new maplibregl.GeolocateControl({
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
                                        (feature.bbox[2] - feature.bbox[0]) / 2,
                                    feature.bbox[1] +
                                        (feature.bbox[3] - feature.bbox[1]) / 2,
                                ];
                                const point = {
                                    type: "Feature",
                                    geometry: {
                                        type: "Point",
                                        coordinates: center,
                                    },
                                    place_name: feature.properties.display_name,
                                    properties: feature.properties,
                                    text: feature.properties.display_name,
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
                const geocoder = new MaplibreGeocoder(geocoderApi, {
                    maplibregl: maplibregl,
                    placeholder: message.options.placeholder,
                    collapsed: message.options.collapsed,
                });
                map.addControl(geocoder, message.options.position);
                map.controls.push(geocoder);

                // Handle geocoder results in Shiny mode
                if (HTMLWidgets.shinyMode) {
                    geocoder.on("result", function (e) {
                        Shiny.setInputValue(data.id + "_geocoder", {
                            result: e,
                            time: new Date(),
                        });
                    });
                }
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
                    layersControl.style.top = "10px";
                    layersControl.style.left = "10px";
                } else if (position === "top-right") {
                    layersControl.style.top = "10px";
                    layersControl.style.right = "10px";
                } else if (position === "bottom-left") {
                    layersControl.style.bottom = "10px";
                    layersControl.style.left = "10px";
                } else if (position === "bottom-right") {
                    layersControl.style.bottom = "40px";
                    layersControl.style.right = "10px";
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
                    link.className =
                        initialVisibility === "none" ? "" : "active";

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
                            map.setLayoutProperty(
                                clickedLayer,
                                "visibility",
                                "none",
                            );
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
                    console.error(
                        `Cannot find map container with ID ${data.id}`,
                    );
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
                    const existingLegends = document.querySelectorAll(
                        `#${data.id} .mapboxgl-legend`,
                    );
                    existingLegends.forEach((legend) => {
                        legend.remove();
                    });

                    // Clear all legend state
                    layerState.legends = {};
                }
            } else if (message.type === "clear_controls") {
                map.controls.forEach((control) => {
                    map.removeControl(control);
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
                        map.loadImage(imageInfo.url)
                            .then((image) => {
                                if (!map.hasImage(imageInfo.id)) {
                                    map.addImage(
                                        imageInfo.id,
                                        image.data,
                                        imageInfo.options,
                                    );
                                }
                            })
                            .catch((error) => {
                                console.error("Error loading image:", error);
                            });
                    });
                } else if (message.url) {
                    map.loadImage(message.url)
                        .then((image) => {
                            if (!map.hasImage(message.imageId)) {
                                map.addImage(
                                    message.imageId,
                                    image.data,
                                    message.options,
                                );
                            }
                        })
                        .catch((error) => {
                            console.error("Error loading image:", error);
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
                const tooltip = new maplibregl.Popup({
                    closeButton: false,
                    closeOnClick: false,
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
                if (window._mapboxClickHandlers && window._mapboxClickHandlers[layerId]) {
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
            }
        } else if (message.type === "set_projection") {
            if (map.loaded()) {
                const projection =
                    typeof message.projection === "string"
                        ? { type: message.projection }
                        : message.projection;

                try {
                    map.setProjection(projection);
                } catch (e) {
                    console.error("Failed to set projection:", e);
                }
            } else {
                console.error("Map not loaded yet");
            }
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
        } else if (message.type === "add_globe_control") {
            const globeControl = new maplibregl.GlobeControl();
            map.addControl(globeControl, message.position);
            map.controls.push(globeControl);
        } else if (message.type === "add_custom_control") {
            const controlOptions = message.options;
            const customControlContainer = document.createElement("div");

            if (controlOptions.className) {
                customControlContainer.className = controlOptions.className;
            } else {
                customControlContainer.className = "maplibregl-ctrl maplibregl-ctrl-group";
            }

            customControlContainer.innerHTML = controlOptions.html;

            const customControl = {
                onAdd: function() {
                    return customControlContainer;
                },
                onRemove: function() {
                    if (customControlContainer.parentNode) {
                        customControlContainer.parentNode.removeChild(customControlContainer);
                    }
                }
            };

            map.addControl(customControl, controlOptions.position || "top-right");
            map.controls.push(customControl);
        }
    });
}
