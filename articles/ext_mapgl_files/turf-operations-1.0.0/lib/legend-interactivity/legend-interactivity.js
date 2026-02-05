/**
 * Legend Interactivity Module for mapgl
 * Shared functionality for both Mapbox GL and MapLibre GL
 *
 * Provides interactive legend capabilities:
 * - Categorical legends: Click to toggle category visibility
 * - Continuous legends: Dual-handle range slider for filtering
 */

/**
 * Main initialization function - called from widget code
 * @param {Object} map - The map instance (mapboxgl or maplibre)
 * @param {string} mapId - The container ID for the map
 * @param {Object} config - Configuration from R side
 */
function initializeLegendInteractivity(map, mapId, config) {
    var legendElement = document.getElementById(config.legendId);
    if (!legendElement) {
        console.warn("Legend element not found:", config.legendId);
        return;
    }

    // Ensure layer state tracking exists
    if (!window._mapglLayerState) {
        window._mapglLayerState = {};
    }
    if (!window._mapglLayerState[mapId]) {
        window._mapglLayerState[mapId] = {
            filters: {},
            paintProperties: {},
            layoutProperties: {},
            tooltips: {},
            popups: {},
            legends: {},
            interactiveFilters: {}
        };
    }

    var layerState = window._mapglLayerState[mapId];

    // Initialize interactive filter state for this layer
    if (config.layerId && !layerState.interactiveFilters[config.layerId]) {
        var originalFilter = null;
        try {
            originalFilter = map.getFilter(config.layerId);
        } catch (e) {
            // Layer may not exist yet
        }
        layerState.interactiveFilters[config.layerId] = {
            originalFilter: originalFilter,
            legendFilters: {}
        };
    }

    // Determine filter column - use provided or auto-detect
    var filterColumn = config.filterColumn;
    if (!filterColumn && config.layerId) {
        filterColumn = detectFilterColumn(map, config.layerId);
    }

    if (!filterColumn) {
        console.warn(
            "Could not determine filter column for interactive legend. " +
                "Please provide filter_column parameter."
        );
        return;
    }

    // Store config for later reference
    legendElement._interactivityConfig = {
        legendId: config.legendId,
        layerId: config.layerId,
        type: config.type,
        values: config.values,
        colors: config.colors,
        filterColumn: filterColumn,
        mapId: mapId
    };

    if (config.type === "categorical") {
        initCategoricalLegend(map, mapId, legendElement, filterColumn, config);
    } else if (config.type === "continuous") {
        initContinuousLegend(map, mapId, legendElement, filterColumn, config);
    }
}

/**
 * Detect filter column from layer's paint expression
 * @param {Object} map - The map instance
 * @param {string} layerId - The layer ID
 * @returns {string|null} The detected column name or null
 */
function detectFilterColumn(map, layerId) {
    var layer = null;
    try {
        layer = map.getLayer(layerId);
    } catch (e) {
        return null;
    }
    if (!layer) return null;

    // Check common paint properties for expressions
    var paintProps = [
        "fill-color",
        "circle-color",
        "line-color",
        "fill-opacity",
        "circle-radius",
        "line-width",
        "fill-extrusion-color"
    ];

    for (var i = 0; i < paintProps.length; i++) {
        var prop = paintProps[i];
        try {
            var paintValue = map.getPaintProperty(layerId, prop);
            if (paintValue && Array.isArray(paintValue)) {
                var column = parseExpressionForColumn(paintValue);
                if (column) return column;
            }
        } catch (e) {
            // Property may not exist for this layer type
        }
    }

    return null;
}

/**
 * Parse expression to extract column name
 * Handles: match, interpolate, step, case expressions
 * @param {Array} expr - The expression array
 * @returns {string|null} The column name or null
 */
function parseExpressionForColumn(expr) {
    if (!Array.isArray(expr) || expr.length < 2) return null;

    var type = expr[0];

    // match: ["match", ["get", "column"], ...]
    if (type === "match" && Array.isArray(expr[1])) {
        if (expr[1][0] === "get" && typeof expr[1][1] === "string") {
            return expr[1][1];
        }
    }

    // interpolate: ["interpolate", ["linear"], ["get", "column"], ...]
    if (type === "interpolate" && expr.length >= 3) {
        var getExpr = expr[2];
        if (Array.isArray(getExpr) && getExpr[0] === "get") {
            return getExpr[1];
        }
    }

    // step: ["step", ["get", "column"], base, ...]
    if (type === "step" && Array.isArray(expr[1])) {
        if (expr[1][0] === "get" && typeof expr[1][1] === "string") {
            return expr[1][1];
        }
    }

    // case with na_color wrapper: ["case", ["==", ["get", "column"], null], na_color, inner_expr]
    if (type === "case" && expr.length >= 4) {
        // Check inner expression (last non-default element)
        for (var i = 3; i < expr.length; i++) {
            if (Array.isArray(expr[i])) {
                var result = parseExpressionForColumn(expr[i]);
                if (result) return result;
            }
        }
    }

    return null;
}

/**
 * Initialize categorical legend interactivity
 */
function initCategoricalLegend(map, mapId, legendElement, filterColumn, config) {
    var items = legendElement.querySelectorAll(".legend-item");

    // Use filterValues for actual filtering, values for display
    var filterValues = config.filterValues || config.values;
    var displayValues = config.values;
    var breaks = config.breaks; // For range-based filtering (from classifications)

    // Track enabled state by index (to map display values to filter values)
    var numCategories = displayValues.length;
    var enabledIndices = new Set();
    for (var i = 0; i < numCategories; i++) {
        enabledIndices.add(i);
    }

    var layerState = window._mapglLayerState[mapId];
    var interactiveState = layerState.interactiveFilters[config.layerId];

    // Store category state
    interactiveState.enabledIndices = enabledIndices;
    interactiveState.filterValues = filterValues;
    interactiveState.breaks = breaks;
    interactiveState.filterColumn = filterColumn;

    // Store original colors for each item
    var originalColors = {};

    // Add click handlers to each item
    items.forEach(function (item, idx) {
        var displayValue = item.getAttribute("data-value") || String(displayValues[idx]);
        item.setAttribute("data-value", displayValue);
        item.setAttribute("data-index", idx);
        item.setAttribute("data-enabled", "true");

        // Store original color - try multiple selectors for different patch types
        var colorSpan = item.querySelector(".legend-color") ||
                        item.querySelector(".legend-shape-svg") ||
                        item.querySelector(".legend-shape-custom");
        if (colorSpan) {
            originalColors[idx] = colorSpan.style.backgroundColor ||
                                  colorSpan.getAttribute("fill") ||
                                  config.colors[idx];
        } else {
            // Fallback to config colors
            originalColors[idx] = config.colors[idx];
        }

        item.addEventListener("click", function (e) {
            e.preventDefault();
            e.stopPropagation();

            var currentlyEnabled = item.getAttribute("data-enabled") === "true";
            var newState = !currentlyEnabled;

            item.setAttribute("data-enabled", String(newState));

            if (newState) {
                enabledIndices.add(idx);
                // Restore original color
                if (colorSpan) {
                    colorSpan.style.backgroundColor = originalColors[idx];
                }
            } else {
                enabledIndices.delete(idx);
                // Grey out
                if (colorSpan) {
                    colorSpan.style.backgroundColor = "#cccccc";
                }
            }

            // Apply filter - use breaks for range-based, filterValues for categorical
            if (breaks && breaks.length > 0) {
                applyRangeBasedCategoricalFilter(
                    map,
                    mapId,
                    config.layerId,
                    filterColumn,
                    enabledIndices,
                    breaks,
                    interactiveState.originalFilter
                );
            } else {
                applyCategoricalFilter(
                    map,
                    mapId,
                    config.layerId,
                    filterColumn,
                    enabledIndices,
                    filterValues,
                    interactiveState.originalFilter
                );
            }

            // Update reset button visibility
            updateResetButton(legendElement, enabledIndices.size < numCategories);

            // Send to Shiny if applicable
            if (typeof HTMLWidgets !== "undefined" && HTMLWidgets.shinyMode) {
                // Get enabled filter values for Shiny
                var enabledFilterValues = [];
                enabledIndices.forEach(function(i) {
                    enabledFilterValues.push(filterValues[i]);
                });
                Shiny.setInputValue(mapId + "_legend_filter", {
                    legendId: config.legendId,
                    layerId: config.layerId,
                    type: "categorical",
                    column: filterColumn,
                    enabledValues: enabledFilterValues,
                    timestamp: Date.now()
                });
            }
        });
    });

    // Add reset button
    addResetButton(legendElement, function () {
        items.forEach(function (item, i) {
            item.setAttribute("data-enabled", "true");
            var colorSpan = item.querySelector(".legend-color") ||
                            item.querySelector(".legend-shape-svg") ||
                            item.querySelector(".legend-shape-custom");
            if (colorSpan && originalColors[i] !== undefined) {
                colorSpan.style.backgroundColor = originalColors[i];
            }
        });
        enabledIndices.clear();
        for (var i = 0; i < numCategories; i++) {
            enabledIndices.add(i);
        }

        // Reset filter to original
        if (interactiveState.originalFilter) {
            map.setFilter(config.layerId, interactiveState.originalFilter);
        } else {
            map.setFilter(config.layerId, null);
        }
        layerState.filters[config.layerId] = interactiveState.originalFilter;

        updateResetButton(legendElement, false);

        // Send to Shiny
        if (typeof HTMLWidgets !== "undefined" && HTMLWidgets.shinyMode) {
            Shiny.setInputValue(mapId + "_legend_filter", {
                legendId: config.legendId,
                layerId: config.layerId,
                type: "categorical",
                column: filterColumn,
                enabledValues: filterValues.slice(),
                timestamp: Date.now()
            });
        }
    });
}

/**
 * Initialize continuous legend interactivity with gradient-overlay handles
 * Features:
 * - Vertical line handles positioned on the gradient bar
 * - Semi-transparent overlays on unselected regions
 * - Draggable middle region to pan the selection
 * - Piecewise linear interpolation for non-linear color ramps
 */
function initContinuousLegend(map, mapId, legendElement, filterColumn, config) {
    var values = config.values.map(Number);
    var minValue = Math.min.apply(null, values);
    var maxValue = Math.max.apply(null, values);

    var layerState = window._mapglLayerState[mapId];
    var interactiveState = layerState.interactiveFilters[config.layerId];

    // Store range state
    interactiveState.rangeMin = minValue;
    interactiveState.rangeMax = maxValue;
    interactiveState.originalMin = minValue;
    interactiveState.originalMax = maxValue;
    interactiveState.filterColumn = filterColumn;
    interactiveState.values = values; // Store all values for piecewise interpolation

    // Find the gradient bar
    var gradientBar = legendElement.querySelector(".legend-gradient");
    if (!gradientBar) {
        console.warn("No gradient bar found in legend for interactive continuous legend");
        return;
    }

    // Create the interactive overlay container
    var overlayContainer = document.createElement("div");
    overlayContainer.className = "legend-gradient-overlay-container";

    // Create left ghost overlay (unselected region)
    var leftOverlay = document.createElement("div");
    leftOverlay.className = "legend-gradient-ghost legend-gradient-ghost-left";

    // Create right ghost overlay (unselected region)
    var rightOverlay = document.createElement("div");
    rightOverlay.className = "legend-gradient-ghost legend-gradient-ghost-right";

    // Create left handle (min)
    var leftHandle = document.createElement("div");
    leftHandle.className = "legend-gradient-handle legend-gradient-handle-left";
    leftHandle.title = "Drag to set minimum value";

    // Create right handle (max)
    var rightHandle = document.createElement("div");
    rightHandle.className = "legend-gradient-handle legend-gradient-handle-right";
    rightHandle.title = "Drag to set maximum value";

    // Create middle draggable region
    var middleRegion = document.createElement("div");
    middleRegion.className = "legend-gradient-middle";
    middleRegion.title = "Drag to pan the selected range";

    overlayContainer.appendChild(leftOverlay);
    overlayContainer.appendChild(rightOverlay);
    overlayContainer.appendChild(leftHandle);
    overlayContainer.appendChild(rightHandle);
    overlayContainer.appendChild(middleRegion);

    // Insert overlay container into gradient bar's parent (positioned over gradient)
    gradientBar.style.position = "relative";
    gradientBar.appendChild(overlayContainer);

    // Create current range display below gradient
    var rangeDisplay = document.createElement("div");
    rangeDisplay.className = "legend-range-display";
    rangeDisplay.textContent = formatValue(minValue) + " — " + formatValue(maxValue);

    // Insert range display after gradient, before labels
    var labelsEl = legendElement.querySelector(".legend-labels");
    if (labelsEl) {
        labelsEl.parentNode.insertBefore(rangeDisplay, labelsEl);
    } else {
        gradientBar.parentNode.insertBefore(rangeDisplay, gradientBar.nextSibling);
    }

    // Current selection state (as percentages 0-100)
    var selectionState = {
        leftPercent: 0,
        rightPercent: 100
    };

    /**
     * Convert position percentage (0-100) to data value
     * Uses piecewise linear interpolation for non-linear color ramps
     */
    function positionToValue(percent) {
        if (values.length < 2) return values[0] || 0;

        // Clamp percent
        percent = Math.max(0, Math.min(100, percent));

        var segmentCount = values.length - 1;
        var segmentWidth = 100 / segmentCount;
        var segmentIndex = Math.min(
            Math.floor(percent / segmentWidth),
            segmentCount - 1
        );
        var localPercent = percent - segmentIndex * segmentWidth;
        var localFraction = localPercent / segmentWidth;

        var startVal = values[segmentIndex];
        var endVal = values[segmentIndex + 1];

        return startVal + localFraction * (endVal - startVal);
    }

    /**
     * Convert data value to position percentage (0-100)
     * Inverse of positionToValue
     */
    function valueToPosition(value) {
        if (values.length < 2) return 0;

        // Find which segment this value falls in
        for (var i = 0; i < values.length - 1; i++) {
            var startVal = values[i];
            var endVal = values[i + 1];
            var minSegVal = Math.min(startVal, endVal);
            var maxSegVal = Math.max(startVal, endVal);

            if (value >= minSegVal && value <= maxSegVal) {
                var segmentCount = values.length - 1;
                var segmentWidth = 100 / segmentCount;
                var localFraction = (value - startVal) / (endVal - startVal);
                return i * segmentWidth + localFraction * segmentWidth;
            }
        }

        // Fallback: linear interpolation across full range
        return ((value - minValue) / (maxValue - minValue)) * 100;
    }

    /**
     * Update visual elements based on current selection
     */
    function updateVisuals() {
        var left = selectionState.leftPercent;
        var right = selectionState.rightPercent;

        // Update ghost overlays
        leftOverlay.style.width = left + "%";
        rightOverlay.style.left = right + "%";
        rightOverlay.style.width = (100 - right) + "%";

        // Update handles
        leftHandle.style.left = left + "%";
        rightHandle.style.left = right + "%";

        // Update middle region
        middleRegion.style.left = left + "%";
        middleRegion.style.width = (right - left) + "%";

        // Update range display with actual values
        var minVal = positionToValue(left);
        var maxVal = positionToValue(right);
        rangeDisplay.textContent = formatValue(minVal) + " — " + formatValue(maxVal);
    }

    /**
     * Apply filter based on current selection
     */
    var filterTimeout;
    function applyFilterDebounced() {
        clearTimeout(filterTimeout);
        filterTimeout = setTimeout(function () {
            var minVal = positionToValue(selectionState.leftPercent);
            var maxVal = positionToValue(selectionState.rightPercent);

            interactiveState.rangeMin = minVal;
            interactiveState.rangeMax = maxVal;

            // Check if at full range (within tolerance) - if so, clear filter instead
            // This handles cases where legend breaks are rounded but data has more precision
            var isAtFullRange =
                selectionState.leftPercent <= 0.5 &&
                selectionState.rightPercent >= 99.5;

            if (isAtFullRange) {
                // Restore original filter (no range constraint)
                map.setFilter(config.layerId, interactiveState.originalFilter);
                var layerState = window._mapglLayerState[mapId];
                layerState.filters[config.layerId] = interactiveState.originalFilter;
            } else {
                applyRangeFilter(
                    map,
                    mapId,
                    config.layerId,
                    filterColumn,
                    minVal,
                    maxVal,
                    interactiveState.originalFilter
                );
            }

            // Update reset button visibility
            var hasFilter = !isAtFullRange;
            updateResetButton(legendElement, hasFilter);

            // Send to Shiny if applicable
            if (typeof HTMLWidgets !== "undefined" && HTMLWidgets.shinyMode) {
                Shiny.setInputValue(mapId + "_legend_filter", {
                    legendId: config.legendId,
                    layerId: config.layerId,
                    type: "continuous",
                    column: filterColumn,
                    range: [minVal, maxVal],
                    timestamp: Date.now()
                });
            }
        }, 50);
    }

    /**
     * Get mouse/touch position as percentage of gradient width
     */
    function getPositionPercent(e) {
        var rect = gradientBar.getBoundingClientRect();
        var clientX = e.touches ? e.touches[0].clientX : e.clientX;
        var x = clientX - rect.left;
        var percent = (x / rect.width) * 100;
        return Math.max(0, Math.min(100, percent));
    }

    // Drag state
    var dragState = {
        active: false,
        target: null, // 'left', 'right', or 'middle'
        startX: 0,
        startLeftPercent: 0,
        startRightPercent: 0
    };

    /**
     * Handle drag start
     */
    function onDragStart(e, target) {
        e.preventDefault();
        dragState.active = true;
        dragState.target = target;
        dragState.startX = e.touches ? e.touches[0].clientX : e.clientX;
        dragState.startLeftPercent = selectionState.leftPercent;
        dragState.startRightPercent = selectionState.rightPercent;

        document.addEventListener("mousemove", onDragMove);
        document.addEventListener("mouseup", onDragEnd);
        document.addEventListener("touchmove", onDragMove);
        document.addEventListener("touchend", onDragEnd);

        // Add dragging class for visual feedback
        overlayContainer.classList.add("dragging");
    }

    /**
     * Handle drag movement
     */
    function onDragMove(e) {
        if (!dragState.active) return;

        var percent = getPositionPercent(e);

        if (dragState.target === "left") {
            // Move left handle
            var newLeft = Math.max(
                0,
                Math.min(selectionState.rightPercent - 2, percent)
            );
            selectionState.leftPercent = newLeft;
        } else if (dragState.target === "right") {
            // Move right handle
            var newRight = Math.min(
                100,
                Math.max(selectionState.leftPercent + 2, percent)
            );
            selectionState.rightPercent = newRight;
        } else if (dragState.target === "middle") {
            // Pan both handles together
            var rangeWidth =
                dragState.startRightPercent - dragState.startLeftPercent;
            var currentX = e.touches ? e.touches[0].clientX : e.clientX;
            var rect = gradientBar.getBoundingClientRect();
            var deltaX = currentX - dragState.startX;
            var deltaPct = (deltaX / rect.width) * 100;

            var newLeft = dragState.startLeftPercent + deltaPct;
            var newRight = dragState.startRightPercent + deltaPct;

            // Clamp to bounds
            if (newLeft < 0) {
                newLeft = 0;
                newRight = rangeWidth;
            }
            if (newRight > 100) {
                newRight = 100;
                newLeft = 100 - rangeWidth;
            }

            selectionState.leftPercent = newLeft;
            selectionState.rightPercent = newRight;
        }

        updateVisuals();
        // Visual preview only during drag - filter applied on drag end
    }

    /**
     * Handle drag end - apply filter on release
     */
    function onDragEnd() {
        if (!dragState.active) return;

        dragState.active = false;
        dragState.target = null;

        document.removeEventListener("mousemove", onDragMove);
        document.removeEventListener("mouseup", onDragEnd);
        document.removeEventListener("touchmove", onDragMove);
        document.removeEventListener("touchend", onDragEnd);

        overlayContainer.classList.remove("dragging");

        // Apply filter now that drag is complete
        applyFilterDebounced();
    }

    // Attach event listeners
    leftHandle.addEventListener("mousedown", function (e) {
        onDragStart(e, "left");
    });
    leftHandle.addEventListener("touchstart", function (e) {
        onDragStart(e, "left");
    });

    rightHandle.addEventListener("mousedown", function (e) {
        onDragStart(e, "right");
    });
    rightHandle.addEventListener("touchstart", function (e) {
        onDragStart(e, "right");
    });

    middleRegion.addEventListener("mousedown", function (e) {
        onDragStart(e, "middle");
    });
    middleRegion.addEventListener("touchstart", function (e) {
        onDragStart(e, "middle");
    });

    // Initial visual update
    updateVisuals();

    // Add reset button
    addResetButton(legendElement, function () {
        selectionState.leftPercent = 0;
        selectionState.rightPercent = 100;
        updateVisuals();

        interactiveState.rangeMin = minValue;
        interactiveState.rangeMax = maxValue;

        // Reset filter to original
        if (interactiveState.originalFilter) {
            map.setFilter(config.layerId, interactiveState.originalFilter);
        } else {
            map.setFilter(config.layerId, null);
        }
        layerState.filters[config.layerId] = interactiveState.originalFilter;

        updateResetButton(legendElement, false);

        // Send to Shiny
        if (typeof HTMLWidgets !== "undefined" && HTMLWidgets.shinyMode) {
            Shiny.setInputValue(mapId + "_legend_filter", {
                legendId: config.legendId,
                layerId: config.layerId,
                type: "continuous",
                column: filterColumn,
                range: [minValue, maxValue],
                timestamp: Date.now()
            });
        }
    });
}

/**
 * Apply categorical filter to layer
 * @param enabledIndices - Set of enabled indices
 * @param filterValues - Array of actual data values to filter on
 */
function applyCategoricalFilter(
    map,
    mapId,
    layerId,
    column,
    enabledIndices,
    filterValues,
    originalFilter
) {
    var layerState = window._mapglLayerState[mapId];

    var interactiveFilter;
    if (enabledIndices.size === 0) {
        // No categories enabled - hide all features
        interactiveFilter = ["==", ["get", column], "__IMPOSSIBLE_VALUE__"];
    } else {
        // Build array of enabled filter values - preserve original types
        var enabledValues = [];
        enabledIndices.forEach(function(i) {
            if (filterValues[i] !== undefined) {
                // Don't convert to String - preserve numeric types for proper matching
                enabledValues.push(filterValues[i]);
            }
        });

        // Create "match" filter for enabled categories
        // Format: ["match", ["get", column], [values], true, false]
        interactiveFilter = [
            "match",
            ["get", column],
            enabledValues,
            true,
            false
        ];
    }

    // Combine with original filter if exists
    var finalFilter = combineFilters(originalFilter, interactiveFilter);

    map.setFilter(layerId, finalFilter);
    layerState.filters[layerId] = finalFilter;
}

/**
 * Apply range-based categorical filter (for binned/quantile classifications)
 * @param enabledIndices - Set of enabled bin indices
 * @param breaks - Array of break values [min, break1, break2, ..., max]
 */
function applyRangeBasedCategoricalFilter(
    map,
    mapId,
    layerId,
    column,
    enabledIndices,
    breaks,
    originalFilter
) {
    var layerState = window._mapglLayerState[mapId];

    // Calculate epsilon for floating-point precision and rounding issues
    // Use a larger epsilon (0.1% of range) to account for rounded legend breaks
    var overallMin = breaks[0];
    var overallMax = breaks[breaks.length - 1];
    var range = Math.abs(overallMax - overallMin);
    var epsilon = range > 0 ? range * 0.001 : 0.001;

    var interactiveFilter;
    if (enabledIndices.size === 0) {
        // No categories enabled - hide all features
        interactiveFilter = ["==", ["get", column], "__IMPOSSIBLE_VALUE__"];
    } else {
        // Build "any" filter with range conditions for each enabled bin
        var rangeConditions = [];
        enabledIndices.forEach(function(i) {
            if (i < breaks.length - 1) {
                var minVal = breaks[i];
                var maxVal = breaks[i + 1];
                // Each bin: value >= min AND value < max (except last bin uses <=)
                var isLastBin = (i === breaks.length - 2);
                // Apply epsilon adjustment for edge values to handle floating-point precision
                var filterMin = (i === 0) ? minVal - epsilon : minVal;
                var filterMax = isLastBin ? maxVal + epsilon : maxVal;
                var binCondition = [
                    "all",
                    [">=", ["get", column], filterMin],
                    isLastBin ? ["<=", ["get", column], filterMax] : ["<", ["get", column], filterMax]
                ];
                rangeConditions.push(binCondition);
            }
        });

        if (rangeConditions.length === 1) {
            interactiveFilter = rangeConditions[0];
        } else {
            interactiveFilter = ["any"].concat(rangeConditions);
        }
    }

    // Combine with original filter if exists
    var finalFilter = combineFilters(originalFilter, interactiveFilter);

    map.setFilter(layerId, finalFilter);
    layerState.filters[layerId] = finalFilter;
}

/**
 * Apply range filter to layer
 */
function applyRangeFilter(
    map,
    mapId,
    layerId,
    column,
    min,
    max,
    originalFilter
) {
    var layerState = window._mapglLayerState[mapId];

    // Add epsilon to handle floating-point precision and rounding issues
    // Use a larger epsilon (0.1% of range) to account for rounded legend breaks
    // that may not exactly match data values
    var range = Math.abs(max - min);
    var epsilon = range > 0 ? range * 0.001 : 0.001;
    var filterMin = min - epsilon;
    var filterMax = max + epsilon;

    var interactiveFilter = [
        "all",
        [">=", ["get", column], filterMin],
        ["<=", ["get", column], filterMax]
    ];

    // Combine with original filter if exists
    var finalFilter = combineFilters(originalFilter, interactiveFilter);

    map.setFilter(layerId, finalFilter);
    layerState.filters[layerId] = finalFilter;
}

/**
 * Combine existing filter with interactive filter
 */
function combineFilters(existingFilter, interactiveFilter) {
    if (!existingFilter) {
        return interactiveFilter;
    }
    // Wrap both in "all"
    return ["all", existingFilter, interactiveFilter];
}

/**
 * Add reset button to legend
 */
function addResetButton(legendElement, resetCallback) {
    if (legendElement.querySelector(".legend-reset-btn")) return;

    var resetBtn = document.createElement("button");
    resetBtn.className = "legend-reset-btn";
    resetBtn.textContent = "Reset Filter";
    resetBtn.addEventListener("click", function (e) {
        e.preventDefault();
        e.stopPropagation();
        resetCallback();
    });
    legendElement.appendChild(resetBtn);
}

/**
 * Update reset button visibility
 */
function updateResetButton(legendElement, show) {
    var resetBtn = legendElement.querySelector(".legend-reset-btn");
    if (resetBtn) {
        resetBtn.classList.toggle("visible", show);
    }
}

/**
 * Format numeric value for display
 */
function formatValue(value) {
    if (value === null || value === undefined || isNaN(value)) {
        return String(value);
    }
    var absValue = Math.abs(value);
    if (absValue >= 1000000) {
        return (value / 1000000).toFixed(1) + "M";
    } else if (absValue >= 1000) {
        return (value / 1000).toFixed(1) + "K";
    } else if (Number.isInteger(value)) {
        return value.toString();
    } else {
        return value.toFixed(2);
    }
}

/**
 * Initialize draggable functionality for legends
 * Called automatically when legends are rendered
 * @param {HTMLElement} container - The map container element
 */
function initializeDraggableLegends(container) {
    var legends = container.querySelectorAll('.mapboxgl-legend[data-draggable="true"]');
    legends.forEach(function(legend) {
        // Skip if already initialized
        if (legend._draggableInitialized) return;
        legend._draggableInitialized = true;

        makeLegendDraggable(legend);
    });
}

/**
 * Make a single legend element draggable
 * @param {HTMLElement} legend - The legend element
 */
function makeLegendDraggable(legend) {
    var isDragging = false;
    var startX, startY;
    var startLeft, startTop;

    // Add draggable class for styling
    legend.classList.add('legend-draggable');

    // Get the map container (parent of legend)
    var mapContainer = legend.closest('.mapboxgl-map, .maplibregl-map') || legend.parentElement;

    function onMouseDown(e) {
        // Don't start drag if clicking on interactive elements (including slider handles)
        if (e.target.closest('.legend-item, .legend-reset-btn, .continuous-slider-container, .legend-gradient-handle, .legend-gradient-middle, .legend-gradient-overlay-container, input, button')) {
            return;
        }

        isDragging = true;
        legend.classList.add('legend-dragging');

        startX = e.clientX;
        startY = e.clientY;

        // Get current position
        var rect = legend.getBoundingClientRect();
        var containerRect = mapContainer.getBoundingClientRect();

        // Calculate position relative to container
        startLeft = rect.left - containerRect.left;
        startTop = rect.top - containerRect.top;

        // Switch to absolute positioning with explicit coordinates
        legend.style.position = 'absolute';
        legend.style.left = startLeft + 'px';
        legend.style.top = startTop + 'px';
        legend.style.right = 'auto';
        legend.style.bottom = 'auto';

        e.preventDefault();
        e.stopPropagation();

        document.addEventListener('mousemove', onMouseMove);
        document.addEventListener('mouseup', onMouseUp);
    }

    function onMouseMove(e) {
        if (!isDragging) return;

        var deltaX = e.clientX - startX;
        var deltaY = e.clientY - startY;

        var newLeft = startLeft + deltaX;
        var newTop = startTop + deltaY;

        // Constrain to container bounds
        var containerRect = mapContainer.getBoundingClientRect();
        var legendRect = legend.getBoundingClientRect();

        var maxLeft = containerRect.width - legendRect.width;
        var maxTop = containerRect.height - legendRect.height;

        newLeft = Math.max(0, Math.min(newLeft, maxLeft));
        newTop = Math.max(0, Math.min(newTop, maxTop));

        legend.style.left = newLeft + 'px';
        legend.style.top = newTop + 'px';

        e.preventDefault();
    }

    function onMouseUp(e) {
        if (!isDragging) return;

        isDragging = false;
        legend.classList.remove('legend-dragging');

        document.removeEventListener('mousemove', onMouseMove);
        document.removeEventListener('mouseup', onMouseUp);
    }

    // Add touch support
    function onTouchStart(e) {
        if (e.touches.length !== 1) return;

        var touch = e.touches[0];
        // Don't start drag if touching interactive elements (including slider handles)
        if (e.target.closest('.legend-item, .legend-reset-btn, .continuous-slider-container, .legend-gradient-handle, .legend-gradient-middle, .legend-gradient-overlay-container, input, button')) {
            return;
        }

        isDragging = true;
        legend.classList.add('legend-dragging');

        startX = touch.clientX;
        startY = touch.clientY;

        var rect = legend.getBoundingClientRect();
        var containerRect = mapContainer.getBoundingClientRect();

        startLeft = rect.left - containerRect.left;
        startTop = rect.top - containerRect.top;

        legend.style.position = 'absolute';
        legend.style.left = startLeft + 'px';
        legend.style.top = startTop + 'px';
        legend.style.right = 'auto';
        legend.style.bottom = 'auto';

        e.preventDefault();
    }

    function onTouchMove(e) {
        if (!isDragging || e.touches.length !== 1) return;

        var touch = e.touches[0];
        var deltaX = touch.clientX - startX;
        var deltaY = touch.clientY - startY;

        var newLeft = startLeft + deltaX;
        var newTop = startTop + deltaY;

        var containerRect = mapContainer.getBoundingClientRect();
        var legendRect = legend.getBoundingClientRect();

        var maxLeft = containerRect.width - legendRect.width;
        var maxTop = containerRect.height - legendRect.height;

        newLeft = Math.max(0, Math.min(newLeft, maxLeft));
        newTop = Math.max(0, Math.min(newTop, maxTop));

        legend.style.left = newLeft + 'px';
        legend.style.top = newTop + 'px';

        e.preventDefault();
    }

    function onTouchEnd(e) {
        if (!isDragging) return;

        isDragging = false;
        legend.classList.remove('legend-dragging');
    }

    legend.addEventListener('mousedown', onMouseDown);
    legend.addEventListener('touchstart', onTouchStart, { passive: false });
    legend.addEventListener('touchmove', onTouchMove, { passive: false });
    legend.addEventListener('touchend', onTouchEnd);
}
