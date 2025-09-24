// Radius/Circle drawing mode for Mapbox GL Draw
// Creates a circle by drawing from center to edge
(function (MapboxDraw) {
    const DrawLine = MapboxDraw.modes.draw_line_string;

    // Utility function to create a vertex feature
    const createVertex = function (parentId, coordinates, path, selected) {
        return {
            type: "Feature",
            properties: {
                meta: "vertex",
                parent: parentId,
                coord_path: path,
                active: selected ? "true" : "false"
            },
            geometry: {
                type: "Point",
                coordinates: coordinates
            }
        };
    };

    // Utility function to calculate distance between two points in kilometers
    const calculateDistance = function (coord1, coord2) {
        const lat1 = coord1[1];
        const lon1 = coord1[0];
        const lat2 = coord2[1];
        const lon2 = coord2[0];

        const R = 6371; // Radius of the Earth in kilometers
        const dLat = (lat2 - lat1) * Math.PI / 180;
        const dLon = (lon2 - lon1) * Math.PI / 180;
        const a =
            Math.sin(dLat/2) * Math.sin(dLat/2) +
            Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
            Math.sin(dLon/2) * Math.sin(dLon/2);
        const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
        const distance = R * c;

        return distance;
    };

    // Utility function to create a GeoJSON circle
    const createGeoJSONCircle = function (center, radiusInKm, parentId, points) {
        points = points || 64;

        const coords = {
            latitude: center[1],
            longitude: center[0]
        };

        const km = radiusInKm;
        const ret = [];
        const distanceX = km / (111.32 * Math.cos((coords.latitude * Math.PI) / 180));
        const distanceY = km / 110.574;

        let theta, x, y;
        for (let i = 0; i < points; i++) {
            theta = (i / points) * (2 * Math.PI);
            x = distanceX * Math.cos(theta);
            y = distanceY * Math.sin(theta);

            ret.push([coords.longitude + x, coords.latitude + y]);
        }
        ret.push(ret[0]);

        return {
            type: "Feature",
            geometry: {
                type: "Polygon",
                coordinates: [ret]
            },
            properties: {
                parent: parentId,
                meta: "radius"
            }
        };
    };

    // Utility function to format distance for display
    const getDisplayMeasurements = function (distanceKm) {
        let metricUnits = "m";
        let metricFormat = "0,0";
        let metricMeasurement;

        let standardUnits = "feet";
        let standardFormat = "0,0";
        let standardMeasurement;

        metricMeasurement = distanceKm * 1000; // Convert to meters
        if (metricMeasurement >= 1000) {
            metricMeasurement = metricMeasurement / 1000;
            metricUnits = "km";
            metricFormat = "0.00";
        }

        standardMeasurement = distanceKm * 1000 * 3.28084; // Convert to feet
        if (standardMeasurement >= 5280) {
            standardMeasurement = standardMeasurement / 5280;
            standardUnits = "mi";
            standardFormat = "0.00";
        }

        // Simple number formatting (without numeral.js dependency)
        const formatNumber = function(num, format) {
            if (format === "0,0") {
                return Math.round(num).toLocaleString();
            } else if (format === "0.00") {
                return num.toFixed(2);
            }
            return num.toString();
        };

        return {
            metric: formatNumber(metricMeasurement, metricFormat) + " " + metricUnits,
            standard: formatNumber(standardMeasurement, standardFormat) + " " + standardUnits
        };
    };

    const doubleClickZoom = {
        enable: function (ctx) {
            setTimeout(function () {
                if (!ctx.map || !ctx.map.doubleClickZoom) return;
                if (ctx._ctx && ctx._ctx.store && ctx._ctx.store.getInitialConfigValue) {
                    if (ctx._ctx.store.getInitialConfigValue("doubleClickZoom")) {
                        ctx.map.doubleClickZoom.enable();
                    }
                }
            }, 0);
        },
        disable: function (ctx) {
            setTimeout(function () {
                if (ctx.map && ctx.map.doubleClickZoom) {
                    ctx.map.doubleClickZoom.disable();
                }
            }, 0);
        }
    };

    const RadiusMode = Object.assign({}, DrawLine);

    RadiusMode.onSetup = function (opts) {
        const line = this.newFeature({
            type: "Feature",
            properties: {},
            geometry: {
                type: "LineString",
                coordinates: []
            }
        });

        this.addFeature(line);
        this.clearSelectedFeatures();
        doubleClickZoom.disable(this);
        this.updateUIClasses({ mouse: "add" });
        this.activateUIButton("line");
        this.setActionableState({ trash: true });

        return {
            line: line,
            currentVertexPosition: 0,
            direction: "forward"
        };
    };

    RadiusMode.onClick = function (state, e) {
        // This ends the drawing after the user creates a second point
        if (state.currentVertexPosition === 1) {
            // Update the second coordinate in place, don't add at position 0
            state.line.updateCoordinate(1, e.lngLat.lng, e.lngLat.lat);
            return this.changeMode("simple_select", { featureIds: [state.line.id] });
        }

        this.updateUIClasses({ mouse: "add" });
        state.line.updateCoordinate(
            state.currentVertexPosition,
            e.lngLat.lng,
            e.lngLat.lat
        );

        if (state.direction === "forward") {
            state.currentVertexPosition += 1;
            state.line.updateCoordinate(
                state.currentVertexPosition,
                e.lngLat.lng,
                e.lngLat.lat
            );
        } else {
            state.line.addCoordinate(0, e.lngLat.lng, e.lngLat.lat);
        }

        return null;
    };

    RadiusMode.onMouseMove = function (state, e) {
        if (state.currentVertexPosition === 1) {
            state.line.updateCoordinate(1, e.lngLat.lng, e.lngLat.lat);
        }
    };

    // Creates the final geojson circle polygon
    RadiusMode.onStop = function (state) {
        doubleClickZoom.enable(this);
        this.activateUIButton();

        // Check to see if we've deleted this feature
        if (this.getFeature(state.line.id) === undefined) return;

        if (state.line.isValid()) {
            const lineGeoJson = state.line.toGeoJSON();
            const coords = lineGeoJson.geometry.coordinates;

            if (coords.length >= 2) {
                // Calculate radius in kilometers
                const radiusKm = calculateDistance(coords[0], coords[1]);

                // Create the circle polygon
                const circleFeature = createGeoJSONCircle(coords[0], radiusKm, state.line.id);

                // Add radius property for reference
                circleFeature.properties.radius = (radiusKm * 1000).toFixed(1);

                // Remove the meta property that was interfering
                delete circleFeature.properties.meta;
                delete circleFeature.properties.parent;

                // Delete the temporary line first
                this.deleteFeature([state.line.id], { silent: true });

                // Add the circle feature to the draw instance
                const circleDrawFeature = this.newFeature(circleFeature);
                this.addFeature(circleDrawFeature);

                this.map.fire("draw.create", {
                    features: [circleDrawFeature.toGeoJSON()]
                });
            } else {
                this.deleteFeature([state.line.id], { silent: true });
            }
        } else {
            this.deleteFeature([state.line.id], { silent: true });
        }

        this.changeMode("simple_select", {}, { silent: true });
    };

    RadiusMode.toDisplayFeatures = function (state, geojson, display) {
        const isActiveLine = geojson.properties.id === state.line.id;
        geojson.properties.active = isActiveLine ? "true" : "false";

        if (!isActiveLine) return display(geojson);

        // Only render the line if it has at least one real coordinate
        if (geojson.geometry.coordinates.length < 2) return null;

        geojson.properties.meta = "feature";

        // Display center vertex as a point feature
        display(createVertex(
            state.line.id,
            geojson.geometry.coordinates[
                state.direction === "forward"
                    ? geojson.geometry.coordinates.length - 2
                    : 1
            ],
            "" + (state.direction === "forward"
                ? geojson.geometry.coordinates.length - 2
                : 1),
            false
        ));

        // Display the line as it is drawn
        display(geojson);

        const coords = geojson.geometry.coordinates;
        if (coords.length >= 2) {
            const distanceKm = calculateDistance(coords[0], coords[1]);
            const displayMeasurements = getDisplayMeasurements(distanceKm);

            // Create custom feature for the current pointer position
            const currentVertex = {
                type: "Feature",
                properties: {
                    meta: "currentPosition",
                    radiusMetric: displayMeasurements.metric,
                    radiusStandard: displayMeasurements.standard,
                    parent: state.line.id
                },
                geometry: {
                    type: "Point",
                    coordinates: coords[1]
                }
            };
            display(currentVertex);

            // Create custom feature for radius circle
            const center = coords[0];
            const circleFeature = createGeoJSONCircle(center, distanceKm, state.line.id);
            display(circleFeature);
        }

        return null;
    };

    RadiusMode.onTrash = function (state) {
        this.deleteFeature([state.line.id], { silent: true });
        this.changeMode("simple_select");
    };

    MapboxDraw.modes.draw_radius = RadiusMode;
})(MapboxDraw);