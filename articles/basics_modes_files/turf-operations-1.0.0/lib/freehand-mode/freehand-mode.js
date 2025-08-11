// This code is derived from the ISC-licensed "mapbox-gl-draw-freehand-mode" plugin
// by Ben Ehmke, Eric Dong, and Joe Woodhouse.
// Source: https://github.com/bemky/mapbox-gl-draw-freehand-mode
(function (MapboxDraw) {
    const { geojsonTypes, cursors, types, updateActions, modes, events } =
        MapboxDraw.constants;

    const FreehandMode = {};

    FreehandMode.onSetup = function () {
        const polygon = this.newFeature({
            type: geojsonTypes.FEATURE,
            properties: {},
            geometry: {
                type: geojsonTypes.POLYGON,
                coordinates: [[]],
            },
        });

        this.addFeature(polygon);
        this.clearSelectedFeatures();

        // Disable map dragging
        setTimeout(() => {
            if (!this.map || !this.map.dragPan) return;
            this.map.dragPan.disable();
        }, 0);

        this.updateUIClasses({ mouse: cursors.ADD });
        this.activateUIButton(types.POLYGON);
        this.setActionableState({
            trash: true,
        });

        return {
            polygon,
            currentVertexPosition: 0,
            dragMoving: false,
            isDrawing: false,
        };
    };

    FreehandMode.onDrag = FreehandMode.onTouchMove = function (state, e) {
        state.dragMoving = true;
        state.isDrawing = true;
        this.updateUIClasses({ mouse: cursors.ADD });
        state.polygon.updateCoordinate(
            `0.${state.currentVertexPosition}`,
            e.lngLat.lng,
            e.lngLat.lat,
        );
        state.currentVertexPosition++;
        state.polygon.updateCoordinate(
            `0.${state.currentVertexPosition}`,
            e.lngLat.lng,
            e.lngLat.lat,
        );
    };

    FreehandMode.onMouseUp = function (state) {
        if (state.dragMoving) {
            this.simplify(state.polygon);
            this.updateUIClasses({ mouse: cursors.MOVE });
            this.fireUpdate();
            this.changeMode(modes.SIMPLE_SELECT, {
                featureIds: [state.polygon.id],
            });
        }
    };

    FreehandMode.fireCreate = function (polygon) {
        this.map.fire(events.CREATE, {
            features: [polygon.toGeoJSON()],
        });
    };

    FreehandMode.fireUpdate = function () {
        this.map.fire(events.UPDATE, {
            action: updateActions.MOVE,
            features: this.getSelected().map((f) => f.toGeoJSON()),
        });
    };

    FreehandMode.simplify = function (polygon) {
        if (!this.map.simplify_freehand) return;

        const tolerance = 1 / Math.pow(1.05, 10 * this.map.getZoom());
        const simplifiedCoords = simplifyGeometry(
            polygon.coordinates[0],
            tolerance,
        );
        polygon.setCoordinates([simplifiedCoords]);
    };

    function simplifyGeometry(points, tolerance) {
        if (points.length <= 2) return points;

        const sqTolerance = tolerance * tolerance;
        const last = points.length - 1;
        const simplified = [points[0]];
        simplifyDPStep(points, 0, last, sqTolerance, simplified);
        simplified.push(points[last]);

        return simplified;
    }

    function simplifyDPStep(points, first, last, sqTolerance, simplified) {
        let maxSqDist = sqTolerance;
        let index;

        for (let i = first + 1; i < last; i++) {
            const sqDist = getSquareSegmentDistance(
                points[i],
                points[first],
                points[last],
            );
            if (sqDist > maxSqDist) {
                index = i;
                maxSqDist = sqDist;
            }
        }

        if (maxSqDist > sqTolerance) {
            if (index - first > 1)
                simplifyDPStep(points, first, index, sqTolerance, simplified);
            simplified.push(points[index]);
            if (last - index > 1)
                simplifyDPStep(points, index, last, sqTolerance, simplified);
        }
    }

    function getSquareSegmentDistance(p, p1, p2) {
        let x = p1[0],
            y = p1[1];
        let dx = p2[0] - x,
            dy = p2[1] - y;

        if (dx !== 0 || dy !== 0) {
            const t = ((p[0] - x) * dx + (p[1] - y) * dy) / (dx * dx + dy * dy);
            if (t > 1) {
                x = p2[0];
                y = p2[1];
            } else if (t > 0) {
                x += dx * t;
                y += dy * t;
            }
        }

        dx = p[0] - x;
        dy = p[1] - y;

        return dx * dx + dy * dy;
    }

    FreehandMode.onStop = function (state) {
        this.updateUIClasses({ mouse: cursors.NONE });
        this.activateUIButton();

        // Enable map dragging
        setTimeout(() => {
            if (!this.map || !this.map.dragPan) return;
            this.map.dragPan.enable();
        }, 0);
    };

    FreehandMode.toDisplayFeatures = function (state, geojson, display) {
        const isActivePolygon = geojson.properties.id === state.polygon.id;
        geojson.properties.active = isActivePolygon ? "true" : "false";
        if (!isActivePolygon) return display(geojson);

        // Only render the polygon if it has at least three points
        if (geojson.geometry.coordinates[0].length < 3) return;

        const coordinateCount = geojson.geometry.coordinates[0].length;

        // If we have fewer than three coordinates, we need to create a LineString instead of a Polygon
        if (coordinateCount < 3) {
            const lineCoordinates = [
                [
                    geojson.geometry.coordinates[0][0][0],
                    geojson.geometry.coordinates[0][0][1],
                ],
                [
                    geojson.geometry.coordinates[0][1][0],
                    geojson.geometry.coordinates[0][1][1],
                ],
            ];
            return display({
                type: geojsonTypes.FEATURE,
                properties: geojson.properties,
                geometry: {
                    coordinates: lineCoordinates,
                    type: geojsonTypes.LINE_STRING,
                },
            });
        }

        return display(geojson);
    };

    FreehandMode.onTrash = function (state) {
        this.deleteFeature([state.polygon.id], { silent: true });
        this.changeMode(modes.SIMPLE_SELECT);
    };

    MapboxDraw.modes.draw_freehand = FreehandMode;
})(MapboxDraw);
