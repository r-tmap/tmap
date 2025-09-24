// Rectangle drawing mode for Mapbox GL Draw
// Adapted from https://github.com/edgespatial/mapbox-gl-draw-rectangle-mode
(function (MapboxDraw) {
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

    const RectangleMode = {
        onSetup: function (opts) {
            const rectangle = this.newFeature({
                type: "Feature",
                properties: {},
                geometry: {
                    type: "Polygon",
                    coordinates: [[]]
                }
            });

            this.addFeature(rectangle);
            this.clearSelectedFeatures();
            doubleClickZoom.disable(this);
            this.updateUIClasses({ mouse: "add" });
            this.setActionableState({ trash: true });

            return {
                rectangle: rectangle
            };
        },

        onClick: function (state, e) {
            // If we have a start point and click on a different point, complete the rectangle
            if (state.startPoint &&
                (state.startPoint[0] !== e.lngLat.lng || state.startPoint[1] !== e.lngLat.lat)) {
                this.updateUIClasses({ mouse: "pointer" });
                state.endPoint = [e.lngLat.lng, e.lngLat.lat];
                this.changeMode("simple_select", { featuresId: state.rectangle.id });
                return;
            }

            // Set the start point
            const startPoint = [e.lngLat.lng, e.lngLat.lat];
            state.startPoint = startPoint;
        },

        onMouseMove: function (state, e) {
            // Update rectangle coordinates as the mouse moves
            if (state.startPoint) {
                const startX = state.startPoint[0];
                const startY = state.startPoint[1];
                const endX = e.lngLat.lng;
                const endY = e.lngLat.lat;

                state.rectangle.updateCoordinate("0.0", startX, startY);
                state.rectangle.updateCoordinate("0.1", endX, startY);
                state.rectangle.updateCoordinate("0.2", endX, endY);
                state.rectangle.updateCoordinate("0.3", startX, endY);
                state.rectangle.updateCoordinate("0.4", startX, startY);
            }
        },

        onKeyUp: function (state, e) {
            if (e.keyCode === 27) { // Escape key
                return this.changeMode("simple_select");
            }
        },

        onStop: function (state) {
            doubleClickZoom.enable(this);
            this.updateUIClasses({ mouse: "none" });
            this.activateUIButton();

            if (this.getFeature(state.rectangle.id) !== undefined) {
                // Remove the closing coordinate (duplicate of first)
                state.rectangle.removeCoordinate("0.4");

                if (state.rectangle.isValid()) {
                    this.map.fire("draw.create", {
                        features: [state.rectangle.toGeoJSON()]
                    });
                } else {
                    this.deleteFeature([state.rectangle.id], { silent: true });
                    this.changeMode("simple_select", {}, { silent: true });
                }
            }
        },

        toDisplayFeatures: function (state, geojson, display) {
            const isActiveRectangle = geojson.properties.id === state.rectangle.id;
            geojson.properties.active = isActiveRectangle ? "true" : "false";

            if (!isActiveRectangle) {
                return display(geojson);
            }

            // Only display the rectangle if we have started drawing
            if (state.startPoint) {
                return display(geojson);
            }
        },

        onTrash: function (state) {
            this.deleteFeature([state.rectangle.id], { silent: true });
            this.changeMode("simple_select");
        }
    };

    MapboxDraw.modes.draw_rectangle = RectangleMode;
})(MapboxDraw);