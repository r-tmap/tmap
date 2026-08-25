// Custom Terra Draw modes for mapgl: pen-tool curve drawing.
//
// "curve" draws a polygon and "curve-linestring" a line whose edges mix
// straight segments and cubic bezier curves: a plain click places a corner
// node; click-and-drag places an anchor and pulls out symmetric curve
// handles; mouse move previews the pending segment; clicking the first node
// (last node for lines) or Enter finishes; Escape cancels; Backspace removes
// the last node. The data model and densifier are ports of the
// mapbox-gl-draw-bezier-curve-mode plugin (MIT, Jeff Sebrechts) that mapgl
// already vendors for the mapbox-gl-draw provider: a node is
// {coords, handle, handle2} in absolute lng/lat, a null handle means a
// corner, and curved segments densify at 19 steps.
//
// The finished feature stores its densified geometry (so measurements,
// download, and the sf round trip work unchanged) plus the control net as a
// JSON STRING property `curveNodes` — a string because MapLibre rejects
// object-valued properties on the GL render path.
//
// Registered by MapglTerraDrawControl via window.MapglTerraDrawModes; this
// file never mutates the vendored `terraDraw` namespace.
(function () {
  "use strict";

  if (
    typeof window.terraDraw === "undefined" ||
    !window.terraDraw.TerraDrawExtend
  ) {
    console.error(
      "mapgl terra-draw curve modes: terra-draw core is not loaded",
    );
    return;
  }

  var TerraDrawExtend = window.terraDraw.TerraDrawExtend;
  var BEZIER_STEPS = 19; // parity with the vendored mapbox bezier plugin
  var HANDLE_MIN_PX = 6; // drags shorter than this revert to a corner node

  // ---------------------------------------------------------------------
  // Pure helpers

  function clampLat(value) {
    return Math.max(-90, Math.min(90, value));
  }

  function mirror(origin, point) {
    return [
      origin[0] * 2 - point[0],
      clampLat(origin[1] * 2 - point[1]),
    ];
  }

  function cubic(p0, c1, c2, p1, t) {
    var mt = 1 - t;
    return [
      mt * mt * mt * p0[0] +
        3 * mt * mt * t * c1[0] +
        3 * mt * t * t * c2[0] +
        t * t * t * p1[0],
      mt * mt * mt * p0[1] +
        3 * mt * mt * t * c1[1] +
        3 * mt * t * t * c2[1] +
        t * t * t * p1[1],
    ];
  }

  function limitPrecision(coords, precision) {
    var factor = Math.pow(10, precision == null ? 9 : precision);
    if (typeof coords[0] === "number") {
      return coords.map(function (value) {
        return Math.round(value * factor) / factor;
      });
    }
    return coords.map(function (part) {
      return limitPrecision(part, precision);
    });
  }

  function halfway(a, b) {
    return [a[0] + (b[0] - a[0]) * 0.5, a[1] + (b[1] - a[1]) * 0.5];
  }

  // The incoming control point of node b (the end of a segment): the broken
  // handle if present, else the reflection of the outgoing handle
  function incomingOf(node) {
    if (node.handle2) return node.handle2;
    if (node.handle) return mirror(node.coords, node.handle);
    return null;
  }

  // Vertices from node a to (exclusive) node b — the plugin's
  // getVerticesBetweenNodes: straight edges contribute only their start
  // anchor; one-sided curves synthesize the missing control point at the
  // halfway point toward the other side's control point
  function segmentVertices(a, b) {
    var verts = [a.coords.slice()];
    if (!a.handle && !b.handle) {
      return verts;
    }
    var c1;
    var c2;
    if (a.handle) {
      c1 = a.handle;
    } else {
      c1 = halfway(a.coords, incomingOf(b));
    }
    if (b.handle) {
      c2 = incomingOf(b);
    } else {
      c2 = halfway(b.coords, a.handle);
    }
    for (var i = 1; i < BEZIER_STEPS; i++) {
      verts.push(cubic(a.coords, c1, c2, b.coords, i / BEZIER_STEPS));
    }
    return verts;
  }

  // Full vertex run. Open: every pairwise segment plus the final anchor.
  // Closed: additionally the last->first segment, terminated with a COPY of
  // the first emitted vertex (exact component-wise closure is required by
  // Terra Draw's polygon validation — never a recomputed t=1 sample).
  function densify(nodes, closed) {
    var verts = [];
    var i;
    for (i = 0; i < nodes.length - 1; i++) {
      verts = verts.concat(segmentVertices(nodes[i], nodes[i + 1]));
    }
    verts.push(nodes[nodes.length - 1].coords.slice());
    if (closed && nodes.length > 1) {
      var closing = segmentVertices(nodes[nodes.length - 1], nodes[0]);
      // drop the repeated last anchor already pushed above
      verts = verts.concat(closing.slice(1));
      verts.push(verts[0].slice());
    }
    return verts;
  }

  function translateCurveNodes(nodes, dLng, dLat) {
    nodes.forEach(function (node) {
      node.coords[0] += dLng;
      node.coords[1] += dLat;
      if (node.handle) {
        node.handle[0] += dLng;
        node.handle[1] += dLat;
      }
      if (node.handle2) {
        node.handle2[0] += dLng;
        node.handle2[1] += dLat;
      }
    });
    return nodes;
  }

  function isFinitePair(value) {
    return (
      Array.isArray(value) &&
      value.length === 2 &&
      isFinite(value[0]) &&
      isFinite(value[1])
    );
  }

  // Proper segment intersection (excluding shared endpoints) for the
  // self-intersection rejection on closed rings
  function orient(p, q, r) {
    var v = (q[0] - p[0]) * (r[1] - p[1]) - (q[1] - p[1]) * (r[0] - p[0]);
    if (v > 0) return 1;
    if (v < 0) return -1;
    return 0;
  }

  function segmentsCross(a1, a2, b1, b2) {
    var o1 = orient(a1, a2, b1);
    var o2 = orient(a1, a2, b2);
    var o3 = orient(b1, b2, a1);
    var o4 = orient(b1, b2, a2);
    return o1 !== o2 && o3 !== o4 && o1 !== 0 && o2 !== 0 && o3 !== 0 && o4 !== 0;
  }

  function ringSelfIntersects(ring) {
    // ring is closed (first === last); n segments = length - 1
    var n = ring.length - 1;
    for (var i = 0; i < n; i++) {
      for (var j = i + 2; j < n; j++) {
        if (i === 0 && j === n - 1) continue; // first and last share a vertex
        if (
          segmentsCross(ring[i], ring[i + 1], ring[j], ring[j + 1])
        ) {
          return true;
        }
      }
    }
    return false;
  }

  // ---------------------------------------------------------------------
  // Shared base

  class CurveBase extends TerraDrawExtend.TerraDrawBaseDrawMode {
    constructor(options) {
      super(options, true);
      this.updateOptions(options);
      this._closed = true; // subclasses override
      this._getContainer =
        options && typeof options.mapglGetContainer === "function"
          ? options.mapglGetContainer
          : null;
      this.cursors = { start: "crosshair", close: "pointer" };
      this.keyEvents = { cancel: "Escape", finish: "Enter" };
      this._pointerDownListener = null;
      this._downCoord = null;
      this._lastCursor = null;
      this._resetInternalState();
    }

    _resetInternalState() {
      this._nodes = [];
      this._currentId = undefined;
      this._nodePointIds = [];
      this._handlePointIds = [];
      this._handleLineId = undefined;
      this._drag = null;
    }

    _minNodes() {
      return this._closed ? 3 : 2;
    }

    // -- lifecycle ------------------------------------------------------

    start() {
      this.setStarted();
      this.setCursor(this.cursors.start);
      // Pointer-down bridge: the adapter reports onDragStart with the
      // threshold-crossing event (~8px late), so the true press position is
      // captured here and used as the anchor for drag-placed nodes
      var container = this._getContainer ? this._getContainer() : null;
      if (container && !this._pointerDownListener) {
        var self = this;
        this._pointerDownListener = function (e) {
          if (e.button !== 0) return;
          try {
            var rect = container.getBoundingClientRect();
            var coord = self.unproject(
              e.clientX - rect.left,
              e.clientY - rect.top,
            );
            self._downCoord = [coord.lng, coord.lat];
          } catch (err) {
            self._downCoord = null;
          }
        };
        container.addEventListener("pointerdown", this._pointerDownListener);
        this._pointerDownContainer = container;
      }
    }

    stop() {
      this.cleanUp();
      this.setStopped();
      this.setCursor("unset");
      if (this._pointerDownContainer && this._pointerDownListener) {
        this._pointerDownContainer.removeEventListener(
          "pointerdown",
          this._pointerDownListener,
        );
      }
      this._pointerDownListener = null;
      this._pointerDownContainer = null;
      this._downCoord = null;
    }

    cleanUp() {
      var ids = [this._currentId]
        .concat(this._nodePointIds, this._handlePointIds, [this._handleLineId])
        .filter(function (id) {
          return id !== undefined && id !== null;
        });
      if (ids.length) {
        try {
          this.store.delete(ids);
        } catch (e) {
          // during a control rebuild the old store is already torn down
        }
      }
      this._resetInternalState();
      if (this.state === "drawing") {
        this.setStarted();
      }
    }

    registerBehaviors() {}

    // -- geometry -------------------------------------------------------

    _buildGeometry(previewCoord) {
      var nodes = this._nodes;
      if (previewCoord) {
        nodes = nodes.concat([
          { coords: previewCoord, handle: null, handle2: null },
        ]);
      }
      var verts = densify(nodes, false);
      if (this._closed) {
        while (verts.length < 3) {
          verts.push(verts[verts.length - 1].slice());
        }
        var ring = limitPrecision(verts, this.coordinatePrecision);
        ring.push(ring[0].slice());
        return { type: "Polygon", coordinates: [ring] };
      }
      while (verts.length < 2) {
        verts.push(verts[verts.length - 1].slice());
      }
      return {
        type: "LineString",
        coordinates: limitPrecision(verts, this.coordinatePrecision),
      };
    }

    _finalGeometry() {
      if (this._closed) {
        var ring = limitPrecision(
          densify(this._nodes, true),
          this.coordinatePrecision,
        );
        // exact closure: terminate with a copy of the rounded first vertex
        ring[ring.length - 1] = ring[0].slice();
        return { type: "Polygon", coordinates: [ring] };
      }
      return {
        type: "LineString",
        coordinates: limitPrecision(
          densify(this._nodes, false),
          this.coordinatePrecision,
        ),
      };
    }

    _updateMain(previewCoord) {
      if (this._currentId === undefined) return;
      try {
        this.store.updateGeometry([
          { id: this._currentId, geometry: this._buildGeometry(previewCoord) },
        ]);
      } catch (e) {
        console.warn("mapgl curve mode: geometry update failed", e);
      }
    }

    // -- guidance -------------------------------------------------------

    _createNodePoint(coords) {
      var ids = this.store.create([
        {
          geometry: { type: "Point", coordinates: coords.slice() },
          properties: {
            mode: this.mode,
            curveGuidance: true,
            curveGuidanceType: "node",
          },
        },
      ]);
      this._nodePointIds.push(ids[0]);
    }

    _updateHandleGuidance(node) {
      var handle = node.handle;
      var inverse = mirror(node.coords, handle);
      var lineGeometry = {
        type: "LineString",
        coordinates: [inverse.slice(), node.coords.slice(), handle.slice()],
      };
      if (this._handleLineId === undefined) {
        var lineIds = this.store.create([
          {
            geometry: lineGeometry,
            properties: {
              mode: this.mode,
              curveGuidance: true,
              curveGuidanceType: "handle-line",
            },
          },
        ]);
        this._handleLineId = lineIds[0];
        var pointIds = this.store.create([
          {
            geometry: { type: "Point", coordinates: handle.slice() },
            properties: {
              mode: this.mode,
              curveGuidance: true,
              curveGuidanceType: "handle",
            },
          },
          {
            geometry: { type: "Point", coordinates: inverse.slice() },
            properties: {
              mode: this.mode,
              curveGuidance: true,
              curveGuidanceType: "handle",
            },
          },
        ]);
        this._handlePointIds = pointIds;
      } else {
        this.store.updateGeometry([
          { id: this._handleLineId, geometry: lineGeometry },
          {
            id: this._handlePointIds[0],
            geometry: { type: "Point", coordinates: handle.slice() },
          },
          {
            id: this._handlePointIds[1],
            geometry: { type: "Point", coordinates: inverse.slice() },
          },
        ]);
      }
    }

    _clearHandleGuidance() {
      var ids = this._handlePointIds.concat(
        this._handleLineId !== undefined ? [this._handleLineId] : [],
      );
      if (ids.length) {
        try {
          this.store.delete(ids);
        } catch (e) {}
      }
      this._handlePointIds = [];
      this._handleLineId = undefined;
    }

    // -- interaction ----------------------------------------------------

    _pxDistToCoord(event, coords) {
      var p = this.project(coords[0], coords[1]);
      var dx = p.x - event.containerX;
      var dy = p.y - event.containerY;
      return Math.sqrt(dx * dx + dy * dy);
    }

    _isOverCloseTarget(event) {
      if (this._nodes.length < this._minNodes()) return false;
      var target = this._closed
        ? this._nodes[0].coords
        : this._nodes[this._nodes.length - 1].coords;
      return this._pxDistToCoord(event, target) <= this.pointerDistance;
    }

    _beginCurve(coord) {
      this._nodes = [{ coords: coord.slice(), handle: null, handle2: null }];
      var ids = this.store.create([
        {
          geometry: this._buildGeometry(null),
          properties: { mode: this.mode, currentlyDrawing: true },
        },
      ]);
      this._currentId = ids[0];
      this._createNodePoint(coord);
      this.setDrawing();
    }

    _appendNode(coord) {
      this._nodes.push({ coords: coord.slice(), handle: null, handle2: null });
      this._createNodePoint(coord);
      this._clearHandleGuidance();
    }

    onClick(event) {
      if (event.button && event.button !== "left") return;
      this._downCoord = null;
      var coord = [event.lng, event.lat];
      if (this._currentId === undefined) {
        this._beginCurve(coord);
        return;
      }
      if (this._isOverCloseTarget(event)) {
        this._finish();
        return;
      }
      // ignore clicks on/near the last node (double-click guard) and on the
      // close target before the node minimum is met
      var last = this._nodes[this._nodes.length - 1];
      if (this._pxDistToCoord(event, last.coords) <= this.pointerDistance) {
        return;
      }
      if (
        this._closed &&
        this._nodes.length < this._minNodes() &&
        this._pxDistToCoord(event, this._nodes[0].coords) <=
          this.pointerDistance
      ) {
        return;
      }
      this._appendNode(coord);
      this._updateMain(coord);
    }

    onMouseMove(event) {
      if (this._currentId === undefined) return;
      this._lastCursor = [event.lng, event.lat];
      this._updateMain(this._lastCursor);
      this.setCursor(
        this._isOverCloseTarget(event)
          ? this.cursors.close
          : this.cursors.start,
      );
    }

    onDragStart(event, setMapDraggability) {
      if (typeof setMapDraggability === "function") {
        setMapDraggability(false);
      }
      if (this._currentId !== undefined && this._isOverCloseTarget(event)) {
        this._drag = null;
        return;
      }
      // the true press position, not the ~8px-late threshold-crossing event
      var coord = this._downCoord || [event.lng, event.lat];
      this._downCoord = null;
      if (this._currentId === undefined) {
        this._beginCurve(coord);
      } else {
        this._appendNode(coord);
      }
      this._drag = {
        index: this._nodes.length - 1,
        startX: event.containerX,
        startY: event.containerY,
        moved: false,
      };
    }

    onDrag(event) {
      if (!this._drag || this._currentId === undefined) return;
      var dx = event.containerX - this._drag.startX;
      var dy = event.containerY - this._drag.startY;
      if (!this._drag.moved && Math.sqrt(dx * dx + dy * dy) >= HANDLE_MIN_PX) {
        this._drag.moved = true;
      }
      if (!this._drag.moved) return;
      var node = this._nodes[this._drag.index];
      node.handle = [event.lng, event.lat];
      this._updateHandleGuidance(node);
      this._updateMain(this._lastCursor || node.coords);
    }

    onDragEnd(event, setMapDraggability) {
      if (typeof setMapDraggability === "function") {
        setMapDraggability(true);
      }
      if (!this._drag || this._currentId === undefined) return;
      if (!this._drag.moved) {
        var node = this._nodes[this._drag.index];
        node.handle = null;
        this._clearHandleGuidance();
      }
      this._updateMain(null);
      this._drag = null;
    }

    onKeyUp(event) {
      if (event.key === this.keyEvents.finish) {
        if (
          this._currentId !== undefined &&
          this._nodes.length >= this._minNodes()
        ) {
          this._finish();
        }
        return;
      }
      if (event.key === this.keyEvents.cancel) {
        this.cleanUp();
        this.setCursor(this.cursors.start);
        return;
      }
      if (event.key === "Backspace" && this._currentId !== undefined) {
        if (this._nodes.length <= 1) {
          this.cleanUp();
          this.setCursor(this.cursors.start);
          return;
        }
        this._nodes.pop();
        var pointId = this._nodePointIds.pop();
        if (pointId !== undefined) {
          try {
            this.store.delete([pointId]);
          } catch (e) {}
        }
        this._clearHandleGuidance();
        this._updateMain(this._lastCursor);
      }
    }

    _finish() {
      if (
        this._currentId === undefined ||
        this._nodes.length < this._minNodes()
      ) {
        return;
      }
      var geometry = this._finalGeometry();
      if (this._closed && ringSelfIntersects(geometry.coordinates[0])) {
        console.warn(
          "mapgl curve mode: the curve outline crosses itself — adjust the shape before finishing",
        );
        return;
      }
      var guidance = this._nodePointIds
        .concat(this._handlePointIds)
        .concat(this._handleLineId !== undefined ? [this._handleLineId] : []);
      if (guidance.length) {
        try {
          this.store.delete(guidance);
        } catch (e) {}
      }
      var id = this._currentId;
      this.store.updateGeometry([{ id: id, geometry: geometry }]);
      this.store.updateProperty([
        { id: id, property: "curveNodes", value: JSON.stringify(this._nodes) },
        { id: id, property: "currentlyDrawing", value: undefined },
      ]);
      this._resetInternalState();
      if (this.state === "drawing") {
        this.setStarted();
      }
      this.setCursor(this.cursors.start);
      this.onFinish(id, { mode: this.mode, action: "draw" });
    }

    // -- styling & validation ------------------------------------------

    // Consumes mode option names (fillColor, outlineColor, lineStringColor,
    // nodeColor, handleColor, ... — what the control's _modeConfig emits);
    // returns adapter output names (polygonFillColor, pointColor, ...) on
    // the getDefaultStyling() object
    styleFeature(feature) {
      var styles = TerraDrawExtend.getDefaultStyling();
      var props = feature.properties || {};
      if (props.mode !== this.mode) return styles;

      if (props.curveGuidance) {
        if (props.curveGuidanceType === "handle-line") {
          styles.lineStringColor = this.getHexColorStylingValue(
            this.styles.handleLineColor,
            "#666666",
            feature,
          );
          styles.lineStringWidth = this.getNumericStylingValue(
            this.styles.handleLineWidth,
            1.5,
            feature,
          );
          styles.lineStringDash = [2, 2];
          styles.zIndex = 30;
        } else if (props.curveGuidanceType === "handle") {
          styles.pointColor = this.getHexColorStylingValue(
            this.styles.handleColor,
            "#666666",
            feature,
          );
          styles.pointWidth = this.getNumericStylingValue(
            this.styles.handlePointWidth,
            4,
            feature,
          );
          styles.pointOutlineColor = "#FFFFFF";
          styles.pointOutlineWidth = 1;
          styles.zIndex = 40;
        } else {
          styles.pointColor = this.getHexColorStylingValue(
            this.styles.nodeColor,
            "#FBB03B",
            feature,
          );
          styles.pointWidth = this.getNumericStylingValue(
            this.styles.nodePointWidth,
            5,
            feature,
          );
          styles.pointOutlineColor = "#FFFFFF";
          styles.pointOutlineWidth = 2;
          styles.zIndex = 40;
        }
        return styles;
      }

      if (feature.geometry.type === "Polygon") {
        styles.polygonFillColor = this.getHexColorStylingValue(
          this.styles.fillColor,
          styles.polygonFillColor,
          feature,
        );
        styles.polygonFillOpacity = this.getNumericStylingValue(
          this.styles.fillOpacity,
          styles.polygonFillOpacity,
          feature,
        );
        styles.polygonOutlineColor = this.getHexColorStylingValue(
          this.styles.outlineColor,
          styles.polygonOutlineColor,
          feature,
        );
        styles.polygonOutlineWidth = this.getNumericStylingValue(
          this.styles.outlineWidth,
          styles.polygonOutlineWidth,
          feature,
        );
        styles.zIndex = 10;
      } else if (feature.geometry.type === "LineString") {
        styles.lineStringColor = this.getHexColorStylingValue(
          this.styles.lineStringColor,
          styles.lineStringColor,
          feature,
        );
        styles.lineStringWidth = this.getNumericStylingValue(
          this.styles.lineStringWidth,
          styles.lineStringWidth,
          feature,
        );
        styles.zIndex = 10;
      }
      return styles;
    }

    // Validates both the densified geometry and the curveNodes contract so
    // malformed metadata demotes cleanly (via the control's base-mode retry)
    // instead of shipping a curve whose move-resync silently no-ops.
    // Guidance and in-progress features skip the metadata contract.
    validateFeature(feature) {
      var self = this;
      return this.validateModeFeature(feature, function (f) {
        return self._curveValidate(f);
      });
    }

    _curveValidate(feature) {
      var props = feature.properties || {};
      if (props.curveGuidance || props.currentlyDrawing) {
        return { valid: true };
      }
      var geometry = feature.geometry || {};
      var precision = this.coordinatePrecision == null
        ? 9
        : this.coordinatePrecision;
      var coords;
      if (this._closed) {
        if (geometry.type !== "Polygon") {
          return { valid: false, reason: "Feature is not a Polygon" };
        }
        if ((geometry.coordinates || []).length !== 1) {
          return { valid: false, reason: "Feature has holes" };
        }
        coords = geometry.coordinates[0];
        if (coords.length < 4) {
          return {
            valid: false,
            reason: "Feature has less than 4 coordinates",
          };
        }
        var first = coords[0];
        var last = coords[coords.length - 1];
        if (first[0] !== last[0] || first[1] !== last[1]) {
          return { valid: false, reason: "Feature coordinates are not closed" };
        }
      } else {
        if (geometry.type !== "LineString") {
          return { valid: false, reason: "Feature is not a LineString" };
        }
        coords = geometry.coordinates || [];
        if (coords.length < 2) {
          return {
            valid: false,
            reason: "Feature has less than 2 coordinates",
          };
        }
      }
      for (var i = 0; i < coords.length; i++) {
        var c = coords[i];
        if (
          !isFinitePair(c) ||
          c[0] < -180 ||
          c[0] > 180 ||
          c[1] < -90 ||
          c[1] > 90
        ) {
          return { valid: false, reason: "Feature has invalid coordinates" };
        }
        if (
          limitPrecision(c, precision)[0] !== c[0] ||
          limitPrecision(c, precision)[1] !== c[1]
        ) {
          return {
            valid: false,
            reason: "Feature has coordinates with excessive precision",
          };
        }
      }
      if (this._closed && ringSelfIntersects(coords)) {
        return { valid: false, reason: "Feature intersects itself" };
      }

      // control-net contract
      if (typeof props.curveNodes !== "string") {
        return { valid: false, reason: "Feature has no curve metadata" };
      }
      var nodes;
      try {
        nodes = JSON.parse(props.curveNodes);
      } catch (e) {
        return { valid: false, reason: "Feature has invalid curve metadata" };
      }
      if (!Array.isArray(nodes) || nodes.length < this._minNodes()) {
        return { valid: false, reason: "Feature has invalid curve metadata" };
      }
      for (var j = 0; j < nodes.length; j++) {
        var node = nodes[j];
        if (!node || !isFinitePair(node.coords)) {
          return { valid: false, reason: "Feature has invalid curve metadata" };
        }
        if (node.handle != null && !isFinitePair(node.handle)) {
          return { valid: false, reason: "Feature has invalid curve metadata" };
        }
        if (node.handle2 != null && !isFinitePair(node.handle2)) {
          return { valid: false, reason: "Feature has invalid curve metadata" };
        }
      }
      // the ring/line must start at node 0's anchor or move-resync breaks
      var anchor = limitPrecision(nodes[0].coords, precision);
      if (
        Math.abs(anchor[0] - coords[0][0]) > 1e-9 ||
        Math.abs(anchor[1] - coords[0][1]) > 1e-9
      ) {
        return { valid: false, reason: "Feature has invalid curve metadata" };
      }
      return { valid: true };
    }
  }

  class TerraDrawCurveMode extends CurveBase {
    constructor(options) {
      super(options);
      this.mode = "curve";
      this._closed = true;
    }
  }

  class TerraDrawCurveLineStringMode extends CurveBase {
    constructor(options) {
      super(options);
      this.mode = "curve-linestring";
      this._closed = false;
    }
  }

  window.MapglTerraDrawModes = {
    TerraDrawCurveMode: TerraDrawCurveMode,
    TerraDrawCurveLineStringMode: TerraDrawCurveLineStringMode,
    translateCurveNodes: translateCurveNodes,
    // exposed for unit tests
    _densify: densify,
    _segmentVertices: segmentVertices,
    _ringSelfIntersects: ringSelfIntersects,
  };
})();
