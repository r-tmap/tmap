LeafletWidget.methods.addGlifyPolygonsSrc = function(fillColor, fillOpacity, group, layerId, pane) {

  var map = this;

// FIX ME clrs, pop need to be layer specificly named!!!!!

  // color
  var clrs;
  if (fillColor === null) {
    clrs = function(index, feature) { return col[layerId][0][index]; };
  } else {
    clrs = fillColor;
  }

  var shapeslayer = L.glify.shapes({
    map: map,
    click: function (e, feature) {
      if (typeof(popup) === "undefined") {
          return;
      } else if (typeof(popup[layerId]) === "undefined") {
        return;
      } else {
      if (map.hasLayer(shapeslayer.layer)) {
          var idx = data[layerId][0].features.findIndex(k => k==feature);
          L.popup()
            .setLatLng(e.latlng)
            .setContent(popup[layerId][0][idx].toString())
            .openOn(map);
        }
      }
    },
    data: data[layerId][0],
    color: clrs,
    opacity: fillOpacity,
    className: group,
    pane: pane
  });

  map.layerManager.addLayer(shapeslayer.layer, "glify", null, group);

};
