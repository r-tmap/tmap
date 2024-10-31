LeafletWidget.methods.addGlifyPolylinesSrc = function(color, weight, opacity, group, layerId) {

  var map = this;

  // color
  var clrs;
  if (color === null) {
    clrs = function(index, feature) { return col[layerId][0][index]; };
  } else {
    clrs = color;
  }

  // radius
  var wght;
  if (weight === null) {
    wght = function(index, feature) { return wgt[layerId][0][index]; };
  } else {
    wght = weight;
  }

  var lineslayer = L.glify.lines({
    map: map,
    click: function (e, feature) {
      if (typeof(popup) === "undefined") {
          return;
      } else if (typeof(popup[layerId]) === "undefined") {
        return;
      } else {
      if (map.hasLayer(lineslayer.glLayer)) {
          var idx = data[layerId][0].features.findIndex(k => k==feature);
          L.popup()
            .setLatLng(e.latlng)
            .setContent(popup[layerId][0][idx].toString())
            .openOn(map);
        }
      }
    },
    latitudeKey: 1,
    longitudeKey: 0,
    data: data[layerId][0],
    color: clrs,
    opacity: opacity,
    weight: wght,
    className: group
  });

  map.layerManager.addLayer(lineslayer.glLayer, null, null, group);

};
