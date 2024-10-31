LeafletWidget.methods.addGlifyPolygons = function(data, cols, popup, opacity, group, layerId) {

  var map = this;

  var clrs;
  if (cols.length === 1) {
    clrs = cols[0];
  } else {
    clrs = function(index, feature) { return cols[index]; };
  }

  var click_event = function(e, feature, addpopup, popup) {
    if (map.hasLayer(shapeslayer.glLayer)) {
      var idx = data.features.findIndex(k => k==feature);
      if (HTMLWidgets.shinyMode) {
        Shiny.setInputValue(map.id + "_glify_click", {
          id: layerId ? layerId[idx] : idx+1,
          group: Object.values(shapeslayer.glLayer._eventParents)[0].groupname,
          lat: e.latlng.lat,
          lng: e.latlng.lng,
          data: feature.properties
        });
      }
      if (addpopup) {
        var content = popup === true ? '<pre>'+JSON.stringify(feature.properties,null,' ').replace(/[\{\}"]/g,'')+'</pre>' : popup[idx].toString();
        L.popup({ maxWidth: 2000 })
        .setLatLng(e.latlng)
        .setContent(content)
        .openOn(map);
      }
    }
  };

  var pop = function (e, feature) {
    click_event(e, feature, popup !== null, popup);
  };

  var shapeslayer = L.glify.shapes({
    map: map,
    click: pop,
    data: data,
    color: clrs,
    opacity: opacity,
    className: group
  });

  map.layerManager.addLayer(shapeslayer.glLayer, "glify", layerId, group);
};


LeafletWidget.methods.removeGlPolygons = function(layerId) {
  this.layerManager.removeLayer("glify", layerId);
};