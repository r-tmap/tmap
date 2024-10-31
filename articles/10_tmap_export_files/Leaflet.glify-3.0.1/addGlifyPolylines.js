LeafletWidget.methods.addGlifyPolylines = function(data, cols, popup, opacity, group, weight, layerId) {

  var map = this;

  var clrs;
  if (cols.length === 1) {
    clrs = cols[0];
  } else {
    clrs = function(index, feature) { return cols[index]; };
  }

  var wght;
  if (weight.length === undefined) {
    wght = weight;
  } else {
    wght = function(index, feature) { return weight[index]; };
  }

  var click_event = function(e, feature, addpopup, popup) {
    if (map.hasLayer(lineslayer.glLayer)) {
      var idx = data.features.findIndex(k => k==feature);
      if (HTMLWidgets.shinyMode) {
        Shiny.setInputValue(map.id + "_glify_click", {
          id: layerId ? layerId[idx] : idx+1,
          group: Object.values(lineslayer.glLayer._eventParents)[0].groupname,
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

  var lineslayer = L.glify.lines({
    map: map,
    click: pop,
    latitudeKey: 1,
    longitudeKey: 0,
    data: data,
    color: clrs,
    opacity: opacity,
    className: group,
    weight: wght
  });

  map.layerManager.addLayer(lineslayer.glLayer, "glify", layerId, group);
};


LeafletWidget.methods.removeGlPolylines = function(layerId) {
  this.layerManager.removeLayer("glify", layerId);
};

