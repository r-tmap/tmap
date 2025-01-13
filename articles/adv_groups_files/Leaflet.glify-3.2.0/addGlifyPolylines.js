LeafletWidget.methods.addGlifyPolylines = function(data, cols, popup, label, opacity, group, weight, layerId, pane) {

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
    if (map.hasLayer(lineslayer.layer)) {
      var idx = data.features.findIndex(k => k==feature);
      if (HTMLWidgets.shinyMode) {
        Shiny.setInputValue(map.id + "_glify_click", {
          id: layerId ? layerId[idx] : idx+1,
          group: Object.values(lineslayer.layer._eventParents)[0].groupname,
          lat: e.latlng.lat,
          lng: e.latlng.lng,
          data: feature.properties
        });
      }
      if (addpopup) {
        var content = popup === true ? json2table(feature.properties) : popup[idx].toString();

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

  // var label = "testtest";
  let tooltip = new L.Tooltip();

  var hover_event = function(e, feature, addlabel, label) {
    if (map.hasLayer(lineslayer.layer)) {
      if (addlabel) {
        tooltip
         .setLatLng(e.latlng)
         .setContent(feature.properties[[label]].toString())
         .addTo(map);
      }
    }
  }

  var hvr = function(e, feature) {
    hover_event(e, feature, label !== null, label);
  }


  var lineslayer = L.glify.lines({
    map: map,
    click: pop,
    hover: hvr,
    latitudeKey: 1,
    longitudeKey: 0,
    data: data,
    color: clrs,
    opacity: opacity,
    className: group,
    weight: wght,
    pane: pane
  });

  map.layerManager.addLayer(lineslayer.layer, "glify", layerId, group);
};


LeafletWidget.methods.removeGlPolylines = function(layerId) {
  this.layerManager.removeLayer("glify", layerId);
};

