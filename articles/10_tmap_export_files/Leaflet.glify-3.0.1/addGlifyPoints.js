LeafletWidget.methods.addGlifyPoints = function(data, cols, popup, opacity, radius, group, layerId) {

  const map = this;

  // colors
  var clrs;
  if (cols.length === 1) {
    clrs = cols[0];
  } else {
    clrs = function(index, point) { return cols[index]; };
  }

  // radius
  var rad;
  if (typeof(radius) === "number") {
    rad = radius;
  } else {
    rad = function(index, point) { return radius[index]; };
  }

/*
  var pop;
  if (popup) {
      if (popup === true) {
        pop = function (e, feature) {
          var popUp = '<pre>'+JSON.stringify(feature.properties,null,' ').replace(/[\{\}"]/g,'')+'</pre>';
          if (map.hasLayer(pointslayer.glLayer)) {
            L.popup({ maxWidth: 2000 })
              .setLatLng(e.latlng)
              .setContent(popUp)
              .openOn(map);
          }
        };
      } else {
        pop = function (e, feature) {
          if (map.hasLayer(pointslayer.glLayer)) {
            L.popup({ maxWidth: 2000 })
              .setLatLng(e.latlng)
              .setContent(feature.properties[[popup]].toString())
              .openOn(map);
          }
        };
      }
  } else {
      pop = null;
  }

  var pointslayer = L.glify.points({
    map: map,
    click: pop,
    data: data,
    color: clrs,
    opacity: opacity,
    size: size,
    className: group
  });

  map.layerManager.addLayer(pointslayer.glLayer, null, null, group);
*/

  var pointslayer = L.glify.points({
    map: map,
    click: (e, point, xy) => {
      var idx = data.findIndex(k => k==point);
      //set up a standalone popup (use a popup as a layer)
      if (map.hasLayer(pointslayer.glLayer)) {
        var content = popup ? popup[idx].toString() : null;
        if (HTMLWidgets.shinyMode) {
              Shiny.setInputValue(map.id + "_glify_click", {
                id: layerId ? layerId[idx] : idx+1,
                group: pointslayer.settings.className,
                lat: point[0],
                lng: point[1],
                data: content
              });
        }
        if (popup !== null) {
          L.popup()
            .setLatLng(point)
            .setContent(content)
            .openOn(map);
        }
      }
    },
    data: data,
    color: clrs,
    opacity: opacity,
    size: rad,
    className: group
  });

  map.layerManager.addLayer(pointslayer.glLayer, "glify", layerId, group);
};


LeafletWidget.methods.removeGlPoints = function(layerId) {
  this.layerManager.removeLayer("glify", layerId);
};

LeafletWidget.methods.clearGlLayers = function() {
  this.layerManager.clearLayers("glify");
};