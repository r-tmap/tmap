LeafletWidget.methods.addGlifyPoints = function(data, cols, popup, label, opacity, radius, group, layerId, dotOptions, pane) {

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

  // click function
  let clickFun = (e, point, xy) => {
      var idx = data.findIndex(k => k==point);
      //set up a standalone popup (use a popup as a layer)
      if (map.hasLayer(pointslayer.layer)) {
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
    };

  let tooltip = new L.Tooltip();

  var hover_event = function(e, point, addlabel, label) {
    var idx = data.findIndex(k => k==point);
      //set up a standalone label (use a label as a layer)
      if (map.hasLayer(pointslayer.layer)) {
        var content = label ? label[idx].toString() : null;
        if (label !== null) {
          tooltip
            .setLatLng(point)
            .setContent(content)
            .addTo(map);
        }
      }
  }

  var hvr = function(e, feature) {
    hover_event(e, feature, label !== null, label);
  }

  // arguments for gl layer
  var pointsArgs = {
    map: map,
    click: clickFun,
    hover: hvr,
    data: data,
    color: clrs,
    opacity: opacity,
    size: rad,
    className: group,
    pane: pane
  };

  // extract correct fragmentShaderSource if provided via dotOptions
  if (dotOptions.fragmentShaderSource !== undefined && dotOptions.fragmentShaderSource !== null) {
    let fragmentShader = dotOptions.fragmentShaderSource;
    dotOptions.fragmentShaderSource = () => {
      return L.glify.shader.fragment[fragmentShader];
    };
  }

  // append dotOptions to layer arguments
  Object.entries(dotOptions).forEach(([key,value]) => { pointsArgs[key] = value });

  // initialze layer
  var pointslayer = L.glify.points(pointsArgs);

  // add layer to map using RStudio leaflet's layerManager
  map.layerManager.addLayer(pointslayer.layer, "glify", layerId, group);
};


LeafletWidget.methods.removeGlPoints = function(layerId) {
  this.layerManager.removeLayer("glify", layerId);
};

LeafletWidget.methods.clearGlLayers = function() {
  this.layerManager.clearLayers("glify");
};