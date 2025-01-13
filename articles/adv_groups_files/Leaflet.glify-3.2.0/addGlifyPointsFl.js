LeafletWidget.methods.addGlifyPointsFl = function(data_var, color_var, popup_var, opacity, size, layerId, pane) {

  var map = this;
  var data_fl = document.getElementById(data_var + '-1-attachment' ).href;
  var color_fl = document.getElementById(color_var + '-1-attachment' ).href;
  if (popup_var) var popup_fl = document.getElementById(popup_var + '-1-attachment' ).href;

  wget([data_fl, color_fl, popup_fl], function(points, colors, popups) {
    var cols = JSON.parse(colors);
    var clrs;
    if (cols.length === 1) {
      clrs = cols[0];
    } else {
      clrs = function(index, point) { return cols[index]; };
    }
    var dat = JSON.parse(points);
    if (popup_var) var pop = JSON.parse(popups);
    L.glify.points({
      map: map,
      click: function (e, point, xy) {
        var idx = dat.indexOf(point);
        //set up a standalone popup (use a popup as a layer)
        L.popup()
            .setLatLng(point)
            .setContent(pop[idx].toString())
            .openOn(map);

        console.log(point);
      },
      data: dat,
      color: clrs,
      opacity: opacity,
      size: size,
      pane: pane,
      // className: "test"
    });
    //map.layerManager.addLayer(test, null, null, "test");
  });

  function wget(urls, fn) {
    var results = [],
      complete = 0,
      total = urls.length;

    urls.forEach(function(url, i) {
      var request = new XMLHttpRequest();
      request.open('GET', url, true);
      request.onload = function () {
        if (request.status < 200 && request.status > 400) return;
        results[i] = request.responseText;
        complete++;
        if (complete === total) fn.apply(null, results);
      };
      request.send();
    });
  }

  function clr(colors) {
    if (colors.length === 1) {
      return colors[0];
    }
    if (colors.length > 1) {
      return function(index, point) { return cols[index]; };
    }
  }

};
