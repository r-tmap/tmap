LeafletWidget.methods.addGlifyPolygonsFl = function(data_var, color_var, popup_var, opacity, pane) {

  var map = this;
  var data_fl = document.getElementById(data_var + '-1-attachment' ).href;
  var color_fl = document.getElementById(color_var + '-1-attachment' ).href;
  var popup_fl;
  // if (popup_var) var popup_fl = document.getElementById(popup_var + '-1-attachment' ).href;

  wget([data_fl, color_fl, popup_fl], function(polygons, colors, popups) {
    var cols = JSON.parse(colors);
    var clrs;
    if (cols.length === 1) {
      clrs = cols[0];
    } else {
      clrs = function(index, feature) { return cols[index]; };
    }

    if (popup_var) {
        var pop = function (e, feature) {
          L.popup()
            .setLatLng(e.latlng)
            .setContent(feature.properties[[popup_var]].toString())
            .openOn(map);

          console.log(feature);
          console.log(e);
        };
    } else {
        var pop = null;
    }

    var dat = JSON.parse(polygons);
    //if (popup_var) var pop = JSON.parse(popups);
    L.glify.shapes({
      map: map,
      click: pop,
      data: dat,
      color: clrs,
      opacity: opacity,
      // className: "glify-pls"
      pane: pane
    });
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
      return function(index, feature) { return cols[index]; };
    }
  }

};
