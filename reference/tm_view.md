# View mode options

View mode options. These options are specific to the view mode.

## Usage

``` r
tm_view(
  use_browser,
  use_WebGL,
  control.position,
  control.bases,
  control.overlays,
  control.collapse,
  set_bounds,
  set_view,
  set_zoom_limits,
  use_circle_markers,
  leaflet.options,
  ...
)
```

## Arguments

- use_browser:

  If `TRUE` it opens an external browser, and `FALSE` (default) it opens
  the internal IDEs (e.g. RStudio) browser.

- use_WebGL:

  use webGL for points, lines, and polygons. For large spatial objects,
  this is much faster than the standard leaflet layer functions.
  However, it can not always be used for two reasons. First, the number
  of visual variables are limited; only fill, size, and color (for
  lines) are supported. Second, projected CRS's are not supported.
  Furthermore, it has the drawback that polygon borders are not as
  sharp. By default only `TRUE` for large spatial objects (1000 or more
  features) when the mentioned criteria are met. By default `TRUE` if no
  other visual variables are used.

- control.position:

  The position of the control. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- control.bases:

  base layers

- control.overlays:

  overlay layers

- control.collapse:

  Should the box be collapsed or expanded?

- set_bounds:

  logical that determines whether maximum bounds are set, or a bounding
  box. Not applicable in plot mode. In view mode, this is passed on to
  setMaxBounds()

- set_view:

  numeric vector that determines the view. Either a vector of three:
  `lng`, `lat`, and `zoom`, or a single value: `zoom`. See setView().
  Only applicable if `bbox` is not specified

- set_zoom_limits:

  numeric vector of two that set the minimum and maximum zoom levels
  (see tileOptions()).

- use_circle_markers:

  If `TRUE` (default) circle shaped symbols (e.g.
  [`tm_dots()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md)
  and
  [`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md))
  will be rendered as addCircleMarkers() instead of addMarkers(). The
  former is faster, the latter can support any symbol since it is based
  on icons

- leaflet.options:

  options passed on to
  [leafletOptions()](https://rstudio.github.io/leaflet/reference/leaflet.html)

- ...:

  to catch deprecated arguments

## See also

[`tm_group()`](https://r-tmap.github.io/tmap/reference/tm_group.md)
