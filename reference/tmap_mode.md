# Set tmap mode to static plotting or interactive viewing

- `tmap_mode()` informs of the current mode (if called without
  argument).

- `ttm()` toggles between the most recent two modes.

- `ttmp()` same as `ttm()` and calls
  [`tmap_last()`](https://r-tmap.github.io/tmap/reference/tmap_last.md)
  to display the last map in the new mode.

- `rtm()` rotate between between all modes

- `rtmp()` same as `rtm()` and calls
  [`tmap_last()`](https://r-tmap.github.io/tmap/reference/tmap_last.md)
  to display the last map in the new mode.

Set tmap mode to static plotting or interactive viewing. The global
option `tmap.mode` determines the whether thematic maps are plot in the
graphics device, or shown as an interactive leaflet map (see also
[`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md).
The function `tmap_mode()` is a wrapper to set this global option. The
convenient function `ttm()`, which stands for toggle thematic map, is a
toggle switch between the two modes. The function `ttmp()` stands for
toggle thematic map and print last map: it does the same as `ttm()`
followed by
[`tmap_last()`](https://r-tmap.github.io/tmap/reference/tmap_last.md);
in order words, it shows the last map in the other mode. It is
recommended to use `tmap_mode()` in scripts and `ttm()`/`ttmp()` in the
console.

## Usage

``` r
tmap_mode(mode = NULL)

ttm()

rtm()

ttmp()

rtmp()
```

## Arguments

- mode:

  One of `"plot"` or `"view"`. See Details for more info.

## Value

- `tmap_mode()` returns the current tmap mode invisibly (when called
  without argument). Otherwise, returns the previous mode.

- `ttm()` switches mode and returns previous tmap mode invisibly. The
  previous tmap mode before switching.

## `mode = "plot"`

Thematic maps are shown in the graphics device. This is the default
mode, and supports all tmap's features, such as small multiples (see
[`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md))
and extensive layout settings (see
[`tm_layout()`](https://r-tmap.github.io/tmap/reference/tm_layout.md)).
It is recommended to use
[`tmap_save()`](https://r-tmap.github.io/tmap/reference/tmap_save.md)
for saving static maps.

## `mode = "view"`

Thematic maps are viewed interactively in the web browser or RStudio's
Viewer pane. Maps are fully interactive with tiles from OpenStreetMap or
other map providers (see
[`tm_tiles()`](https://r-tmap.github.io/tmap/reference/tm_basemap.md)).
See also
[`tm_view()`](https://r-tmap.github.io/tmap/reference/tm_view.md) for
options related to the `"view"` mode. This mode generates a
[`leaflet::leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
widget, which can also be directly obtained with
[`tmap_leaflet()`](https://r-tmap.github.io/tmap/reference/tmap_leaflet.md).
With R Markdown, it is possible to publish it to an HTML page.

However, there are a couple of constraints in comparison to `"plot"`:

## References

Tennekes, M., 2018, tmap: Thematic Maps in R, Journal of Statistical
Software, 84(6), 1-39,
[doi:10.18637/jss.v084.i06](https://doi.org/10.18637/jss.v084.i06)

## See also

[vignette about
modes](https://r-tmap.github.io/tmap/articles/basics_modes)

- [`tmap_last()`](https://r-tmap.github.io/tmap/reference/tmap_last.md)
  to show the last map

- [`tm_view()`](https://r-tmap.github.io/tmap/reference/tm_view.md) for
  viewing options

- [`tmap_leaflet()`](https://r-tmap.github.io/tmap/reference/tmap_leaflet.md)
  for obtaining a leaflet widget

- [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md)
  for tmap options

## Examples

``` r
current.mode = tmap_mode()
#> ℹ tmap modes "plot" - "view"
#> ℹ toggle with `tmap::ttm()`
#> This message is displayed once per session.

tmap_mode("plot")
#> ℹ tmap modes "plot" - "view"

tm_shape(World) + tm_polygons("HPI")


tmap_mode("view")
#> ℹ tmap modes "plot" - "view"

tm_shape(World) + tm_polygons("HPI")

ttm()
#> ℹ tmap modes "plot" - "view"

tm_shape(World) + tm_polygons("HPI")


tmap_mode(current.mode)
#> ℹ tmap modes "plot" - "view"
```
