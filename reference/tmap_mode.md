# Set tmap mode

- `tmap_mode()` gets (no argument) or sets the current mode.

- `ttm()` toggles between the two most recent modes.

- `ttmp()` same as `ttm()`, then calls
  [`tmap_last()`](https://r-tmap.github.io/tmap/reference/tmap_last.md).

- `rtm()` rotates through all modes in the pool.

- `rtmp()` same as `rtm()`, then calls
  [`tmap_last()`](https://r-tmap.github.io/tmap/reference/tmap_last.md).

- `tmap_mode_pool()` restricts which modes are cycled by `ttm()` and
  `rtm()`. Call without arguments to inspect the current pool, or pass
  `NULL` to reset.

It is recommended to use `tmap_mode()` in scripts and `ttm()`/`ttmp()`
in the console.

## Usage

``` r
tmap_mode(mode = NULL, silent = FALSE)

ttm()

rtm()

tmap_mode_pool(modes = NULL, silent = FALSE)

ttmp()

rtmp()
```

## Arguments

- mode:

  A string specifying the mode. See
  [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md)
  for available modes.

- silent:

  Should the mode be switched silently? Default `FALSE`.

- modes:

  Character vector of mode names (minimum 2), or `NULL` to reset.

## Value

- `tmap_mode()` returns the current mode invisibly when called without
  argument, otherwise the previous mode.

- `ttm()`, `rtm()` return the previous mode invisibly.

- `tmap_mode_pool()` returns the previous pool invisibly.

## Details

The default modes are `"plot"` (static, graphics device) and `"view"`
(interactive, browser or RStudio Viewer). Additional modes such as
`"maplibre"` and `"mapbox"` become available when tmap.mapgl is loaded.

## References

Tennekes, M., 2018, tmap: Thematic Maps in R, Journal of Statistical
Software, 84(6), 1-39,
[doi:10.18637/jss.v084.i06](https://doi.org/10.18637/jss.v084.i06)

## See also

- .doc_see_also_modes()

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
