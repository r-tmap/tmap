# Export tmap to the format of the used graphics mode

- `tmap_grob()` returns a
  [`grob`](https://rdrr.io/r/grid/grid-defunct.html) object
  (`"plot" mode`)

- `tmap_leaflet()` a
  [`leaflet`](https://rstudio.github.io/leaflet/reference/leaflet.html)
  object (`"view"` mode).

## Usage

``` r
tmap_leaflet(x, show = FALSE, ...)

tmap_grob(x, asp = NA, scale = 1, show = FALSE, ...)
```

## Arguments

- x:

  a tmap object.

- show:

  show the map?

- ...:

  Arguments passed on to
  [`print.tmap`](https://r-tmap.github.io/tmap/reference/print.tmap.md)

  `return.asp`

  :   should the aspect ratio be returned?

  `vp`

  :   viewport (for `"plot"` mode)

  `knit`

  :   A logical, should knit?

  `in.shiny`

  :   A logical, is the map drawn in **shiny**?

  `proxy`

  :   A logical, if `in.shiny`, is
      [`tmapProxy()`](https://r-tmap.github.io/tmap/reference/renderTmap.md)
      used?

  `options`

  :   A vector of options

- asp, scale:

  the desired aspect ratio and scale of the map. Only applicable for
  `"plot"` mode.

## Value

- `tmap_grob()` returns a
  [`grob`](https://rdrr.io/r/grid/grid-defunct.html) object (`"plot"`
  mode)

- `tmap_leaflet()` a
  [`leaflet`](https://rstudio.github.io/leaflet/reference/leaflet.html)
  object (`"view"` mode). In case small multiples are shown, a list is
  returned.

## Examples

``` r
map = tm_shape(World) + tm_polygons()
tmap_leaflet(map, show = TRUE)
```
