# Scales: RGB

Scales in tmap are configured by the family of functions with prefix
`tm_scale`. Such function should be used for the input of the `.scale`
arguments in the layer functions (e.g. `fill.scale` in
[`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md)).
The function `tm_scale_rgb()` is used to transform r, g, b band
variables to colors. This function is adopted from (and works similar
as)
[`stars::st_rgb()`](https://r-spatial.github.io/stars/reference/st_rgb.html)

## Usage

``` r
tm_scale_rgb(
  value.na = NA,
  stretch = FALSE,
  probs = c(0, 1),
  max_color_value = 255L
)

tm_scale_rgba(
  value.na = NA,
  stretch = FALSE,
  probs = c(0, 1),
  max_color_value = 255
)
```

## Arguments

- value.na:

  value for missing values

- stretch:

  should each (r, g, b) band be stretched? Possible values: `"percent"`
  (same as `TRUE`), `"histogram"`, `FALSE`. In the first case, the
  values are stretched to `probs[1]...probs[2]`. In the second case, a
  histogram equalization is performed

- probs:

  probability (quantile) values when `stretch = "percent"`

- max_color_value:

  maximum value

## See also

[`tm_scale()`](https://r-tmap.github.io/tmap/reference/tm_scale.md) and
[`stars::st_rgb()`](https://r-spatial.github.io/stars/reference/st_rgb.html)

## Examples

``` r
if (FALSE) { # \dontrun{
require(stars)
file = system.file("tif/L7_ETMs.tif", package = "stars")

L7 = stars::read_stars(file)

tm_shape(L7) +
  tm_rgb(col.scale = tm_scale_rgb(probs = c(0, .99), stretch = TRUE))

tm_shape(L7) +
  tm_rgb(col.scale = tm_scale_rgb(stretch = "histogram"))
} # }
```
