# Scales: as is

Scales in tmap are configured by the family of functions with prefix
`tm_scale`. Such function should be used for the input of the `.scale`
arguments in the layer functions (e.g. `fill.scale` in
[`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md)).
The function `tm_scale_asis()` is used to take data values as they are
and use them as such for the visual variable.

## Usage

``` r
tm_scale_asis(values.scale = NA, value.neutral = NA, value.na = NA, ...)
```

## Arguments

- values.scale:

  (generic scale argument) Scaling of the values. Only useful for
  size-related visual variables, such as `size` of
  [`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md)
  and `lwd` of
  [`tm_lines()`](https://r-tmap.github.io/tmap/reference/tm_lines.md).

- value.neutral:

  (generic scale argument) Value that can be considered neutral. This is
  used for legends of other visual variables of the same map layer. E.g.
  when both `fill` and `size` are used for
  [`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md)
  (using filled circles), the size legend items are filled with the
  `value.neutral` color from the `fill.scale` scale, and fill legend
  items are bubbles of size `value.neutral` from the `size.scale` scale.

- value.na:

  (generic scale argument) Value used for missing values. See tmap
  option `"value.na"` for defaults per visual variable.

- ...:

  Arguments caught (and not used) from the automatic function
  [`tm_scale()`](https://r-tmap.github.io/tmap/reference/tm_scale.md)

## See also

[`tm_scale()`](https://r-tmap.github.io/tmap/reference/tm_scale.md)
