# Scales: automatic scale

Scales in tmap are configured by the family of functions with prefix
`tm_scale`. Such function should be used for the input of the `.scale`
arguments in the layer functions (e.g. `fill.scale` in
[`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md)).
The function `tm_scale()` is a scale that is set automatically given by
the data type (factor, numeric, and integer) and the visual variable.
The tmap option `scales.var` contains information which scale is applied
when.

## Usage

``` r
tm_scale(...)
```

## Arguments

- ...:

  arguments passed on to the applied scale function `tm_scale_*()`

## See also

[`tm_scale_asis()`](https://r-tmap.github.io/tmap/reference/tm_scale_asis.md),
[`tm_scale_ordinal()`](https://r-tmap.github.io/tmap/reference/tm_scale_categorical.md),
[`tm_scale_categorical()`](https://r-tmap.github.io/tmap/reference/tm_scale_categorical.md),
[`tm_scale_intervals()`](https://r-tmap.github.io/tmap/reference/tm_scale_intervals.md),
[`tm_scale_discrete()`](https://r-tmap.github.io/tmap/reference/tm_scale_discrete.md),
[`tm_scale_continuous()`](https://r-tmap.github.io/tmap/reference/tm_scale_continuous.md),
[`tm_scale_rank()`](https://r-tmap.github.io/tmap/reference/tm_scale_rank.md),
[`tm_scale_continuous_log()`](https://r-tmap.github.io/tmap/reference/tm_scale_continuous.md),
[`tm_scale_continuous_log2()`](https://r-tmap.github.io/tmap/reference/tm_scale_continuous.md),
[`tm_scale_continuous_log10()`](https://r-tmap.github.io/tmap/reference/tm_scale_continuous.md),
[`tm_scale_continuous_log1p()`](https://r-tmap.github.io/tmap/reference/tm_scale_continuous.md),
[`tm_scale_continuous_sqrt()`](https://r-tmap.github.io/tmap/reference/tm_scale_continuous.md),
[`tm_scale_continuous_pseudo_log()`](https://r-tmap.github.io/tmap/reference/tm_scale_continuous.md),
[`tm_scale_rgb()`](https://r-tmap.github.io/tmap/reference/tm_scale_rgb.md),
[`tm_scale_bivariate()`](https://r-tmap.github.io/tmap/reference/tm_scale_bivariate.md)
