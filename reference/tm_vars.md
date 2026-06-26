# tmap function to specify variables

tmap function to specify all variables in the shape object

## Usage

``` r
tm_vars(
  x = NA,
  dimvalues = NULL,
  n = NA,
  multivariate = FALSE,
  animate = FALSE
)
```

## Arguments

- x:

  variable names, variable indices, or a dimension name

- dimvalues:

  dimension values

- n:

  if specified the first `n` variables are taken (or the first `n`
  dimension values)

- multivariate:

  in case multiple variables are specified, should they serve as facets
  (FALSE) or as a multivariate visual variable?

- animate:

  should the variable(s) be animated? (experimental)

## See also

[Scales](https://r-tmap.github.io/tmap/articles/basics_scales), [Visual
variables](https://r-tmap.github.io/tmap/articles/basics_vv),
[Multivariate visual
variables](https://r-tmap.github.io/tmap/articles/adv_multivariate)
