# Plot mode options

Plot mode options. This option is specific to the plot mode.

## Usage

``` r
tm_plot(use_gradient, limit_latitude_3857)
```

## Arguments

- use_gradient:

  Use gradient fill using
  [linearGradient()](https://rdrr.io/r/grid/patterns.html)

- limit_latitude_3857:

  Vector of two limit latitude values for maps printed in Web Mercator
  projection (EPSG 3857). If `c(-90, 90)` the poles will be inflated too
  much. The Web Mercator is defines as `c(-85.06, 85.06)`, but the
  default setting in tmap is `c(-84, 84)`.
