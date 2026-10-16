# tmap extensions: tmap.cartogram

## Cartograms

In cartograms, areal regions are distorted such that the obtained area
sizes are proportional to a quantitative variable. There are three
types: contiguous, non-contiguous and dorling cartogram.

The extension package
[**tmap.cartogram**](https://r-tmap.github.io/tmap.cartogram/) provides
new layer functions for tmap to create various types of cartograms.

It is strongly recommended to use an equal area crs (see
[instructions](https://r-tmap.github.io/tmap.cartogram/articles/intro#map-projection)).

## Contiguous cartogram

The shapes of the polygons are distorted, where the neighborhood
relationships between them are preserved as much as possible.

Text labels follow the distorted shapes. With `col = "auto"`, each label
is black or white, depending on the fill color underneath.

``` r

Africa = World[World$continent == "Africa", ]

tm_shape(Africa, crs = "+proj=robin") +
  tm_cartogram(
    size = "*pop_est", 
    fill = "life_exp",
    fill.scale = tm_scale_intervals(values = "-cols4all.bu_br_div"),
    fill.legend = tm_legend("Age"),
    options = opt_tm_cartogram(itermax = 15)) +
  tm_text("name", col = "auto", size = 0.6) +
tm_title("Life Expectancy") +
tm_animate_fast(play = "pingpong")
#> Cartogram in progress...
```

![](ext_cartogram_files/figure-html/unnamed-chunk-3-1.gif)

## Non-contiguous cartograms

Here, the polygons are only resized while they keep their shape.

``` r

tm_shape(Africa, crs = "+proj=robin") +
  tm_cartogram_ncont(
    size = "pop_est", 
    fill = "inequality",
    fill.scale = tm_scale_continuous(values = "cols4all.pu_gn_div", values.range = c(0, 0.5)),
    fill.legend = tm_legend(""),
    options = opt_tm_cartogram_ncont()) +
  tm_text("name", col = "auto", options = opt_tm_text(point.label = TRUE)) +
tm_title("Income inequality (Gini coefficient)")
#> Cartogram in progress...
```

![](ext_cartogram_files/figure-html/unnamed-chunk-4-1.png)

## Dorling cartogram

Polygons are replaced by non-overlapping bubbles. The labels are scaled
with the bubbles (a square root scale, since the bubble areas are
proportional to `pop_est`).

``` r

tm_shape(World, crs = "+proj=robin") +
  tm_cartogram_dorling(
    size = "pop_est", 
    fill = "press",
    fill.scale = tm_scale_continuous(values = "cols4all.pu_gn_div", midpoint = 50),
    fill.legend = tm_legend("", height = 30)) +
  tm_text("iso_a3", col = "auto", size = "pop_est",
    size.scale = tm_scale_continuous_sqrt(values.scale = 1.2),
    size.legend = tm_legend_hide()) +
tm_title("World Press Freedom Index")
#> Cartogram in progress...
```

![](ext_cartogram_files/figure-html/unnamed-chunk-5-1.png)
