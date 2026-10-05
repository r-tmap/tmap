# tmap basics: charts

## Introduction

Each map variable (e.g. `fill` in
[`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md))
has an additional `.chart` argument via which charts can be shown:

``` r

tm_shape(World) +
  tm_polygons(
    fill = "press",
    fill.scale = tm_scale_intervals(n=10, values = "scico.hawaii"),
    fill.legend = tm_legend("World Press\nFreedom Index"),
    fill.chart = tm_chart_bar()) +
tm_crs("auto")
```

![](basics_charts_files/figure-html/unnamed-chunk-3-1.png)

## Chart types

### Numeric variables

``` r

tm_shape(World) +
  tm_polygons("HPI",
    fill.scale = tm_scale_intervals(),
    fill.chart = tm_chart_donut())
#> [tip] Consider a suitable map projection, e.g. by adding `+ tm_crs("auto")`.
#> This message is displayed once per session.
```

![](basics_charts_files/figure-html/unnamed-chunk-4-1.png)

``` r

tm_shape(World) +
  tm_polygons("HPI",
    fill.scale = tm_scale_intervals(),
    fill.chart = tm_chart_box())
```

![](basics_charts_files/figure-html/unnamed-chunk-5-1.png)

``` r

tm_shape(World) +
  tm_polygons("HPI",
    fill.scale = tm_scale_intervals(),
    fill.chart = tm_chart_violin())
```

![](basics_charts_files/figure-html/unnamed-chunk-6-1.png)

### Categorical variable

``` r

tm_shape(World) +
  tm_polygons("economy",
    fill.scale = tm_scale_categorical(),
    fill.chart = tm_chart_bar())
```

![](basics_charts_files/figure-html/unnamed-chunk-7-1.png)

``` r

tm_shape(World) +
  tm_polygons("economy",
    fill.scale = tm_scale_categorical(),
    fill.chart = tm_chart_donut())
```

![](basics_charts_files/figure-html/unnamed-chunk-8-1.png)

### Bivariate charts

``` r

tm_shape(World) +
  tm_polygons(tm_vars(c("HPI", "well_being"), multivariate = TRUE),
    fill.chart = tm_chart_heatmap())
#> bivariate legend Labels abbreviated by the first two letters, e.g.: "2.0 - 2.9"
#> => "2".
#> This message is displayed once per session.
```

![](basics_charts_files/figure-html/unnamed-chunk-9-1.png)

## Position

We can update the position of the chart to bottom right (in a separate
frame). See [vignette about
positioning](https://r-tmap.github.io/tmap/articles/adv_positions).

``` r

tm_shape(World) +
  tm_polygons(
    fill = "press",
    fill.scale = tm_scale_intervals(n=10, values = "scico.hawaii"),
    fill.legend = tm_legend("World Press\nFreedom Index"),
    fill.chart = tm_chart_bar(position = tm_pos_out("center", "bottom", pos.h = "right"))) +
tm_crs("auto")
```

![](basics_charts_files/figure-html/unnamed-chunk-10-1.png)

Or, in case we would like the chart to be next to the legend, but in a
different frame:

``` r

tm_shape(World) +
  tm_polygons(
    fill = "press",
    fill.scale = tm_scale_intervals(n=10, values = "scico.hawaii"),
    fill.legend = tm_legend("World Press\nFreedom Index", group.frame = FALSE),
    fill.chart = tm_chart_bar(position = tm_pos_out("center", "bottom", align.v = "top"))) +
    tm_layout(component.stack_margin = .5) +
tm_crs("auto")
#> Warning: Component group arguments, such as `group.frame`, are deprecated as of 4.1.
#> Please use `group_id = "ID"` in combination with `tm_components(frame_combine =
#> FALSE)` instead.
```

![](basics_charts_files/figure-html/unnamed-chunk-11-1.png)

## Additional ggplot2 code

``` r

require(ggplot2)
#> Loading required package: ggplot2
tm_shape(World) +
  tm_polygons("HPI",
    fill.scale = tm_scale_intervals(),
    fill.chart = tm_chart_bar(
      extra.ggplot2 = theme(
        panel.grid.major.y = element_line(colour = "red")
      ))
    )
```

![](basics_charts_files/figure-html/unnamed-chunk-12-1.png)

## Frame and background

The frame and background around a chart are drawn by tmap, not by
ggplot2: they belong to the group of map components that the chart is
part of (see [vignette about grouping of
components](https://r-tmap.github.io/tmap/articles/adv_comp_group)).
Therefore, they cannot be changed with ggplot2 code, such as
`theme(plot.background = element_rect(colour = NA))` via
`extra.ggplot2`. Instead, use the `frame` and `bg` arguments of the
chart function:

``` r

tm_shape(World) +
  tm_polygons(
    fill = "life_exp",
    fill.legend = tm_legend_hide(),
    fill.chart = tm_chart_histogram(
      position = tm_pos_in("left", "bottom"),
      frame = FALSE,
      bg.color = "#FFC067")) +
tm_crs("auto")
```

![](basics_charts_files/figure-html/unnamed-chunk-13-1.png)

These arguments are passed on to the group, so they apply to all
components in it. Alternatively, they can be set for all charts at once
with `tm_components("tm_chart", frame = FALSE, bg.color = "#FFC067")`.
