# Legend charts

Legend charts are small charts that are added to the map, usually in
addition to legends.

## Usage

``` r
tm_chart_histogram(
  breaks,
  plot.axis.x,
  plot.axis.y,
  extra.ggplot2,
  position,
  group_id,
  width,
  height,
  stack,
  z,
  ...
)

tm_chart_bar(
  plot.axis.x,
  plot.axis.y,
  extra.ggplot2,
  position,
  group_id,
  width,
  height,
  stack,
  z,
  ...
)

tm_chart_donut(position, group_id, width, height, stack, z, ...)

tm_chart_violin(position, group_id, width, height, stack, z, ...)

tm_chart_box(position, group_id, width, height, stack, z, ...)

tm_chart_none()

tm_chart_heatmap(position, group_id, width, height, stack, z, ...)
```

## Arguments

- breaks:

  The breaks of the bins (for histograms)

- plot.axis.x, plot.axis.y:

  Should the x axis and y axis be plot?

- extra.ggplot2:

  Extra ggplot2 code

- position:

  The position specification of the component: an object created with
  [`tm_pos_in()`](https://r-tmap.github.io/tmap/reference/tm_pos.md) or
  [`tm_pos_out()`](https://r-tmap.github.io/tmap/reference/tm_pos.md).
  Or, as a shortcut, a vector of two values, specifying the x and y
  coordinates. The first is `"left"`, `"center"` or `"right"` (or upper
  case, meaning tighter to the map frame), the second `"top"`,
  `"center"` or `"bottom"`. Numeric values are also supported, where 0,
  0 means left bottom and 1, 1 right top. See also [vignette about
  positioning](https://r-tmap.github.io/tmap/articles/adv_positions). In
  case multiple components should be combined (stacked), use `group_id`
  and specify `component` in
  [`tm_components()`](https://r-tmap.github.io/tmap/reference/tm_components.md).

- group_id:

  Component group id name. All components (e.g. legends, titles, etc)
  with the same `group_id` will be grouped. The specifications of how
  they are placed (e.g. stacking, margins etc.) are determined in
  [`tm_components()`](https://r-tmap.github.io/tmap/reference/tm_components.md)
  where its argument `id` should correspond to `group_id`.

- width, height:

  width and height of the component.

- stack:

  stack with other map components, either `"vertical"` or
  `"horizontal"`.

- z:

  z index, e.g. the place of the component relative to the other
  componets

- ...:

  passed on to
  [`tm_title()`](https://r-tmap.github.io/tmap/reference/tm_title.md)

## Details

Note that these charts are different from charts drawn inside the map.
Those are called glyphs (to be implemented).

## See also

[Vignette about
charts](https://r-tmap.github.io/tmap/articles/basics_charts)

## Examples

``` r
tm_shape(World) +
  tm_polygons("HPI",
    fill.scale = tm_scale_intervals(),
    fill.chart = tm_chart_histogram())
```
