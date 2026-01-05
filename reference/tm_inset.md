# Map component: inset maps and other objects

Map component that adds an inset object, e.g. a mini map

## Usage

``` r
tm_inset(
  x = NULL,
  height,
  width,
  margins,
  between_margin,
  position,
  group_id,
  frame,
  frame.color,
  frame.alpha,
  frame.lwd,
  frame.r,
  bg,
  bg.color,
  bg.alpha,
  box_frame,
  box_frame.color,
  box_frame.alpha,
  box_frame.lwd,
  box_frame.lty,
  box_bg,
  box_bg.color,
  box_bg.alpha,
  main_frame,
  main_frame.r,
  main_frame.color,
  main_frame.alpha,
  main_frame.lwd,
  z
)
```

## Arguments

- x:

  object to draw. Can be: bounding box, tmap object, ggplot2 object,
  grob object, image file name.

- height:

  height of the component in number of text line heights.

- width:

  width of the component in number of text line heights.

- margins:

  margins

- between_margin:

  Margin between

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

- frame:

  frame should a frame be drawn?

- frame.color:

  frame color

- frame.alpha:

  frame alpha transparancy

- frame.lwd:

  frame line width

- frame.r:

  Radius of the rounded frame corners. 0 means no rounding.

- bg:

  Show background?

- bg.color:

  Background color

- bg.alpha:

  Background transparency

- box_frame:

  Should a box frame be drawn in the main map that shows where the inset
  is? `TRUE` by default

- box_frame.color, box_frame.alpha, box_frame.lwd, box_frame.lty:

  Properties of the box frame

- box_bg:

  Should the frame box have a background? `FALSE` by default

- box_bg.color, box_bg.alpha:

  Properties of the box background

- main_frame:

  Should a frame be drawn around the inset map? Note that this is
  different from the general map component frame (the argument `frame`)

- main_frame.r, main_frame.color, main_frame.alpha, main_frame.lwd:

  Properties of the main frame

- z:

  z index, e.g. the place of the component relative to the other
  componets

## Examples

``` r
## map
bb = tmaptools::bb(NLD_prov[NLD_prov$name == "Utrecht",], ext = 1.05)

bb_Randstad =
  sf::st_bbox(c(xmin = 120000, xmax = 150000, ymin = 460000, ymax = 500000), crs = 28992)

tm_shape(NLD_dist) +
  tm_polygons(
    fill = "dwelling_value",
    fill.scale = tm_scale_continuous_pseudo_log(values = "-cols4all.pu_gn_div"),
    col = NULL) +
tm_shape(NLD_muni) +
  tm_borders(col = "black", lwd = 0.5) +
tm_shape(NLD_prov) +
  tm_borders(col = "black", lwd = 1.5) +
tm_inset(bb_Randstad, height = 12, width = 12, position = c("left", "top")) +
  tm_compass(position = c("left", "top"), )


## ggplot2
if (requireNamespace("ggplot2")) {
  library(ggplot2)
  p = ggplot(World, aes(x = gender, y = press, colour = continent)) +
    geom_point() +
    theme_bw()

  tm_shape(World) +
    tm_polygons(
      fill = "gender",
      fill.scale = tm_scale(values = "-cols4all.pu_gn_div")) +
    tm_inset(p, height = 15, width = 20, position = tm_pos_in("left", "bottom"))
}
#> Warning: Removed 20 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> [plot mode] fit legend/component: Some legend items or map compoments do not
#> fit well, and are therefore rescaled.
#> ℹ Set the tmap option `component.autoscale = FALSE` to disable rescaling.
```
