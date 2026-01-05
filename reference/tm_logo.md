# Map component: logo

Map component that adds a logo.

## Usage

``` r
tm_logo(
  file,
  height,
  margins,
  between_margin,
  stack,
  position,
  group_id,
  frame,
  frame.color,
  frame.alpha,
  frame.lwd,
  frame.r,
  z
)
```

## Arguments

- file:

  either a filename or url of a png image. If multiple files/urls are
  provided with a character vector, the logos are placed near each
  other. To specify logos for small multiples use a list of character
  values/vectors. In order to stack logos vertically, multiple `tm_logo`
  elements can be stacked.

- height:

  height of the logo in number of text line heights. The width is scaled
  based the height and the aspect ratio of the logo. If multiple logos
  are specified by a vector or list, the heights can be specified
  accordingly.

- margins:

  margins

- between_margin:

  Margin between

- stack:

  stack with other map components, either `"vertical"` or
  `"horizontal"`.

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

- z:

  z index, e.g. the place of the component relative to the other
  componets

## See also

[Vignette about
components](https://r-tmap.github.io/tmap/articles/basics_components)

## Examples

``` r
data(World)

tm_shape(World) +
  tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "brewer.rd_yl_gn")) +
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
        system.file("help", "figures", "logo.png", package = "tmap"))) +
  tm_logo("http://blog.kulikulifoods.com/wp-content/uploads/2014/10/logo.png",
      height=5, position = c("left", "top"))
```
