# Map component: scale bar

Map component that adds a scale bar.

## Usage

``` r
tm_scalebar(
  breaks,
  width,
  allow_clipping,
  text.size,
  text.color,
  color.dark,
  color.light,
  lwd,
  position,
  group_id,
  bg,
  bg.color,
  bg.alpha,
  size = "deprecated",
  stack,
  frame,
  frame.color,
  frame.alpha,
  frame.lwd,
  frame.r,
  margins,
  z
)
```

## Arguments

- breaks:

  Scale bar break positions. E.g. `c(0, 10, 50)` places breaks at 0, 10,
  and 50 units. The unit is controlled by the `unit` argument from
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md).
  When `NULL` (default), break positions are chosen automatically.

- width:

  Width of the scale bar, in number of text line heights (roughly
  equivalent to character widths). When `breaks` are specified, `width`
  is only useful for fine-tuning, e.g. to prevent label clipping or
  reduce excess whitespace.

- allow_clipping:

  Should clipping of the last label be allowed? If `TRUE` (default), the
  last break label including its unit suffix is printed even when it
  extends beyond the frame. If `FALSE`, that label is suppressed and the
  unit suffix is appended to the second-to-last label instead.

- text.size:

  text size

- text.color:

  text.color

- color.dark:

  color.dark

- color.light:

  color.light

- lwd:

  linewidth

- position:

  The position specification of the component: an object created with
  [`tm_pos_in()`](https://r-tmap.github.io/tmap/reference/tm_pos.md) or
  [`tm_pos_out()`](https://r-tmap.github.io/tmap/reference/tm_pos.md).
  Or, as a shortcut, a vector of two values, specifying the x and y
  coordinates. The first is `"left"`, `"center"` or `"right"` (or upper
  case, meaning tighter to the map frame), the second `"top"`,
  `"center"` or `"bottom"`. Numeric values are also supported, where 0,
  0 means left bottom and 1, 1 right top. See also vignette:
  [Positioning of
  components](https://r-tmap.github.io/tmap/articles/adv_positions). In
  case multiple components should be combined (stacked), use `group_id`
  and specify `component` in
  [`tm_components()`](https://r-tmap.github.io/tmap/reference/tm_components.md).

- group_id:

  Component group id name. All components (e.g. legends, titles, etc)
  with the same `group_id` will be grouped. The specifications of how
  they are placed (e.g. stacking, margins etc.) are determined in
  [`tm_components()`](https://r-tmap.github.io/tmap/reference/tm_components.md)
  where its argument `id` should correspond to `group_id`.

- bg:

  Show background?

- bg.color:

  Background color

- bg.alpha:

  Background transparency

- size:

  Deprecated (use `text.size` instead)

- stack:

  stack with other map components, either `"vertical"` or
  `"horizontal"`.

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

- margins:

  margins

- z:

  z index, e.g. the place of the component relative to the other
  componets

## See also

[Components](https://r-tmap.github.io/tmap/articles/basics_components),
[Positioning of
components](https://r-tmap.github.io/tmap/articles/adv_positions),
[Grouping of
components](https://r-tmap.github.io/tmap/articles/adv_comp_group),
[Choropleth
(Netherlands)](https://r-tmap.github.io/tmap/articles/examples_choro_NLD)
