# Map component: (credits) text

Map component that adds a text, typically used as credits. This function
is the same as
[`tm_title()`](https://r-tmap.github.io/tmap/reference/tm_title.md) but
with different default values.

## Usage

``` r
tm_credits(
  text,
  size,
  color,
  padding,
  fontface,
  fontfamily,
  alpha,
  stack,
  just,
  frame,
  frame.lwd,
  frame.r,
  bg,
  bg.color,
  bg.alpha,
  position,
  group_id,
  width,
  height,
  z,
  ...
)
```

## Arguments

- text:

  text

- size:

  font size

- color:

  font color

- padding:

  padding

- fontface:

  font face, bold, italic

- fontfamily:

  font family

- alpha:

  alpha transparency of the text

- stack:

  stack with other map components, either `"vertical"` or
  `"horizontal"`.

- just:

  just

- frame:

  frame should a frame be drawn?

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

- z:

  z index, e.g. the place of the component relative to the other
  componets

- ...:

  to catch deprecated arguments

## See also

[Vignette about
components](https://r-tmap.github.io/tmap/articles/basics_components)
