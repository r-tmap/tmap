# Map component: compass

Map component that adds a compass

## Usage

``` r
tm_compass(
  north,
  type,
  text.size,
  size,
  show.labels,
  cardinal.directions,
  text.color,
  color.dark,
  color.light,
  lwd,
  position,
  group_id,
  bg,
  bg.color,
  bg.alpha,
  stack,
  just,
  frame,
  frame.color,
  frame.alpha,
  frame.lwd,
  frame.r,
  margins,
  z,
  ...
)
```

## Arguments

- north:

  north

- type:

  compass type, one of: `"arrow"`, `"4star"`, `"8star"`, `"radar"`,
  `"rose"`. The default is controlled by
  [`tm_layout`](https://r-tmap.github.io/tmap/reference/tm_layout.md)
  (which uses `"arrow"` for the default style)

- text.size:

  text.size

- size:

  size

- show.labels:

  show.labels

- cardinal.directions:

  cardinal.directions

- text.color:

  text.color

- color.dark:

  color.dark

- color.light:

  color.light

- lwd:

  lwd

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

- bg:

  Show background?

- bg.color:

  Background color

- bg.alpha:

  Background transparency

- stack:

  stack with other map components, either `"vertical"` or
  `"horizontal"`.

- just:

  just

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

- ...:

  to catch deprecated arguments (alpha)

## See also

[Vignette about
components](https://r-tmap.github.io/tmap/articles/basics_components)
