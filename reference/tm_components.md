# Group components

Group components

## Usage

``` r
tm_components(
  group_id = "",
  position,
  stack,
  frame_combine,
  equalize,
  resize_as_group,
  stack_margin,
  offset,
  frame,
  frame.color,
  frame.alpha,
  frame.lwd,
  frame.r,
  bg,
  bg.color,
  bg.alpha
)
```

## Arguments

- group_id:

  id of the component group. By default set to `""`, which will apply to
  all components. There are two other options. 1) To use the same
  (self-chosen) name that corresponds to the `group_id` argument of a
  component function, such as
  [`tm_legend()`](https://r-tmap.github.io/tmap/reference/tm_legend.md)
  and
  [`tm_title()`](https://r-tmap.github.io/tmap/reference/tm_title.md). 2)
  To specify one (or more) component function names, e.g. `"tm_legend"`
  or `c("tm_scalebar", "tm_compass")`.

- position:

  The position specification of the components in this group: an object
  created with
  [`tm_pos_in()`](https://r-tmap.github.io/tmap/reference/tm_pos.md) or
  [`tm_pos_out()`](https://r-tmap.github.io/tmap/reference/tm_pos.md).
  Or, as a shortcut, a vector of two values, specifying the x and y
  coordinates. The first is `"left"`, `"center"` or `"right"` (or upper
  case, meaning tighter to the map frame), the second `"top"`,
  `"center"` or `"bottom"`. Numeric values are also supported, where 0,
  0 means left bottom and 1, 1 right top. See also [vignette about
  positioning](https://r-tmap.github.io/tmap/articles/adv_positions).

- stack:

  stacking `"horizontal"` or `"vertical"`

- frame_combine:

  put frame around all components that are drawn on the same location.
  Whether a frame is drawn is still decided by the `frame` argument of
  the 'main' (first) component.

- equalize:

  in case `frame_combine` is `FALSE`, should the separate frames be
  equalized, i.e. have the same width (when stacked vertically) or
  height (when stacked horizontally)?

- resize_as_group:

  in case a component if rescaled because of the limited space, rescale
  the other components proportionally?

- stack_margin:

  Margin between components

- offset:

  Offset margin between frame and the components block

- frame:

  Should a frame be drawn? By default `TRUE` for legends, charts and
  insets, and `FALSE` otherwise.

- frame.color:

  frame color

- frame.alpha:

  frame alpha transparancy

- frame.lwd:

  frame line width

- frame.r:

  Radius of the rounded frame corners. 0 means no rounding.

- bg:

  Background color the components block. Is usually set in each
  component function, but if specified here, it will overwrite them.

- bg.color:

  Background color the components block. Is usually set in each
  component function, but if specified here, it will overwrite them.

- bg.alpha:

  Background alpha transparency of the components block. Is usually set
  in each component function, but if specified here, it will overwrite
  them.

## Value

A
[`tmap-element`](https://r-tmap.github.io/tmap/reference/tmap-element.md)
