# Map component: scale bar

This function was renamed to
[`tm_scalebar()`](https://r-tmap.github.io/tmap/reference/tm_scalebar.md)
in tmap v4.0.

## Usage

``` r
tm_scale_bar(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`tm_scalebar`](https://r-tmap.github.io/tmap/reference/tm_scalebar.md)

  `breaks`

  :   breaks. E.g. `c(0, 10, 50)` places scale bar breaks at 0, 10, and
      50 units. These units are specified in
      [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md).

  `width`

  :   width of the scale bar. Units are number of text line heights,
      which is similar to the number of characters. In case `beaks` are
      specified, the `width` is only handy to finetune the approximated
      width, e.g. in case clipping of the labels occurs, or there is too
      much whitespace.

  `allow_clipping`

  :   should clipping of the last label by allowed? If `TRUE` (default),
      the last break label including unit is printed even when it
      doesn't fit the frame. If `FALSE` it will not be printed. Instead
      the unit suffix is added to the second last label.

  `text.size`

  :   text size

  `text.color`

  :   text.color

  `color.dark`

  :   color.dark

  `color.light`

  :   color.light

  `lwd`

  :   linewidth

  `size`

  :   Deprecated (use `text.size` instead)

  `stack`

  :   stack with other map components, either `"vertical"` or
      `"horizontal"`.

  `margins`

  :   margins

  `frame`

  :   frame should a frame be drawn?

  `frame.color`

  :   frame color

  `frame.alpha`

  :   frame alpha transparancy

  `frame.lwd`

  :   frame line width

  `frame.r`

  :   Radius of the rounded frame corners. 0 means no rounding.

  `bg`

  :   Show background?

  `bg.color`

  :   Background color

  `bg.alpha`

  :   Background transparency

  `position`

  :   The position specification of the component: an object created
      with
      [`tm_pos_in()`](https://r-tmap.github.io/tmap/reference/tm_pos.md)
      or
      [`tm_pos_out()`](https://r-tmap.github.io/tmap/reference/tm_pos.md).
      Or, as a shortcut, a vector of two values, specifying the x and y
      coordinates. The first is `"left"`, `"center"` or `"right"` (or
      upper case, meaning tighter to the map frame), the second `"top"`,
      `"center"` or `"bottom"`. Numeric values are also supported, where
      0, 0 means left bottom and 1, 1 right top. See also [vignette
      about
      positioning](https://r-tmap.github.io/tmap/articles/adv_positions).
      In case multiple components should be combined (stacked), use
      `group_id` and specify `component` in
      [`tm_components()`](https://r-tmap.github.io/tmap/reference/tm_components.md).

  `group_id`

  :   Component group id name. All components (e.g. legends, titles,
      etc) with the same `group_id` will be grouped. The specifications
      of how they are placed (e.g. stacking, margins etc.) are
      determined in
      [`tm_components()`](https://r-tmap.github.io/tmap/reference/tm_components.md)
      where its argument `id` should correspond to `group_id`.

  `z`

  :   z index, e.g. the place of the component relative to the other
      componets
