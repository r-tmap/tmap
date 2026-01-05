# Map component: manual legend

Map component that adds a manual legend.

## Usage

``` r
tm_add_legend(
  ...,
  labels = "",
  type = "symbols",
  title = "",
  orientation = NULL,
  position = NULL,
  group_id = NA_character_,
  group = NA,
  group.control = "check",
  z = NA_integer_
)
```

## Arguments

- ...:

  visual variables and arguments passed on to
  [`tm_legend()`](https://r-tmap.github.io/tmap/reference/tm_legend.md).
  By default, the argument `type` is set to `"symbols"`, which means
  that the supported visual variables are: `"fill"`, `"col"`, `"shape"`,
  `"size"`, `"fill_alpha"`, `"col_alpha"`, `"lty"`, `"lwd"`,
  `"linejoin"`, and `"lineend"`. The number of legend items will be
  equal to the maximum number of specific values (and specified labels.)

- labels:

  labels by default `""` (so omitted)

- type:

  the layer type from which the visual variables (see `...`) are taken.
  Options: `"symbols"` (default), `"lines"`, `"polygons"`, and `"text"`.

- title:

  The title of the legend.

- orientation:

  The orientation of the legend.

- position:

  The position of the legend. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- group_id:

  Component group id name. All components (e.g. legends, titles, etc)
  with the same `group_id` will be grouped. The specifications of how
  they are placed (e.g. stacking, margins etc.) are determined in
  [`tm_components()`](https://r-tmap.github.io/tmap/reference/tm_components.md)
  where its argument `id` should correspond to `group_id`.

- group:

  Name of the group to which this layer belongs. This is only relevant
  in view mode, where layer groups can be switched (see `group.control`)

- group.control:

  In view mode, the group control determines how layer groups can be
  switched on and off. Options: `"radio"` for radio buttons (meaning
  only one group can be shown), `"check"` for check boxes (so multiple
  groups can be shown), and `"none"` for no control (the group cannot be
  (de)selected).

- z:

  z index, e.g. the place of the component relative to the other
  componets

## Examples

``` r
if (FALSE) { # \dontrun{
tm_shape(NLD_muni) +
  tm_borders() +
  tm_basemap("OpenStreetMap") +
  tm_add_legend(labels = c("Motorway", "Primary road", "Secondary road", "Railway"),
          col = c("#E892A1", "#FCD6A4", "#F8FABF", "#707070"),
          lty = c("solid", "solid", "solid", "dotted"),
          lwd = 3,
          type = "lines",
          bg.color = "grey92",
          bg.alpha = 1)
} # }
```
