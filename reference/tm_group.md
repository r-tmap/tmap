# Layer group control

Controls the layer groups in interactive maps (view mode): the layer
control box (radio buttons or check boxes) and at which zoom levels the
layers are displayed at.

## Usage

``` r
tm_group(name, control = NA, zoom_levels = NA)
```

## Arguments

- name:

  group name that corresponds with the group name specified in the layer
  functions (e.g.
  [`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md))

- control:

  The group control determines how layer groups can be switched on and
  off. Options: `"radio"` for radio buttons (meaning only one group can
  be shown), `"check"` for check boxes (so multiple groups can be
  shown), and `"none"` for no control (the group cannot be
  (de)selected).

- zoom_levels:

  The zoom levels at which the group is displays at. When specified
  `control` will be set to `"none"`.

## See also

[vignette about layer
groups](https://r-tmap.github.io/tmap/articles/adv_groups)
