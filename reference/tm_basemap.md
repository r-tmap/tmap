# Map layer: basemap / overlay tiles

Map layer that draws tiles from a tile server. `tm_basemap()` draws the
tile layer as basemap, i.e. as bottom layer. In contrast, `tm_tiles()`
draws the tile layer as overlay layer, where the stacking order
corresponds with the order in which this layer is called, just like
other map layers.

## Usage

``` r
tm_basemap(
  server = NA,
  alpha = NULL,
  zoom = NULL,
  api = NULL,
  max.native.zoom = 17,
  sub = "abc",
  zindex = 0,
  group = NA,
  group.control = "radio"
)

tm_tiles(
  server = NA,
  alpha = NULL,
  zoom = NULL,
  max.native.zoom = 17,
  sub = "abc",
  zindex = NA,
  group = NA,
  group.control = "check"
)
```

## Arguments

- server:

  Name of the provider or an URL. Or a vector of multiple values. The
  list of available providers can be obtained with `providers` (tip: in
  RStudio, type `leaflet::providers$` to see the options). See
  <https://leaflet-extras.github.io/leaflet-providers/preview/> for a
  preview of those. When a URL is provided, it should be in template
  format, e.g. `"https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"`.
  Use `NULL` in `tm_basemap()` to disable basemaps. It can be a named
  vector. In that case these names will be used a group names, as
  alternative to the argument `group`.

- alpha:

  Transparency level

- zoom:

  Zoom level (only used in plot mode)

- api:

  API key. Needed for `Stadia` and `Thunderforest` maps in plot mode.
  See details

- max.native.zoom:

  Maximum native zoom level (only used in view mode). The minimum and
  maximum zoom levels are determined in
  [`tm_view()`](https://r-tmap.github.io/tmap/reference/tm_view.md).

- sub:

  subdomain of the tile server. Only used when `server` is a url
  template. The default is `"abc"` which works for most tile servers.

- zindex:

  zindex of the pane in view mode. By default, it is set to the layer
  number plus 400. By default, the tmap layers will therefore be placed
  in the custom panes `"tmap401"`, `"tmap402"`, etc., except for the
  base tile layers, which are placed in the standard `"tile"`. This
  parameter determines both the name of the pane and the z-index, which
  determines the pane order from bottom to top. For instance, if
  `zindex` is set to 500, the pane will be named `"tmap500"`.

- group:

  Name of the group to which this layer belongs. This is only relevant
  in view mode, where layer groups can be switched (see `group.control`)

- group.control:

  In view mode, the group control determines how layer groups can be
  switched on and off. Options: `"radio"` for radio buttons (meaning
  only one group can be shown), `"check"` for check boxes (so multiple
  groups can be shown), and `"none"` for no control (the group cannot be
  (de)selected).

## Details

API keys. For Stadia and Thunderforest maps, an API key is required.
This can be set via the argument `api`. Alternatively they can be stored
in environment variables `"STADIA_MAPS"` and `THUNDERFOREST_MAPS` with
`Sys.setenv`

## See also

[Basemap
examples](https://r-tmap.github.io/tmap/articles/basics_basemaps)

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("maptiles")) {
  # view mode
  current_mode = tmap_mode("view")
  tm_basemap("Stadia.StamenWatercolor") +
    tm_shape(World) +
    tm_polygons(
      "HPI",
      fill.scale = tm_scale(values = "reds"),
      fill_alpha.scale = 0.5)

  tm_shape(World, crs = "+proj=eqearth") +
    tm_polygons(
      "HPI",
      fill.scale = tm_scale(values = "reds"),
      fill_alpha.scale = 0.5) +
  tm_basemap(NULL)

  # plot mode:
  tmap_mode("plot")
  tm_basemap() +
    tm_shape(World) +
    tm_polygons("HPI")

  tm_basemap("OpenTopoMap") +
    tm_shape(World) +
    tm_polygons(fill = NA, col = "black")

  tm_basemap("CartoDB.PositronNoLabels") +
  tm_shape(NLD_prov, crs = 4236) +
    tm_borders() +
    tm_facets_wrap("name") +
    tm_tiles("CartoDB.PositronOnlyLabels")

  # restore mode
  tmap_mode(current_mode)
}
} # }
```
