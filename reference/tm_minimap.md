# Map component: minimap

Map component that adds a
[minimap](https://rstudio.github.io/leaflet/reference/addMiniMap.html)
in view mode.

## Usage

``` r
tm_minimap(
  server,
  toggle,
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
  z,
  ...
)
```

## Arguments

- server:

  name of the provider or an URL (see
  [`tm_tiles`](https://r-tmap.github.io/tmap/reference/tm_basemap.md)).
  By default, it shows the same map as the basemap, and moreover, it
  will automatically change when the user switches basemaps. Note the
  latter does not happen when `server` is specified.

- toggle:

  should the minimap have a button to minimise it? By default `TRUE`.

- width, height:

  width and height of the component.

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

- z:

  z index, e.g. the place of the component relative to the other
  componets

- ...:

  Arguments passed on to
  [`leaflet::addMiniMap`](https://rstudio.github.io/leaflet/reference/addMiniMap.html)

  `map`

  :   a map widget object

  `collapsedWidth`

  :   The width of the toggle marker and the minimap when collapsed, in
      pixels. Defaults to 19.

  `collapsedHeight`

  :   The height of the toggle marker and the minimap when collapsed, in
      pixels. Defaults to 19.

  `zoomLevelOffset`

  :   The offset applied to the zoom in the minimap compared to the zoom
      of the main map. Can be positive or negative, defaults to -5.

  `zoomLevelFixed`

  :   Overrides the offset to apply a fixed zoom level to the minimap
      regardless of the main map zoom. Set it to any valid zoom level,
      if unset zoomLevelOffset is used instead.

  `centerFixed`

  :   Applies a fixed position to the minimap regardless of the main
      map's view / position. Prevents panning the minimap, but does
      allow zooming (both in the minimap and the main map). If the
      minimap is zoomed, it will always zoom around the centerFixed
      point. You can pass in a LatLng-equivalent object. Defaults to
      false.

  `zoomAnimation`

  :   Sets whether the minimap should have an animated zoom. (Will cause
      it to lag a bit after the movement of the main map.) Defaults to
      false.

  `toggleDisplay`

  :   Sets whether the minimap should have a button to minimize it.
      Defaults to false.

  `autoToggleDisplay`

  :   Sets whether the minimap should hide automatically, if the parent
      map bounds does not fit within the minimap bounds. Especially
      useful when 'zoomLevelFixed' is set.

  `minimized`

  :   Sets whether the minimap should start in a minimized position.

  `aimingRectOptions`

  :   Sets the style of the aiming rectangle by passing in a
      Path.Options
      (<https://web.archive.org/web/20220702182250/https://leafletjs.com/reference-1.3.4.html#path-options>)
      object. (Clickable will always be overridden and set to false.)

  `shadowRectOptions`

  :   Sets the style of the aiming shadow rectangle by passing in a
      Path.Options
      (<https://web.archive.org/web/20220702182250/https://leafletjs.com/reference-1.3.4.html#path-option>)
      object. (Clickable will always be overridden and set to false.)

  `strings`

  :   Overrides the default strings allowing for translation.

  `tiles`

  :   URL for tiles or one of the pre-defined providers.

  `mapOptions`

  :   Sets Leaflet options for the MiniMap map. It does not override the
      MiniMap default map options but extends them.

## See also

[Vignette about
components](https://r-tmap.github.io/tmap/articles/basics_components)
