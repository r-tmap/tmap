# tmap options

Get or set the tmap options globally. For map specific options, we
recommend to use
[`tm_options()`](https://r-tmap.github.io/tmap/reference/tm_options.md)
or [`tm_layout()`](https://r-tmap.github.io/tmap/reference/tm_layout.md)
via which the layout-related options can be set. `tmap_options()`
functions similar to
[`base::options()`](https://rdrr.io/r/base/options.html).

## Usage

``` r
tm_check_fix()

tmap_options(
  ...,
  crs,
  facet.max,
  free.scales,
  raster.max_cells,
  raster.warp,
  show.messages,
  show.warnings,
  output.format,
  output.size,
  output.dpi,
  animation.dpi,
  value.const,
  value.na,
  value.null,
  value.blank,
  values.var,
  values.range,
  value.neutral,
  values.scale,
  scales.var,
  scale.misc.args,
  continuous.nclass_per_legend_break,
  continuous.nclasses,
  label.format,
  label.na,
  scale,
  asp,
  bg,
  bg.color,
  outer.bg,
  outer.bg.color,
  frame,
  frame.color,
  frame.alpha,
  frame.lwd,
  frame.r,
  frame.double_line,
  outer.margins,
  inner.margins,
  inner.margins.extra,
  meta.margins,
  meta.auto_margins,
  between_margin,
  panel.margin,
  xlab.show,
  xlab.text,
  xlab.size,
  xlab.color,
  xlab.rotation,
  xlab.space,
  xlab.fontface,
  xlab.fontfamily,
  xlab.alpha,
  xlab.side,
  ylab.show,
  ylab.text,
  ylab.size,
  ylab.color,
  ylab.rotation,
  ylab.space,
  ylab.fontface,
  ylab.fontfamily,
  ylab.alpha,
  ylab.side,
  panel.type,
  panel.wrap.pos,
  panel.xtab.pos,
  unit,
  color.sepia_intensity,
  color.saturation,
  color_vision_deficiency_sim,
  text.fontface,
  text.fontfamily,
  r,
  component.position,
  component.offset,
  component.stack_margin,
  component.autoscale,
  component.resize_as_group,
  component.frame_combine,
  component.stack,
  legend.stack,
  chart.stack,
  component.equalize,
  component.frame,
  component.frame.color,
  component.frame.alpha,
  component.frame.lwd,
  component.frame.r,
  component.bg,
  component.bg.color,
  component.bg.alpha,
  legend.show,
  legend.orientation,
  legend.position,
  legend.width,
  legend.height,
  legend.reverse,
  legend.na.show,
  legend.title.color,
  legend.title.size,
  legend.title.fontface,
  legend.title.fontfamily,
  legend.title.alpha,
  legend.xlab.color,
  legend.xlab.size,
  legend.xlab.rot,
  legend.xlab.fontface,
  legend.xlab.fontfamily,
  legend.xlab.alpha,
  legend.ylab.color,
  legend.ylab.size,
  legend.ylab.rot,
  legend.ylab.fontface,
  legend.ylab.fontfamily,
  legend.ylab.alpha,
  legend.text.color,
  legend.text.size,
  legend.text.fontface,
  legend.text.fontfamily,
  legend.text.alpha,
  legend.frame,
  legend.frame.color,
  legend.frame.alpha,
  legend.frame.lwd,
  legend.frame.r,
  legend.bg,
  legend.bg.color,
  legend.bg.alpha,
  legend.only,
  legend.absolute_fontsize,
  legend.settings.portrait,
  legend.settings.landscape,
  add_legend.position,
  chart.show,
  chart.plot.axis.x,
  chart.plot.axis.y,
  chart.position,
  chart.width,
  chart.height,
  chart.reverse,
  chart.na.show,
  chart.title.color,
  chart.title.size,
  chart.title.fontface,
  chart.title.fontfamily,
  chart.title.alpha,
  chart.xlab.color,
  chart.xlab.size,
  chart.xlab.fontface,
  chart.xlab.fontfamily,
  chart.xlab.alpha,
  chart.ylab.color,
  chart.ylab.size,
  chart.ylab.fontface,
  chart.ylab.fontfamily,
  chart.ylab.alpha,
  chart.text.color,
  chart.text.size,
  chart.text.fontface,
  chart.text.fontfamily,
  chart.text.alpha,
  chart.frame,
  chart.frame.color,
  chart.frame.alpha,
  chart.frame.lwd,
  chart.frame.r,
  chart.bg,
  chart.bg.color,
  chart.bg.alpha,
  chart.object.color,
  title.size,
  title.color,
  title.fontface,
  title.fontfamily,
  title.alpha,
  title.padding,
  title.frame,
  title.frame.color,
  title.frame.alpha,
  title.frame.lwd,
  title.frame.r,
  title.position,
  title.width,
  credits.size,
  credits.color,
  credits.fontface,
  credits.fontfamily,
  credits.alpha,
  credits.padding,
  credits.position,
  credits.width,
  credits.height,
  compass.north,
  compass.type,
  compass.text.size,
  compass.size,
  compass.show.labels,
  compass.cardinal.directions,
  compass.text.color,
  compass.color.dark,
  compass.color.light,
  compass.lwd,
  compass.margins,
  compass.position,
  inset.position,
  logo.height,
  logo.margins,
  logo.between_margin,
  logo.position,
  inset_map.height,
  inset_map.width,
  inset_map.margins,
  inset_map.between_margin,
  inset_map.position,
  inset_map.frame,
  inset.height,
  inset.width,
  inset.margins,
  inset.between_margin,
  inset.frame,
  inset.bg,
  inset.bg.color,
  inset.bg.alpha,
  inset_grob.height,
  inset_grob.width,
  inset_gg.height,
  inset_gg.width,
  scalebar.breaks,
  scalebar.width,
  scalebar.allow_clipping,
  scalebar.text.size,
  scalebar.text.color,
  scalebar.text.fontface,
  scalebar.text.fontfamily,
  scalebar.color.dark,
  scalebar.color.light,
  scalebar.lwd,
  scalebar.size,
  scalebar.margins,
  scalebar.position,
  grid.show,
  grid.labels.pos,
  grid.x,
  grid.y,
  grid.n.x,
  grid.n.y,
  grid.crs,
  grid.col,
  grid.lwd,
  grid.alpha,
  grid.labels.show,
  grid.labels.size,
  grid.labels.col,
  grid.labels.fontface,
  grid.labels.fontfamily,
  grid.labels.rot,
  grid.labels.format,
  grid.labels.cardinal,
  grid.labels.margin.x,
  grid.labels.margin.y,
  grid.labels.space.x,
  grid.labels.space.y,
  grid.labels.inside_frame,
  grid.ticks,
  grid.lines,
  grid.ndiscr,
  mouse_coordinates.position,
  minimap.server,
  minimap.toggle,
  minimap.position,
  panel.show,
  panel.labels,
  panel.label.size,
  panel.label.color,
  panel.label.fontface,
  panel.label.fontfamily,
  panel.label.alpha,
  panel.label.bg,
  panel.label.bg.color,
  panel.label.bg.alpha,
  panel.label.frame,
  panel.label.frame.color,
  panel.label.frame.alpha,
  panel.label.frame.lwd,
  panel.label.frame.r,
  panel.label.height,
  panel.label.rot,
  qtm.scalebar,
  qtm.minimap,
  qtm.mouse_coordinates,
  earth_boundary,
  earth_boundary.color,
  earth_boundary.lwd,
  earth_datum,
  space,
  space.color,
  space_overlay,
  check_and_fix,
  basemap.show,
  basemap.server,
  basemap.alpha,
  basemap.zoom,
  tiles.show,
  tiles.server,
  tiles.alpha,
  tiles.zoom,
  attr.color,
  crs_extra,
  crs_global,
  crs_basemap,
  use_gradient,
  use_browser,
  use_WebGL,
  control.position,
  control.bases,
  control.overlays,
  control.collapse,
  set_bounds,
  set_view,
  set_zoom_limits,
  use_circle_markers,
  leaflet.options,
  title = NULL,
  main.title = NULL,
  main.title.size = NULL,
  main.title.color = NULL,
  main.title.fontface = NULL,
  main.title.fontfamily = NULL,
  main.title.position = NULL,
  fontface = NULL,
  fontfamily = NULL
)

tmap_options_mode(
  mode = NA,
  style = NULL,
  mode.specific = TRUE,
  default.options = FALSE
)

tmap_options_diff()

tmap_options_reset()

tmap_options_save(style)
```

## Arguments

- ...:

  List of tmap options to be set, or option names (characters) to be
  returned (see details)

- crs:

  Map crs (see
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md)).
  `NA` means the crs is specified in
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md).
  The crs that is used by the transformation functions is defined in
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md).

- facet.max:

  Maximum number of facets

- free.scales:

  For backward compatibility: if this value is set, it will be used to
  impute the free arguments in the layer functions

- raster.max_cells:

  Maximum number of raster grid cells. Can be mode specific
  `c(plot = 3000, view = 1000, 1000)` (the last value is the fall back
  default)

- raster.warp:

  Should rasters be warped or transformed in case a different projection
  (crs) is used? Warping creates a new regular raster in the target crs,
  whereas transforming creates a (usually non-regular) raster in the
  target crs. The former is lossy, but much faster and is therefore the
  default. When a different projection (crs) is used, a (usually)
  regular raster will be

- show.messages:

  Show messages?

- show.warnings:

  Show warnings?

- output.format:

  Output format

- output.size:

  Output size

- output.dpi:

  Output dpi

- animation.dpi:

  Output dpi for animations

- value.const:

  Default visual value constants e.g. the default fill color for
  `tm_shape(World) + tm_polygons()`. A list is required with per visual
  variable a value.

- value.na:

  Default visual values that are used to visualize NA data values. A
  list is required with per visual variable a value.

- value.null:

  Default visual values that are used to visualize null (out-of-scope)
  data values. A list is required with per visual variable a value.

- value.blank:

  Default visual values that correspond to blank. For color these are
  `"#00000000"` meaning transparent. A list is required with per visual
  variable a value.

- values.var:

  Default values when a data variable to mapped to a visual variable,
  e.g. a color palette. A list is required with per visual variable a
  value.

- values.range:

  Default range for values. See `values.range` of
  [`tm_scale_categorical()`](https://r-tmap.github.io/tmap/reference/tm_scale_categorical.md).
  A list is required with per visual variable a value.

- value.neutral:

  Default values for when a data variable to mapped to a visual
  variable, e.g. a color palette. A list is required with per visual
  variable a value.

- values.scale:

  Default scales (as in object sizes) for values. See `values.range` of
  [`tm_scale_categorical()`](https://r-tmap.github.io/tmap/reference/tm_scale_categorical.md).
  A list is required with per visual variable a value.

- scales.var:

  Default scale functions per visual variable and type of data variable.
  A list is required with per visual variable per data type.

- scale.misc.args:

  Default values of scale function-specific arguments. A list is
  required with per scale function and optional per visual variable.

- continuous.nclass_per_legend_break:

  The number of continuous legend breaks within one 'unit' (label). The
  default value is 50.

- continuous.nclasses:

  the number of classes of a continuous scale. Should be odd. The
  default value is 101.

- label.format:

  Format for the labels. These are the default values for
  [`tm_label_format()`](https://r-tmap.github.io/tmap/reference/tm_label_format.md)

- label.na:

  Default label for missing values.

- scale:

  Overall scale of the map

- asp:

  Aspect ratio of each map. When `asp` is set to `NA` (default) the
  aspect ratio will be adjusted to the used shapes. When set to 0 the
  aspect ratio is adjusted to the size of the device divided by the
  number of columns and rows.

- bg:

  Draw map background?

- bg.color:

  Background color of the map.

- outer.bg:

  Draw map background (outside the frame)?

- outer.bg.color:

  Background color of map outside the frame.

- frame:

  Draw map frame?

- frame.color:

  The color of the frame.

- frame.alpha:

  The alpha transparency of the frame.

- frame.lwd:

  The line width of the frame. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'lwd'.

- frame.r:

  The r (radius) of the frame.

- frame.double_line:

  The double line of the frame. `TRUE` or `FALSE`.

- outer.margins:

  The margins of the outer space (outside the frame. A vector of 4
  values: bottom, left, top, right. The unit is the device height (for
  bottom and top) or width (for left and right).

- inner.margins:

  The margins of the inner space (inside the frame). A vector of 4
  values: bottom, left, top, right. The unit is the device height (for
  bottom and top) or width (for left and right).

- inner.margins.extra:

  The extra arguments of the margins of the inner space (inside the
  frame). A list of arguments.

- meta.margins:

  The margins of the meta. A vector of 4 values: bottom, left, top,
  right. The unit is the device height (for bottom and top) or width
  (for left and right).

- meta.auto_margins:

  The auto_margins of the meta.

- between_margin:

  Margin between the map.

- panel.margin:

  The margin of the panel.

- xlab.show:

  The visibility of the xlab. `TRUE` or `FALSE`.

- xlab.text:

  The text of the xlab.

- xlab.size:

  The size of the xlab.

- xlab.color:

  The color of the xlab.

- xlab.rotation:

  The rotation of the xlab.

- xlab.space:

  The space of the xlab. In terms of number of text line heights.

- xlab.fontface:

  The font face of the xlab. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- xlab.fontfamily:

  The font family of the xlab. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- xlab.alpha:

  The alpha transparency of the xlab.

- xlab.side:

  The side of the xlab.

- ylab.show:

  The visibility of the ylab. `TRUE` or `FALSE`.

- ylab.text:

  The text of the ylab.

- ylab.size:

  The size of the ylab.

- ylab.color:

  The color of the ylab.

- ylab.rotation:

  The rotation of the ylab.

- ylab.space:

  The space of the ylab. In terms of number of text line heights.

- ylab.fontface:

  The font face of the ylab. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- ylab.fontfamily:

  The font family of the ylab. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- ylab.alpha:

  The alpha transparency of the ylab.

- ylab.side:

  The side of the ylab.

- panel.type:

  The type of the panel.

- panel.wrap.pos:

  The panel positions for wrapped facets created with
  [`tm_facets_grid()`](https://r-tmap.github.io/tmap/reference/tm_facets.md).
  One of `"left"`, `"right"`, `"top"` (default) or `"bottom"`.

- panel.xtab.pos:

  The panel positions for grid facets created with
  [`tm_facets_grid()`](https://r-tmap.github.io/tmap/reference/tm_facets.md).
  Vector of two, where the first determines the locations of row panels
  (`"left"` or `"right"`) and the second the location of column panels (
  `"top"` or \`"bottom")

- unit:

  Unit of the coordinate

- color.sepia_intensity:

  The sepia_intensity of the color.

- color.saturation:

  The saturation of the color.

- color_vision_deficiency_sim:

  Color vision deficiency simulation. Either `"protan"`, `"deutan"`, or
  `"tritan"`.

- text.fontface:

  The font face of the text. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- text.fontfamily:

  The font family of the text. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- r:

  The r (radius) (overall).

- component.position:

  The position of the component. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- component.offset:

  The offset of the component.

- component.stack_margin:

  The stack_margin of the component.

- component.autoscale:

  The autoscale of the component.

- component.resize_as_group:

  The resize_as_group of the component.

- component.frame_combine:

  The frame_combine of the component.

- component.stack:

  The stack of the component.

- legend.stack:

  The stack of the legend.

- chart.stack:

  The stack of the chart.

- component.equalize:

  The equalize of the component.

- component.frame:

  The frame of the component.

- component.frame.color:

  The color of the frame of the component.

- component.frame.alpha:

  The alpha transparency of the frame of the component.

- component.frame.lwd:

  The line width of the frame of the component. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'lwd'.

- component.frame.r:

  The r (radius) of the frame of the component.

- component.bg:

  The bg of the component.

- component.bg.color:

  The color of the bg of the component.

- component.bg.alpha:

  The alpha transparency of the bg of the component.

- legend.show:

  The visibility of the legend. `TRUE` or `FALSE`.

- legend.orientation:

  The orientation of the legend.

- legend.position:

  The position of the legend. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- legend.width:

  The width of the legend.

- legend.height:

  The height of the legend.

- legend.reverse:

  The reverse of the legend.

- legend.na.show:

  The visibility of the na of the legend. `TRUE` or `FALSE`.

- legend.title.color:

  The color of the title of the legend.

- legend.title.size:

  The size of the title of the legend.

- legend.title.fontface:

  The font face of the title of the legend. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- legend.title.fontfamily:

  The font family of the title of the legend. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- legend.title.alpha:

  The alpha transparency of the title of the legend.

- legend.xlab.color:

  The color of the xlab of the legend.

- legend.xlab.size:

  The size of the xlab of the legend.

- legend.xlab.rot:

  The rot of the xlab of the legend.

- legend.xlab.fontface:

  The font face of the xlab of the legend. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- legend.xlab.fontfamily:

  The font family of the xlab of the legend. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- legend.xlab.alpha:

  The alpha transparency of the xlab of the legend.

- legend.ylab.color:

  The color of the ylab of the legend.

- legend.ylab.size:

  The size of the ylab of the legend.

- legend.ylab.rot:

  The rot of the ylab of the legend.

- legend.ylab.fontface:

  The font face of the ylab of the legend. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- legend.ylab.fontfamily:

  The font family of the ylab of the legend. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- legend.ylab.alpha:

  The alpha transparency of the ylab of the legend.

- legend.text.color:

  The color of the text of the legend.

- legend.text.size:

  The size of the text of the legend.

- legend.text.fontface:

  The font face of the text of the legend. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- legend.text.fontfamily:

  The font family of the text of the legend. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- legend.text.alpha:

  The alpha transparency of the text of the legend.

- legend.frame:

  The frame of the legend.

- legend.frame.color:

  The color of the frame of the legend.

- legend.frame.alpha:

  The alpha transparency of the frame of the legend.

- legend.frame.lwd:

  The line width of the frame of the legend. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'lwd'.

- legend.frame.r:

  The r (radius) of the frame of the legend.

- legend.bg:

  The bg of the legend.

- legend.bg.color:

  The color of the bg of the legend.

- legend.bg.alpha:

  The alpha transparency of the bg of the legend.

- legend.only:

  Should only legends be printed (so without map)?

- legend.absolute_fontsize:

  The absolute fontsize of the legend. So far, only used to calculate
  legend dimensions

- legend.settings.portrait:

  The portrait of the settings of the legend.

- legend.settings.landscape:

  The landscape of the settings of the legend.

- add_legend.position:

  The position of the add_legend. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- chart.show:

  The visibility of the chart. `TRUE` or `FALSE`.

- chart.plot.axis.x:

  The x of the axis of the plot of the chart.

- chart.plot.axis.y:

  The y of the axis of the plot of the chart.

- chart.position:

  The position of the chart. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- chart.width:

  The width of the chart.

- chart.height:

  The height of the chart.

- chart.reverse:

  The reverse of the chart.

- chart.na.show:

  The visibility of the na of the chart. `TRUE` or `FALSE`.

- chart.title.color:

  The color of the title of the chart.

- chart.title.size:

  The size of the title of the chart.

- chart.title.fontface:

  The font face of the title of the chart. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- chart.title.fontfamily:

  The font family of the title of the chart. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- chart.title.alpha:

  The alpha transparency of the title of the chart.

- chart.xlab.color:

  The color of the xlab of the chart.

- chart.xlab.size:

  The size of the xlab of the chart.

- chart.xlab.fontface:

  The font face of the xlab of the chart. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- chart.xlab.fontfamily:

  The font family of the xlab of the chart. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- chart.xlab.alpha:

  The alpha transparency of the xlab of the chart.

- chart.ylab.color:

  The color of the ylab of the chart.

- chart.ylab.size:

  The size of the ylab of the chart.

- chart.ylab.fontface:

  The font face of the ylab of the chart. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- chart.ylab.fontfamily:

  The font family of the ylab of the chart. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- chart.ylab.alpha:

  The alpha transparency of the ylab of the chart.

- chart.text.color:

  The color of the text of the chart.

- chart.text.size:

  The size of the text of the chart.

- chart.text.fontface:

  The font face of the text of the chart. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- chart.text.fontfamily:

  The font family of the text of the chart. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- chart.text.alpha:

  The alpha transparency of the text of the chart.

- chart.frame:

  The frame of the chart.

- chart.frame.color:

  The color of the frame of the chart.

- chart.frame.alpha:

  The alpha transparency of the frame of the chart.

- chart.frame.lwd:

  The line width of the frame of the chart. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'lwd'.

- chart.frame.r:

  The r (radius) of the frame of the chart.

- chart.bg:

  The bg of the chart.

- chart.bg.color:

  The color of the bg of the chart.

- chart.bg.alpha:

  The alpha transparency of the bg of the chart.

- chart.object.color:

  The color of the object of the chart.

- title.size:

  The size of the title.

- title.color:

  The color of the title.

- title.fontface:

  The font face of the title. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- title.fontfamily:

  The font family of the title. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- title.alpha:

  The alpha transparency of the title.

- title.padding:

  The padding of the title. A vector of 4 values: bottom, left, top,
  right. The unit is the device height (for bottom and top) or width
  (for left and right).

- title.frame:

  The frame of the title.

- title.frame.color:

  The color of the frame of the title.

- title.frame.alpha:

  The alpha transparency of the frame of the title.

- title.frame.lwd:

  The line width of the frame of the title. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'lwd'.

- title.frame.r:

  The r (radius) of the frame of the title.

- title.position:

  The position of the title. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- title.width:

  The width of the title.

- credits.size:

  The size of the credits.

- credits.color:

  The color of the credits.

- credits.fontface:

  The font face of the credits. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- credits.fontfamily:

  The font family of the credits. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- credits.alpha:

  The alpha transparency of the credits.

- credits.padding:

  The padding of the credits. A vector of 4 values: bottom, left, top,
  right. The unit is the device height (for bottom and top) or width
  (for left and right).

- credits.position:

  The position of the credits. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- credits.width:

  The width of the credits.

- credits.height:

  The height of the credits.

- compass.north:

  The north of the compass.

- compass.type:

  The type of the compass.

- compass.text.size:

  The size of the text of the compass.

- compass.size:

  The size of the compass.

- compass.show.labels:

  The labels of the show of the compass.

- compass.cardinal.directions:

  The directions of the cardinal of the compass.

- compass.text.color:

  The color of the text of the compass.

- compass.color.dark:

  The dark of the color of the compass.

- compass.color.light:

  The light of the color of the compass.

- compass.lwd:

  The line width of the compass. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'lwd'.

- compass.margins:

  The margins of the compass. A vector of 4 values: bottom, left, top,
  right. The unit is the device height (for bottom and top) or width
  (for left and right).

- compass.position:

  The position of the compass. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- inset.position:

  The position of the inset. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- logo.height:

  The height of the logo.

- logo.margins:

  The margins of the logo. A vector of 4 values: bottom, left, top,
  right. The unit is the device height (for bottom and top) or width
  (for left and right).

- logo.between_margin:

  The between_margin of the logo.

- logo.position:

  The position of the logo. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- inset_map.height:

  The height of the inset_map.

- inset_map.width:

  The width of the inset_map.

- inset_map.margins:

  The margins of the inset_map. A vector of 4 values: bottom, left, top,
  right. The unit is the device height (for bottom and top) or width
  (for left and right).

- inset_map.between_margin:

  The between_margin of the inset_map.

- inset_map.position:

  The position of the inset_map. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- inset_map.frame:

  The frame of the inset_map.

- inset.height:

  The height of the inset.

- inset.width:

  The width of the inset.

- inset.margins:

  The margins of the inset. A vector of 4 values: bottom, left, top,
  right. The unit is the device height (for bottom and top) or width
  (for left and right).

- inset.between_margin:

  The between_margin of the inset.

- inset.frame:

  The frame of the inset.

- inset.bg:

  The bg of the inset.

- inset.bg.color:

  The color of the bg of the inset.

- inset.bg.alpha:

  The alpha transparency of the bg of the inset.

- inset_grob.height:

  The height of the inset_grob.

- inset_grob.width:

  The width of the inset_grob.

- inset_gg.height:

  The height of the inset_gg.

- inset_gg.width:

  The width of the inset_gg.

- scalebar.breaks:

  See
  [`tm_scalebar()`](https://r-tmap.github.io/tmap/reference/tm_scalebar.md)

- scalebar.width:

  See
  [`tm_scalebar()`](https://r-tmap.github.io/tmap/reference/tm_scalebar.md)

- scalebar.allow_clipping:

  See
  [`tm_scalebar()`](https://r-tmap.github.io/tmap/reference/tm_scalebar.md)

- scalebar.text.size:

  The size of the text of the scalebar.

- scalebar.text.color:

  The color of the text of the scalebar.

- scalebar.text.fontface:

  The font face of the text of the scalebar. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- scalebar.text.fontfamily:

  The font family of the text of the scalebar. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- scalebar.color.dark:

  The dark of the color of the scalebar.

- scalebar.color.light:

  The light of the color of the scalebar.

- scalebar.lwd:

  The line width of the scalebar. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'lwd'.

- scalebar.size:

  The size of the scalebar.

- scalebar.margins:

  The margins of the scalebar. A vector of 4 values: bottom, left, top,
  right. The unit is the device height (for bottom and top) or width
  (for left and right).

- scalebar.position:

  The position of the scalebar. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- grid.show:

  The visibility of the grid. `TRUE` or `FALSE`.

- grid.labels.pos:

  The pos of the labels of the grid.

- grid.x:

  The x of the grid.

- grid.y:

  The y of the grid.

- grid.n.x:

  The x of the n of the grid.

- grid.n.y:

  The y of the n of the grid.

- grid.crs:

  The coordinate reference system (CRS) of the grid.

- grid.col:

  The color of the grid.

- grid.lwd:

  The line width of the grid. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'lwd'.

- grid.alpha:

  The alpha transparency of the grid.

- grid.labels.show:

  The visibility of the labels of the grid. `TRUE` or `FALSE`.

- grid.labels.size:

  The size of the labels of the grid.

- grid.labels.col:

  The color of the labels of the grid.

- grid.labels.fontface:

  The font face of the labels of the grid. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- grid.labels.fontfamily:

  The font family of the labels of the grid. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- grid.labels.rot:

  The rot of the labels of the grid.

- grid.labels.format:

  The format of the labels of the grid.

- grid.labels.cardinal:

  The cardinal of the labels of the grid.

- grid.labels.margin.x:

  The x of the margin of the labels of the grid.

- grid.labels.margin.y:

  The y of the margin of the labels of the grid.

- grid.labels.space.x:

  The x of the space of the labels of the grid.

- grid.labels.space.y:

  The y of the space of the labels of the grid.

- grid.labels.inside_frame:

  The inside_frame of the labels of the grid.

- grid.ticks:

  The ticks of the grid.

- grid.lines:

  The lines of the grid.

- grid.ndiscr:

  The ndiscr of the grid.

- mouse_coordinates.position:

  The position of the mouse_coordinates. A tm_pos object, or a shortcut
  of two values: horizontal (left, center, right) and vertical (top,
  center, bottom). See tm_pos for details

- minimap.server:

  The server of the minimap.

- minimap.toggle:

  The toggle of the minimap.

- minimap.position:

  The position of the minimap. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- panel.show:

  The visibility of the panel. `TRUE` or `FALSE`.

- panel.labels:

  The labels of the panel.

- panel.label.size:

  The size of the label of the panel.

- panel.label.color:

  The color of the label of the panel.

- panel.label.fontface:

  The font face of the label of the panel. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'font'.

- panel.label.fontfamily:

  The font family of the label of the panel. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option
  'family'.

- panel.label.alpha:

  The alpha transparency of the label of the panel.

- panel.label.bg:

  The bg of the label of the panel.

- panel.label.bg.color:

  The color of the bg of the label of the panel.

- panel.label.bg.alpha:

  The alpha transparency of the bg of the label of the panel.

- panel.label.frame:

  The frame of the label of the panel.

- panel.label.frame.color:

  The color of the frame of the label of the panel.

- panel.label.frame.alpha:

  The alpha transparency of the frame of the label of the panel.

- panel.label.frame.lwd:

  The line width of the frame of the label of the panel. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'lwd'.

- panel.label.frame.r:

  The r (radius) of the frame of the label of the panel.

- panel.label.height:

  The height of the label of the panel.

- panel.label.rot:

  Rotation angles of the panel labels. Vector of four values that
  determine the panel label rotation when they are placed left, top,
  right, and bottom. The default angles are 90, 0, 270 and 0
  respectively. Note that the second value is the most common, since
  labels are by default shown on top (see `panel.wrap.pos`). In
  cross-table facets created with
  [`tm_facets_grid()`](https://r-tmap.github.io/tmap/reference/tm_facets.md),
  the first two values are used by default (see `panel.xtab.pos`).

- qtm.scalebar:

  The scalebar of the qtm.

- qtm.minimap:

  The minimap of the qtm.

- qtm.mouse_coordinates:

  The mouse_coordinates of the qtm.

- earth_boundary:

  The earth boundary

- earth_boundary.color:

  The color of the earth_boundary.

- earth_boundary.lwd:

  The line width of the earth_boundary. See
  [`graphics::par`](https://rdrr.io/r/graphics/par.html), option 'lwd'.

- earth_datum:

  Earth datum

- space:

  Should the space be drawn? Only applicable is earth_boundary is
  enabled.

- space.color:

  The color of the space.

- space_overlay:

  Should the space be drawn as overlay (to make sure spatial features or
  rasters do not exceed the earth boundary), or as background? By
  default `TRUE` when a raster is warped.

- check_and_fix:

  Should attempt to fix an invalid shapefile

- basemap.show:

  The visibility of the basemap. `TRUE` or `FALSE`.

- basemap.server:

  The server of the basemap.

- basemap.alpha:

  The alpha transparency of the basemap.

- basemap.zoom:

  The zoom of the basemap.

- tiles.show:

  The visibility of the tiles. `TRUE` or `FALSE`.

- tiles.server:

  The server of the tiles.

- tiles.alpha:

  The alpha transparency of the tiles.

- tiles.zoom:

  The zoom of the tiles.

- attr.color:

  The color of the attr.

- crs_extra:

  Only used internally (work in progress)

- crs_global:

  The used crs for world maps

- crs_basemap:

  The crs_basemap (overall).

- use_gradient:

  Use gradient fill using
  [linearGradient()](https://rdrr.io/r/grid/patterns.html)

- use_browser:

  If `TRUE` it opens an external browser, and `FALSE` (default) it opens
  the internal IDEs (e.g. RStudio) browser.

- use_WebGL:

  use webGL for points, lines, and polygons. For large spatial objects,
  this is much faster than the standard leaflet layer functions.
  However, it can not always be used for two reasons. First, the number
  of visual variables are limited; only fill, size, and color (for
  lines) are supported. Second, projected CRS's are not supported.
  Furthermore, it has the drawback that polygon borders are not as
  sharp. By default only `TRUE` for large spatial objects (1000 or more
  features) when the mentioned criteria are met. By default `TRUE` if no
  other visual variables are used.

- control.position:

  The position of the control. A tm_pos object, or a shortcut of two
  values: horizontal (left, center, right) and vertical (top, center,
  bottom). See tm_pos for details

- control.bases:

  base layers

- control.overlays:

  overlay layers

- control.collapse:

  Should the box be collapsed or expanded?

- set_bounds:

  logical that determines whether maximum bounds are set, or a bounding
  box. Not applicable in plot mode. In view mode, this is passed on to
  setMaxBounds()

- set_view:

  numeric vector that determines the view. Either a vector of three:
  `lng`, `lat`, and `zoom`, or a single value: `zoom`. See setView().
  Only applicable if `bbox` is not specified

- set_zoom_limits:

  numeric vector of two that set the minimum and maximum zoom levels
  (see tileOptions()).

- use_circle_markers:

  If `TRUE` (default) circle shaped symbols (e.g.
  [`tm_dots()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md)
  and
  [`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md))
  will be rendered as addCircleMarkers() instead of addMarkers(). The
  former is faster, the latter can support any symbol since it is based
  on icons

- leaflet.options:

  options passed on to
  [leafletOptions()](https://rstudio.github.io/leaflet/reference/leaflet.html)

- title:

  deprecated See
  [`tm_title()`](https://r-tmap.github.io/tmap/reference/tm_title.md)

- main.title:

  deprecated See
  [`tm_title()`](https://r-tmap.github.io/tmap/reference/tm_title.md)

- main.title.size, main.title.color, main.title.fontface,
  main.title.fontfamily, main.title.position:

  deprecated. Use the `title.` options instead.

- fontface, fontfamily:

  renamed to `text.fontface` and `text.fontfamily`

- mode:

  mode, e.g. `"plot"` or `"view"`

- style:

  style, see
  [`tmap_style()`](https://r-tmap.github.io/tmap/reference/tmap_style.md)
  for available styles

- mode.specific:

  Should only mode-specific options be returned? `TRUE` by default.

- default.options:

  return the default options or the current options?

## Examples

``` r
# get all options
opt = tmap_options()

# print as a tree
if (requireNamespace("lobstr")) {
  lobstr::tree(opt)
}
#> Loading required namespace: lobstr
#> <list>
#> ├─modes: <list>
#> │ ├─plot: <list>
#> │ │ ├─name: "Grid"
#> │ │ ├─use_gradient: FALSE
#> │ │ ├─crs_basemap: 3857
#> │ │ └─limit_latitude_3857<dbl [2]>: -84, 84
#> │ └─view: <list>
#> │   ├─name: "Leaflet"
#> │   ├─use_browser: FALSE
#> │   ├─use_WebGL: NA
#> │   ├─legend.position: S3<tm_pos>
#> │   │ ├─pos.h: "right"
#> │   │ ├─pos.v: "bottom"
#> │   │ ├─align.h: "left"
#> │   │ ├─align.v: "top"
#> │   │ ├─just.h: "left"
#> │   │ ├─just.v: "top"
#> │   │ ├─called<chr [6]>: "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │   │ └─type: "in"
#> │   ├─crs_basemap: <list>
#> │   │ ├─dimensions: 3857
#> │   │ └─4326
#> │   ├─facet.max: 16
#> │   ├─control.position<chr [2]>: "left", "top"
#> │   ├─control.collapse: TRUE
#> │   ├─basemap.show: TRUE
#> │   ├─set_bounds: FALSE
#> │   ├─set_view: NA
#> │   ├─set_zoom_limits: NA
#> │   ├─qtm.scalebar: TRUE
#> │   ├─qtm.minimap: FALSE
#> │   ├─qtm.mouse_coordinates: TRUE
#> │   ├─use_circle_markers: TRUE
#> │   └─leaflet.options: <list>
#> ├─crs: NA
#> ├─bbox: <NULL>
#> ├─facet.max: 64
#> ├─free.scales: <NULL>
#> ├─raster.max_cells: 1e+07
#> ├─raster.warp: TRUE
#> ├─show.messages: TRUE
#> ├─show.warnings: TRUE
#> ├─output.format: "png"
#> ├─output.size: 49
#> ├─output.dpi: 300
#> ├─animation.dpi: 100
#> ├─value.const: <list>
#> │ ├─fill.symbols: "grey60"
#> │ ├─fill.dots: "black"
#> │ ├─fill: "grey85"
#> │ ├─col.polygons: "grey25"
#> │ ├─col.symbols: "grey25"
#> │ ├─col.raster: "grey40"
#> │ ├─col.text: "black"
#> │ ├─col: "black"
#> │ ├─bgcol.labels_highlighted: "white"
#> │ ├─bgcol: "#00000000"
#> │ ├─lwd: 1
#> │ ├─lty: "solid"
#> │ ├─text: "Abc"
#> │ ├─text.labels: ""
#> │ ├─fontface: <NULL>
#> │ ├─shape.squares: 22
#> │ ├─shape.dots: 19
#> │ ├─shape.markers: S3<tmap_icons>
#> │ │ ├─iconUrl: "/home/runner/work/_temp/Library/..."
#> │ │ ├─iconWidth: 25
#> │ │ ├─iconHeight: 41
#> │ │ ├─iconAnchorX: 12
#> │ │ └─iconAnchorY: 41
#> │ ├─shape: 21
#> │ ├─size.bubbles: 1.3333
#> │ ├─size.squares: 1.3333
#> │ ├─size.dots: 0.3
#> │ ├─size: 1
#> │ ├─fill_alpha: 1
#> │ ├─col_alpha: 1
#> │ ├─bgcol_alpha: 1
#> │ ├─angle: 0
#> │ └─num: 1
#> ├─value.na: <list>
#> │ ├─fill: "grey75"
#> │ ├─col: "grey75"
#> │ ├─col.raster: "#00000000"
#> │ ├─bgcol: "grey75"
#> │ ├─lty: "solid"
#> │ ├─text: "Unknown"
#> │ ├─fontface: <NULL>
#> │ ├─fill_alpha: 1
#> │ ├─col_alpha: 1
#> │ ├─bgcol_alpha: 1
#> │ ├─col_alpha.raster: 0
#> │ └─angle: 0
#> ├─value.null: <list>
#> │ ├─fill: "grey95"
#> │ ├─col: "grey95"
#> │ ├─col.polygons: "grey40"
#> │ ├─bgcol: "grey95"
#> │ ├─lty: "solid"
#> │ ├─lwd: 0.2
#> │ ├─shape: 20
#> │ ├─text: ""
#> │ ├─fontface: <NULL>
#> │ ├─fill_alpha: 1
#> │ ├─col_alpha: 1
#> │ ├─bgcol_alpha: 1
#> │ ├─size: 0.2
#> │ ├─angle: 0
#> │ └─num: 0
#> ├─value.blank: <list>
#> │ ├─fill: "#00000000"
#> │ ├─col: "#00000000"
#> │ ├─bgcol: "#00000000"
#> │ ├─lty: "blank"
#> │ ├─lwd: NA
#> │ ├─text: ""
#> │ ├─fontface: <NULL>
#> │ ├─fill_alpha: 0
#> │ ├─col_alpha: 0
#> │ ├─bgcol_alpha: 0
#> │ ├─angle: 0
#> │ └─num: 0
#> ├─values.var: <list>
#> │ ├─fill: <list>
#> │ │ ├─seq: "-hcl.blues3"
#> │ │ ├─div: "pu_gn_div"
#> │ │ ├─unord: "cols4all.area7"
#> │ │ ├─ord: "-hcl.blues3"
#> │ │ ├─cyc: "tol.rainbow_pu_rd"
#> │ │ └─biv: "pu_gn_bivs"
#> │ ├─col: <list>
#> │ │ ├─seq: "-hcl.blues3"
#> │ │ ├─div: "pu_gn_div"
#> │ │ ├─unord: "cols4all.line7"
#> │ │ ├─ord: "-hcl.blues3"
#> │ │ ├─cyc: "tol.rainbow_pu_rd"
#> │ │ └─biv: "pu_gn_bivs"
#> │ ├─fill.dots: <list>
#> │ │ ├─seq: "-hcl.blues3"
#> │ │ ├─div: "pu_gn_div"
#> │ │ ├─unord: "cols4all.line7"
#> │ │ ├─ord: "-hcl.blues3"
#> │ │ ├─cyc: "tol.rainbow_pu_rd"
#> │ │ └─biv: "pu_gn_bivs"
#> │ ├─bgcol: <list>
#> │ │ ├─seq: "-hcl.blues3"
#> │ │ ├─div: "pu_gn_div"
#> │ │ ├─unord: "cols4all.area7"
#> │ │ ├─ord: "-hcl.blues3"
#> │ │ ├─cyc: "tol.rainbow_pu_rd"
#> │ │ └─biv: "pu_gn_bivs"
#> │ ├─size: S3<tmapSeq>
#> │ │ ├─from: 0
#> │ │ ├─to: 1
#> │ │ └─power: "sqrt"
#> │ ├─size.bubbles: S3<tmapSeq>
#> │ │ ├─from: 0
#> │ │ ├─to: 1
#> │ │ └─power: "sqrt"
#> │ ├─lwd<dbl [2]>: 0, 3
#> │ ├─lty<chr [5]>: "dashed", "dotted", "dotdash", "longdash", "twodash"
#> │ ├─text<chr [26]>: "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", ...
#> │ ├─fontface<chr [3]>: "plain", "italic", "bold"
#> │ ├─fill_alpha<dbl [2]>: 0.25, 1
#> │ ├─col_alpha<dbl [2]>: 0.25, 1
#> │ ├─bgcol_alpha<dbl [2]>: 0.25, 1
#> │ ├─shape<int [5]>: 21, 22, 23, 24, 25
#> │ ├─area<dbl [2]>: 0, 1
#> │ ├─xmod<dbl [2]>: 0, 1
#> │ ├─ymod<dbl [2]>: 0, 1
#> │ ├─angle<dbl [2]>: 0, 360
#> │ └─num<dbl [2]>: 0, 1
#> ├─values.range: <list>
#> │ ├─fill: NA
#> │ ├─col: NA
#> │ ├─size<dbl [2]>: 0, 1
#> │ ├─lwd<dbl [2]>: 0, 1
#> │ ├─lty: NA
#> │ ├─text: NA
#> │ ├─fontface: NA
#> │ ├─fill_alpha: NA
#> │ ├─col_alpha: NA
#> │ ├─shape: NA
#> │ ├─angle: NA
#> │ └─num<dbl [2]>: 0, 1
#> ├─value.neutral: <list>
#> │ ├─size: 0.75
#> │ ├─lwd: 2
#> │ ├─lty: "solid"
#> │ ├─fill_alpha: 1
#> │ ├─col_alpha: 1
#> │ ├─bgcol_alpha: 1
#> │ ├─text: "Abc"
#> │ ├─fontface: "plain"
#> │ ├─angle: 0
#> │ └─num: 1
#> ├─values.scale: <list>
#> │ ├─1
#> │ ├─lwd.lines: 1
#> │ ├─size.symbols: 1
#> │ ├─size.bubbles: 1.3333
#> │ └─size.squares: 1.3333
#> ├─scales.var: <list>
#> │ ├─fill: <list>
#> │ │ ├─fact: "categorical"
#> │ │ ├─num: "intervals"
#> │ │ ├─int: "discrete"
#> │ │ ├─datetime: "intervals"
#> │ │ └─date: "intervals"
#> │ ├─col: <list>
#> │ │ ├─fact: "categorical"
#> │ │ ├─num: "intervals"
#> │ │ ├─int: "discrete"
#> │ │ ├─datetime: "intervals"
#> │ │ └─date: "intervals"
#> │ ├─bgcol: <list>
#> │ │ ├─fact: "categorical"
#> │ │ ├─num: "intervals"
#> │ │ ├─int: "discrete"
#> │ │ ├─datetime: "intervals"
#> │ │ └─date: "intervals"
#> │ ├─lwd: <list>
#> │ │ ├─fact: "categorical"
#> │ │ ├─num: "continuous"
#> │ │ ├─int: "discrete"
#> │ │ ├─datetime: "intervals"
#> │ │ └─date: "intervals"
#> │ ├─lty: <list>
#> │ │ ├─fact: "categorical"
#> │ │ ├─num: "intervals"
#> │ │ ├─datetime: "intervals"
#> │ │ └─date: "intervals"
#> │ ├─shape: <list>
#> │ │ ├─fact: "categorical"
#> │ │ ├─num: "intervals"
#> │ │ ├─datetime: "intervals"
#> │ │ └─date: "intervals"
#> │ ├─size: <list>
#> │ │ ├─fact: "continuous"
#> │ │ ├─num: "continuous"
#> │ │ ├─datetime: "continuous"
#> │ │ └─date: "continuous"
#> │ ├─fill_alpha: <list>
#> │ │ ├─fact: "categorical"
#> │ │ ├─num: "intervals"
#> │ │ ├─datetime: "continuous"
#> │ │ └─date: "continuous"
#> │ ├─col_alpha: <list>
#> │ │ ├─fact: "categorical"
#> │ │ ├─num: "intervals"
#> │ │ ├─datetime: "continuous"
#> │ │ └─date: "continuous"
#> │ ├─bgcol_alpha: <list>
#> │ │ ├─fact: "categorical"
#> │ │ ├─num: "intervals"
#> │ │ ├─datetime: "continuous"
#> │ │ └─date: "continuous"
#> │ ├─area: <list>
#> │ │ ├─fact: "categorical"
#> │ │ ├─num: "continuous"
#> │ │ ├─datetime: "continuous"
#> │ │ └─date: "continuous"
#> │ ├─xmod: <list>
#> │ │ ├─fact: "asis"
#> │ │ └─num: "asis"
#> │ ├─ymod: <list>
#> │ │ ├─fact: "asis"
#> │ │ └─num: "asis"
#> │ ├─angle: <list>
#> │ │ ├─fact: "asis"
#> │ │ └─num: "asis"
#> │ ├─text: <list>
#> │ │ ├─fact: "asis"
#> │ │ └─num: "asis"
#> │ └─fontface: <list>
#> │   ├─fact: "categorical"
#> │   ├─num: "categorical"
#> │   ├─datetime: "categorical"
#> │   └─date: "categorical"
#> ├─scale.misc.args: <list>
#> │ ├─continuous: <list>
#> │ │ ├─n<dbl [3]>: 5, 5, 5
#> │ │ ├─outliers.trunc<lgl [2]>: FALSE, FALSE
#> │ │ ├─trans: "identity"
#> │ │ └─limits: <list>
#> │ │   ├─fill: NA
#> │ │   ├─col: NA
#> │ │   └─0
#> │ ├─continuous_log: <list>
#> │ │ ├─n<dbl [3]>: 5, 5, 5
#> │ │ ├─outliers.trunc<lgl [2]>: FALSE, FALSE
#> │ │ └─limits: <list>
#> │ │   ├─fill: NA
#> │ │   ├─col: NA
#> │ │   └─NA
#> │ ├─continuous_log2: <list>
#> │ │ ├─n<dbl [3]>: 5, 5, 5
#> │ │ ├─outliers.trunc<lgl [2]>: FALSE, FALSE
#> │ │ └─limits: <list>
#> │ │   ├─fill: NA
#> │ │   ├─col: NA
#> │ │   └─NA
#> │ ├─continuous_log10: <list>
#> │ │ ├─n<dbl [3]>: 5, 5, 5
#> │ │ ├─outliers.trunc<lgl [2]>: FALSE, FALSE
#> │ │ └─limits: <list>
#> │ │   ├─fill: NA
#> │ │   ├─col: NA
#> │ │   └─NA
#> │ ├─continuous_log1p: <list>
#> │ │ ├─n<dbl [3]>: 5, 5, 5
#> │ │ ├─outliers.trunc<lgl [2]>: FALSE, FALSE
#> │ │ └─limits: <list>
#> │ │   ├─fill: NA
#> │ │   ├─col: NA
#> │ │   └─NA
#> │ ├─continuous_sqrt: <list>
#> │ │ ├─n<dbl [3]>: 5, 5, 5
#> │ │ ├─outliers.trunc<lgl [2]>: FALSE, FALSE
#> │ │ └─limits: <list>
#> │ │   ├─fill: NA
#> │ │   ├─col: NA
#> │ │   └─0
#> │ ├─continuous_pseudo_log: <list>
#> │ │ ├─n<dbl [3]>: 5, 5, 5
#> │ │ ├─outliers.trunc<lgl [2]>: FALSE, FALSE
#> │ │ └─limits: <list>
#> │ │   ├─fill: NA
#> │ │   ├─col: NA
#> │ │   └─NA
#> │ └─rank: <list>
#> │   ├─n: 5
#> │   └─unit: "rank"
#> ├─continuous.nclass_per_legend_break: 50
#> ├─continuous.nclasses: 101
#> ├─label.format: <list>
#> │ ├─fun: <NULL>
#> │ ├─scientific: FALSE
#> │ ├─digits: NA
#> │ ├─interval.disjoint: TRUE
#> │ ├─big.num.abbr<dbl [5]>: 6, 9, 12, 15, 18
#> │ ├─prefix: ""
#> │ ├─suffix: ""
#> │ ├─text.separator: "-"
#> │ ├─text.less.than: "<"
#> │ ├─text.less.than_as.prefix: TRUE
#> │ ├─text.or.more: "≥"
#> │ ├─text.or.more_as.prefix: TRUE
#> │ ├─text.align: NA
#> │ ├─text.to.columns: FALSE
#> │ └─html.escape: TRUE
#> ├─label.na: "Missing"
#> ├─scale: 1
#> ├─asp: NA
#> ├─bg: TRUE
#> ├─bg.color: "white"
#> ├─outer.bg: FALSE
#> ├─outer.bg.color: "white"
#> ├─frame: TRUE
#> ├─frame.color: NA
#> ├─frame.alpha: 1
#> ├─frame.lwd: 1
#> ├─frame.r: NA
#> ├─frame.double_line: FALSE
#> ├─outer.margins<dbl [4]>: 0.02, 0.02, 0.02, 0.02
#> ├─inner.margins: <list>
#> │ ├─stars<dbl [4]>: 0, 0, 0, 0
#> │ ├─SpatRaster<dbl [4]>: 0, 0, 0, 0
#> │ └─<dbl [4]>0.02, 0.02, 0.02, 0.02
#> ├─inner.margins.extra<dbl [4]>: 0, 0, 0, 0
#> ├─meta.margins: NA
#> ├─meta.auto_margins<dbl [4]>: 0.4, 0.4, 0.4, 0.4
#> ├─between_margin: 0.5
#> ├─panel.margin<dbl [2]>: 0.4, 0
#> ├─xlab.show: FALSE
#> ├─xlab.text: ""
#> ├─xlab.size: 1
#> ├─xlab.color: "black"
#> ├─xlab.rotation: 0
#> ├─xlab.space: 0.5
#> ├─xlab.fontface: <NULL>
#> ├─xlab.fontfamily: <NULL>
#> ├─xlab.alpha: 1
#> ├─xlab.side: "bottom"
#> ├─ylab.show: FALSE
#> ├─ylab.text: ""
#> ├─ylab.size: 1
#> ├─ylab.color: "black"
#> ├─ylab.rotation: 0
#> ├─ylab.space: 0.5
#> ├─ylab.fontface: <NULL>
#> ├─ylab.fontfamily: <NULL>
#> ├─ylab.alpha: 1
#> ├─ylab.side: "left"
#> ├─panel.type: NA
#> ├─panel.wrap.pos: "top"
#> ├─panel.xtab.pos<chr [2]>: "left", "top"
#> ├─unit: "metric"
#> ├─color.sepia_intensity: 0
#> ├─color.saturation: 1
#> ├─color_vision_deficiency_sim: "none"
#> ├─text.fontface: "plain"
#> ├─text.fontfamily: ""
#> ├─r: 2
#> ├─component.position: <list>
#> │ ├─in: S3<tm_pos>
#> │ │ ├─pos.h: "left"
#> │ │ ├─pos.v: "top"
#> │ │ ├─align.h: "left"
#> │ │ ├─align.v: "top"
#> │ │ ├─just.h: "left"
#> │ │ ├─just.v: "top"
#> │ │ ├─called<chr [6]>: "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ │ └─type: "in"
#> │ └─out: S3<tm_pos>
#> │   ├─cell.h: "right"
#> │   ├─cell.v: "center"
#> │   ├─pos.h: "left"
#> │   ├─pos.v: "top"
#> │   ├─align.h: "left"
#> │   ├─align.v: "top"
#> │   ├─just.h: "left"
#> │   ├─just.v: "top"
#> │   ├─called<chr [8]>: "cell.h", "cell.v", "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │   └─type: "out"
#> ├─component.offset<dbl [4]>: 0.75, 0, 0, 0
#> ├─component.stack_margin<dbl [2]>: 0, 0.5
#> ├─component.autoscale: TRUE
#> ├─component.resize_as_group: FALSE
#> ├─component.frame_combine: TRUE
#> ├─component.stack: "vertical"
#> ├─legend.stack<chr [6]>: "vertical", "horizontal", "horizontal", "vertical", "horizontal", "vertical"
#> ├─chart.stack<chr [6]>: "vertical", "horizontal", "horizontal", "vertical", "horizontal", "vertical"
#> ├─component.equalize: TRUE
#> ├─component.frame: FALSE
#> ├─component.frame.color: NA
#> ├─component.frame.alpha: 1
#> ├─component.frame.lwd: 1
#> ├─component.frame.r: NA
#> ├─component.bg: TRUE
#> ├─component.bg.color: "white"
#> ├─component.bg.alpha: 1
#> ├─legend.show: TRUE
#> ├─legend.orientation: "portrait"
#> ├─legend.position: S3<tm_pos>
#> │ ├─cell.h: "right"
#> │ ├─cell.v: "bottom"
#> │ ├─pos.h: "left"
#> │ ├─pos.v: "top"
#> │ ├─align.h: "left"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [8]>: "cell.h", "cell.v", "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "autoout"
#> ├─legend.width: NA
#> ├─legend.height: NA
#> ├─legend.reverse: FALSE
#> ├─legend.na.show: NA
#> ├─legend.title.color: NA
#> ├─legend.title.size: 0.9
#> ├─legend.title.fontface: <NULL>
#> ├─legend.title.fontfamily: <NULL>
#> ├─legend.title.alpha: 1
#> ├─legend.xlab.color: NA
#> ├─legend.xlab.size: 0.9
#> ├─legend.xlab.fontface: <NULL>
#> ├─legend.xlab.fontfamily: <NULL>
#> ├─legend.xlab.alpha: 1
#> ├─legend.ylab.color: NA
#> ├─legend.ylab.size: 0.9
#> ├─legend.ylab.fontface: <NULL>
#> ├─legend.ylab.fontfamily: <NULL>
#> ├─legend.xlab.rot: 0
#> ├─legend.ylab.rot: 0
#> ├─legend.ylab.alpha: 1
#> ├─legend.text.color: NA
#> ├─legend.text.size: 0.7
#> ├─legend.text.fontface: <NULL>
#> ├─legend.text.fontfamily: <NULL>
#> ├─legend.text.alpha: 1
#> ├─legend.frame: TRUE
#> ├─legend.frame.color: NA
#> ├─legend.frame.alpha: 1
#> ├─legend.frame.lwd: 1
#> ├─legend.frame.r: NA
#> ├─legend.bg: TRUE
#> ├─legend.bg.color: "white"
#> ├─legend.bg.alpha: 1
#> ├─legend.only: FALSE
#> ├─legend.absolute_fontsize: 14
#> ├─legend.settings.portrait: <list>
#> │ ├─item.height<dbl [6]>: 1.2, 1, 3, 1.2, 1.2, 1.2
#> │ ├─item.width<dbl [6]>: 1.2, 1, 1.2, 1.2, 3, 1.2
#> │ ├─item.r: NA
#> │ ├─item.space<dbl [6]>: 0.2, 0.2, 0, 0.2, 0.2, 0
#> │ ├─item.na.height<dbl [6]>: NA, NA, 1.2, NA, NA, NA
#> │ ├─item.na.width<dbl [6]>: NA, NA, 1.2, NA, NA, NA
#> │ ├─item.na.space<dbl [6]>: 0.2, 0.3, 1, 0.2, 0.2, 0.2
#> │ ├─item.shape: 107
#> │ ├─title.padding<dbl [4]>: 0, 0, 0.25, 0
#> │ ├─xlab.padding<dbl [4]>: 0, 0, 0.25, 0
#> │ ├─ylab.padding<dbl [4]>: 0, 0, 0.25, 0
#> │ ├─title.align<chr [2]>: "right", "left"
#> │ ├─xlab.align: "left"
#> │ ├─ylab.align: "center"
#> │ ├─ticks: <list>
#> │ │ ├─rect: <list>
#> │ │ ├─symbols: <list>
#> │ │ ├─gradient: <list>
#> │ │ │ └─<dbl [2]>0.85, 1.1
#> │ │ ├─lines: <list>
#> │ │ ├─text: <list>
#> │ │ └─bivariate: <list>
#> │ ├─ticks.disable.na<lgl [6]>: FALSE, FALSE, TRUE, FALSE, FALSE, TRUE
#> │ ├─ticks.col: NA
#> │ ├─ticks.lwd: 1
#> │ ├─margins<dbl [4]>: 0.4, 0.4, 0.4, 0.4
#> │ └─item_text.margin<dbl [2]>: 0.5, 0.25
#> ├─legend.settings.landscape: <list>
#> │ ├─item.height<dbl [5]>: 1, 1, 1.2, 1, 1
#> │ ├─item.width<dbl [5]>: 6, 3, 6, 6, 6
#> │ ├─item.r: NA
#> │ ├─item.space<dbl [5]>: 0.2, 0.3, 0, 0.2, 0.2
#> │ ├─item.na.height<dbl [5]>: NA, NA, 2, NA, NA
#> │ ├─item.na.width<dbl [5]>: NA, NA, 4, NA, NA
#> │ ├─item.na.space<dbl [5]>: 0.2, 0.3, 0.3, 0.2, 0.2
#> │ ├─item.shape: 107
#> │ ├─title.padding<dbl [4]>: 0, 0, 0.25, 0
#> │ ├─xlab.padding<dbl [4]>: 0, 0, 0.25, 0
#> │ ├─ylab.padding<dbl [4]>: 0, 0, 0.25, 0
#> │ ├─title.align<chr [2]>: "right", "left"
#> │ ├─xlab.align: "left"
#> │ ├─ylab.align: "center"
#> │ ├─ticks: <list>
#> │ │ ├─rect: <list>
#> │ │ ├─symbols: <list>
#> │ │ ├─gradient: <list>
#> │ │ │ └─<dbl [2]>0.85, 1.1
#> │ │ ├─lines: <list>
#> │ │ └─text: <list>
#> │ ├─ticks.disable.na<lgl [5]>: FALSE, FALSE, TRUE, FALSE, FALSE
#> │ ├─ticks.col: NA
#> │ ├─ticks.lwd: 1
#> │ ├─margins<dbl [4]>: 0.4, 0.4, 0.4, 0.4
#> │ └─item_text.margin: 0.25
#> ├─add_legend.position: <NULL>
#> ├─chart.show: TRUE
#> ├─chart.plot.axis.x: FALSE
#> ├─chart.plot.axis.y: TRUE
#> ├─chart.position: S3<tm_pos>
#> │ ├─cell.h: "right"
#> │ ├─cell.v: "bottom"
#> │ ├─pos.h: "left"
#> │ ├─pos.v: "top"
#> │ ├─align.h: "left"
#> │ ├─align.v: "bottom"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [8]>: "cell.h", "cell.v", "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "autoout"
#> ├─chart.width<dbl [8]>: 10, 20, 10, 20, 10, 10, 10, 15
#> ├─chart.height<dbl [8]>: 10, 10, 10, 10, 10, 10, 10, 10
#> ├─chart.reverse: FALSE
#> ├─chart.na.show: NA
#> ├─chart.title.color: NA
#> ├─chart.title.size: 0.9
#> ├─chart.title.fontface: <NULL>
#> ├─chart.title.fontfamily: <NULL>
#> ├─chart.title.alpha: <NULL>
#> ├─chart.xlab.color: NA
#> ├─chart.xlab.size: 0.9
#> ├─chart.xlab.fontface: <NULL>
#> ├─chart.xlab.fontfamily: <NULL>
#> ├─chart.xlab.alpha: <NULL>
#> ├─chart.ylab.color: NA
#> ├─chart.ylab.size: 0.9
#> ├─chart.ylab.fontface: <NULL>
#> ├─chart.ylab.fontfamily: <NULL>
#> ├─chart.ylab.alpha: 1
#> ├─chart.text.color: NA
#> ├─chart.text.size: 0.7
#> ├─chart.text.fontface: <NULL>
#> ├─chart.text.fontfamily: <NULL>
#> ├─chart.text.alpha: 1
#> ├─chart.frame: TRUE
#> ├─chart.frame.color: NA
#> ├─chart.frame.alpha: 1
#> ├─chart.frame.lwd: 1
#> ├─chart.frame.r: NA
#> ├─chart.bg: TRUE
#> ├─chart.bg.color: "white"
#> ├─chart.bg.alpha: 1
#> ├─chart.object.color: "#DDDDDD"
#> ├─title.size: 1.3
#> ├─title.color: NA
#> ├─title.fontface: <NULL>
#> ├─title.fontfamily: <NULL>
#> ├─title.alpha: 1
#> ├─title.padding<dbl [4]>: 0.25, 0.25, 0.25, 0.25
#> ├─title.frame: FALSE
#> ├─title.frame.color: NA
#> ├─title.frame.alpha: 1
#> ├─title.frame.lwd: 1
#> ├─title.frame.r: NA
#> ├─title.position: S3<tm_pos>
#> │ ├─cell.h: "center"
#> │ ├─cell.v: "top"
#> │ ├─pos.h: "left"
#> │ ├─pos.v: "top"
#> │ ├─align.h: "left"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [8]>: "cell.h", "cell.v", "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "out"
#> ├─title.width: NA
#> ├─credits.size: 0.7
#> ├─credits.color: NA
#> ├─credits.fontface: <NULL>
#> ├─credits.fontfamily: <NULL>
#> ├─credits.alpha: 1
#> ├─credits.padding<dbl [4]>: 0.25, 0.25, 0.25, 0.25
#> ├─credits.position: S3<tm_pos>
#> │ ├─pos.h: "right"
#> │ ├─pos.v: "bottom"
#> │ ├─align.h: "right"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [6]>: "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "in"
#> ├─credits.width: NA
#> ├─credits.height: NA
#> ├─compass.north: 0
#> ├─compass.type: "arrow"
#> ├─compass.text.size: 0.8
#> ├─compass.size: NA
#> ├─compass.show.labels: 1
#> ├─compass.cardinal.directions<chr [4]>: "N", "E", "S", "W"
#> ├─compass.text.color: NA
#> ├─compass.color.dark: NA
#> ├─compass.color.light: NA
#> ├─compass.lwd: 1
#> ├─compass.margins<dbl [4]>: 0.25, 0.25, 0.25, 0.25
#> ├─compass.position: S3<tm_pos>
#> │ ├─pos.h: "right"
#> │ ├─pos.v: "bottom"
#> │ ├─align.h: "right"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [6]>: "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "in"
#> ├─inset.position: S3<tm_pos>
#> │ ├─pos.h: "right"
#> │ ├─pos.v: "bottom"
#> │ ├─align.h: "right"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [6]>: "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "in"
#> ├─logo.height: 3
#> ├─logo.margins<dbl [4]>: 0.2, 0.2, 0.2, 0.2
#> ├─logo.between_margin: 0.2
#> ├─logo.position: S3<tm_pos>
#> │ ├─pos.h: "right"
#> │ ├─pos.v: "bottom"
#> │ ├─align.h: "right"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [6]>: "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "in"
#> ├─inset.height: 3
#> ├─inset.width: 3
#> ├─inset.margins<dbl [4]>: 0.2, 0.2, 0.2, 0.2
#> ├─inset.between_margin: 0.2
#> ├─inset.position: S3<tm_pos>
#> │ ├─pos.h: "right"
#> │ ├─pos.v: "bottom"
#> │ ├─align.h: "right"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [6]>: "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "in"
#> ├─inset.frame: TRUE
#> ├─inset.bg: TRUE
#> ├─inset.bg.color: "#ffffff"
#> ├─inset.bg.alpha: 1
#> ├─inset.box_frame: TRUE
#> ├─inset.box_frame.color: "#ee2211"
#> ├─inset.box_frame.alpha: 1
#> ├─inset.box_frame.lwd: 2
#> ├─inset.box_frame.lty: "solid"
#> ├─inset.box_bg: FALSE
#> ├─inset.box_bg.color: "#ffffff"
#> ├─inset.box_bg.alpha: 1
#> ├─inset.main_frame: TRUE
#> ├─inset.main_frame.r: NA
#> ├─inset.main_frame.color: NA
#> ├─inset.main_frame.alpha: 1
#> ├─inset.main_frame.lwd: 2
#> ├─inset_map.height: 7
#> ├─inset_map.width: 7
#> ├─inset_map.margins<dbl [4]>: 0.2, 0.2, 0.2, 0.2
#> ├─inset_map.between_margin: 0.2
#> ├─inset_map.position: S3<tm_pos>
#> │ ├─pos.h: "right"
#> │ ├─pos.v: "bottom"
#> │ ├─align.h: "right"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [6]>: "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "in"
#> ├─inset_map.frame: FALSE
#> ├─inset_tmap.height: 7
#> ├─inset_tmap.width: 7
#> ├─inset_grob.height: 7
#> ├─inset_grob.width: 7
#> ├─inset_gg.height: 7
#> ├─inset_gg.width: 7
#> ├─scalebar.breaks: <NULL>
#> ├─scalebar.width: 12
#> ├─scalebar.allow_clipping: FALSE
#> ├─scalebar.text.size: 0.5
#> ├─scalebar.text.color: NA
#> ├─scalebar.text.fontface: <NULL>
#> ├─scalebar.text.fontfamily: <NULL>
#> ├─scalebar.color.dark: NA
#> ├─scalebar.color.light: NA
#> ├─scalebar.lwd: 1
#> ├─scalebar.size: <NULL>
#> ├─scalebar.margins<dbl [4]>: 0.01, 0.01, 0.01, 0.01
#> ├─scalebar.position: S3<tm_pos>
#> │ ├─pos.h: "right"
#> │ ├─pos.v: "bottom"
#> │ ├─align.h: "right"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [6]>: "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "in"
#> ├─grid.show: FALSE
#> ├─grid.labels.pos<chr [2]>: "left", "bottom"
#> ├─grid.x: NA
#> ├─grid.y: NA
#> ├─grid.n.x: NA
#> ├─grid.n.y: NA
#> ├─grid.crs: NA
#> ├─grid.col: NA
#> ├─grid.lwd: 1
#> ├─grid.alpha: NA
#> ├─grid.labels.show: TRUE
#> ├─grid.labels.size: 0.6
#> ├─grid.labels.col: NA
#> ├─grid.labels.fontface: <NULL>
#> ├─grid.labels.fontfamily: <NULL>
#> ├─grid.labels.rot<dbl [2]>: 0, 0
#> ├─grid.labels.format: <list>
#> │ └─big.mark: ","
#> ├─grid.labels.cardinal: FALSE
#> ├─grid.labels.margin.x: 0
#> ├─grid.labels.margin.y: 0
#> ├─grid.labels.space.x: NA
#> ├─grid.labels.space.y: NA
#> ├─grid.labels.inside_frame: FALSE
#> ├─grid.ticks: TRUE
#> ├─grid.lines: TRUE
#> ├─grid.ndiscr: 100
#> ├─mouse_coordinates.position: S3<tm_pos>
#> │ ├─pos.h: "right"
#> │ ├─pos.v: "bottom"
#> │ ├─align.h: "right"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [6]>: "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "in"
#> ├─minimap.server: NA
#> ├─minimap.toggle: TRUE
#> ├─minimap.position: S3<tm_pos>
#> │ ├─pos.h: "right"
#> │ ├─pos.v: "bottom"
#> │ ├─align.h: "right"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [6]>: "pos.h", "pos.v", "align.h", "align.v", "just.h", "just.v"
#> │ └─type: "in"
#> ├─minimap.height: 7
#> ├─minimap.width: 7
#> ├─minimap.margins<dbl [4]>: 0.2, 0.2, 0.2, 0.2
#> ├─minimap.between_margin: 0.2
#> ├─minimap.frame: FALSE
#> ├─minimap.bg: TRUE
#> ├─panel.show: NA
#> ├─panel.labels: NA
#> ├─panel.label.size: 1
#> ├─panel.label.color: "black"
#> ├─panel.label.fontface: <NULL>
#> ├─panel.label.fontfamily: <NULL>
#> ├─panel.label.alpha: 1
#> ├─panel.label.bg: TRUE
#> ├─panel.label.bg.color: "grey80"
#> ├─panel.label.bg.alpha: 1
#> ├─panel.label.frame: TRUE
#> ├─panel.label.frame.color: NA
#> ├─panel.label.frame.alpha: 1
#> ├─panel.label.frame.lwd: 1
#> ├─panel.label.frame.r: NA
#> ├─panel.label.height: 1
#> ├─panel.label.rot<dbl [4]>: 90, 0, 270, 0
#> ├─qtm.scalebar: FALSE
#> ├─qtm.minimap: FALSE
#> ├─qtm.mouse_coordinates: FALSE
#> ├─earth_boundary: FALSE
#> ├─earth_boundary.color: NA
#> ├─earth_boundary.lwd: 1
#> ├─earth_datum: "OGC:CRS84"
#> ├─space: TRUE
#> ├─space.color: "#ffffff"
#> ├─space_overlay: NA
#> ├─check_and_fix: FALSE
#> ├─basemap.show: FALSE
#> ├─basemap.server<chr [3]>: "Esri.WorldGr...", "OpenStreetMap", "Esri.WorldTo..."
#> ├─basemap.alpha: 1
#> ├─basemap.zoom: NA
#> ├─tiles.show: FALSE
#> ├─tiles.server: ""
#> ├─tiles.alpha: 1
#> ├─tiles.zoom: NA
#> ├─attr.color: "black"
#> ├─crs_extra: ""
#> └─crs_global: "+proj=eqearth"

# a fancy set of options:
tmap_options(
  bg.color = "steelblue",
  outer.bg.color = "salmon",
  frame.color = "purple3",
  frame.lwd = 5,
  compass.type = "8star",
  legend.bg.color = "gold",
  legend.position = tm_pos_in(pos.h = "left", pos.v = "top")
 )

if (requireNamespace("lobstr")) {
  lobstr::tree(
    tmap_options_diff()
  )
}
#> current tmap options (style "white (modified)") that are different from default tmap options (style "white"):
#> <list>
#> ├─bg.color: "steelblue"
#> ├─outer.bg.color: "salmon"
#> ├─frame.color: "purple3"
#> ├─frame.lwd: 5
#> ├─legend.position: S3<tm_pos>
#> │ ├─cell.h: "right"
#> │ ├─cell.v: "bottom"
#> │ ├─pos.h: "left"
#> │ ├─pos.v: "top"
#> │ ├─align.h: "left"
#> │ ├─align.v: "top"
#> │ ├─just.h: "left"
#> │ ├─just.v: "top"
#> │ ├─called<chr [2]>: "pos.h", "pos.v"
#> │ └─type: "in"
#> ├─legend.bg.color: "gold"
#> └─compass.type: "8star"

tm_shape(World) +
  tm_polygons("footprint")


tmap_options_save("fancy")
#> current tmap options saved as style "fancy"

# the default style:
tmap_style("white")
#> style set to "white" (tmap default)
#> other available styles are: "gray", "natural", "cobalt", "albatross", "beaver", "bw", "classic", "watercolor", "fancy"
#> tmap v3 styles: "v3" (tmap v3 default), "gray_v3", "natural_v3", "cobalt_v3", "albatross_v3", "beaver_v3", "bw_v3", "classic_v3", "watercolor_v3"

tm_shape(World) +
  tm_polygons("footprint")


tmap_style("fancy")
#> style set to "fancy"
#> other available styles are: "white" (tmap default), "gray", "natural", "cobalt", "albatross", "beaver", "bw", "classic", "watercolor"
#> tmap v3 styles: "v3" (tmap v3 default), "gray_v3", "natural_v3", "cobalt_v3", "albatross_v3", "beaver_v3", "bw_v3", "classic_v3", "watercolor_v3"

tm_shape(World) +
  tm_polygons("footprint")


# reset all options
tmap_options_reset()
#> tmap options successfully reset
```
