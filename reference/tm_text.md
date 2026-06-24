# Map layer: text

Map layer that draws symbols Supported map variables are: `text` (the
text itself) `col` (color), `size` (font size), and `fontface` (font
face).

## Usage

``` r
tm_text(
  text = tm_const(),
  text.scale = tm_scale(),
  text.legend = tm_legend(),
  text.chart = tm_chart_none(),
  text.free = NA,
  size = tm_const(),
  size.scale = tm_scale(),
  size.legend = tm_legend(),
  size.chart = tm_chart_none(),
  size.free = NA,
  col = tm_const(),
  col.scale = tm_scale(),
  col.legend = tm_legend(),
  col.chart = tm_chart_none(),
  col.free = NA,
  col_alpha = tm_const(),
  col_alpha.scale = tm_scale(),
  col_alpha.legend = tm_legend(),
  col_alpha.chart = tm_chart_none(),
  col_alpha.free = NA,
  fontface = tm_const(),
  fontface.scale = tm_scale(),
  fontface.legend = tm_legend(),
  fontface.chart = tm_chart_none(),
  fontface.free = NA,
  fontfamily = NA,
  bgcol = tm_const(),
  bgcol.scale = tm_scale(),
  bgcol.legend = tm_legend(),
  bgcol.chart = tm_chart_none(),
  bgcol.free = NA,
  bgcol_alpha = tm_const(),
  bgcol_alpha.scale = tm_scale(),
  bgcol_alpha.legend = tm_legend(),
  bgcol_alpha.chart = tm_chart_none(),
  bgcol_alpha.free = NA,
  xmod = 0,
  xmod.scale = tm_scale(),
  xmod.legend = tm_legend_hide(),
  xmod.chart = tm_chart_none(),
  xmod.free = NA,
  ymod = 0,
  ymod.scale = tm_scale(),
  ymod.legend = tm_legend_hide(),
  ymod.chart = tm_chart_none(),
  ymod.free = NA,
  angle = 0,
  angle.scale = tm_scale(),
  angle.legend = tm_legend_hide(),
  angle.chart = tm_chart_none(),
  angle.free = NA,
  plot.order = tm_plot_order("size", reverse = FALSE),
  zindex = NA,
  group = NA,
  group.control = "check",
  blend = "over",
  options = opt_tm_text(),
  ...
)

tm_labels(
  text = tm_const(),
  text.scale = tm_scale(),
  text.legend = tm_legend(),
  text.chart = tm_chart_none(),
  text.free = NA,
  size = tm_const(),
  size.scale = tm_scale(),
  size.legend = tm_legend(),
  size.chart = tm_chart_none(),
  size.free = NA,
  col = tm_const(),
  col.scale = tm_scale(),
  col.legend = tm_legend(),
  col.chart = tm_chart_none(),
  col.free = NA,
  col_alpha = tm_const(),
  col_alpha.scale = tm_scale(),
  col_alpha.legend = tm_legend(),
  col_alpha.chart = tm_chart_none(),
  col_alpha.free = NA,
  fontface = tm_const(),
  fontface.scale = tm_scale(),
  fontface.legend = tm_legend(),
  fontface.chart = tm_chart_none(),
  fontface.free = NA,
  fontfamily = "",
  bgcol = tm_const(),
  bgcol.scale = tm_scale(),
  bgcol.legend = tm_legend(),
  bgcol.chart = tm_chart_none(),
  bgcol.free = NA,
  bgcol_alpha = tm_const(),
  bgcol_alpha.scale = tm_scale(),
  bgcol_alpha.legend = tm_legend(),
  bgcol_alpha.chart = tm_chart_none(),
  bgcol_alpha.free = NA,
  xmod = 0,
  xmod.scale = tm_scale(),
  xmod.legend = tm_legend_hide(),
  xmod.chart = tm_chart_none(),
  xmod.free = NA,
  ymod = 0,
  ymod.scale = tm_scale(),
  ymod.legend = tm_legend_hide(),
  ymod.chart = tm_chart_none(),
  ymod.free = NA,
  angle = 0,
  angle.scale = tm_scale(),
  angle.legend = tm_legend_hide(),
  angle.chart = tm_chart_none(),
  angle.free = NA,
  plot.order = tm_plot_order("AREA", reverse = FALSE, na.order = "bottom"),
  zindex = NA,
  group = NA,
  group.control = "check",
  options = opt_tm_labels(),
  ...
)

tm_labels_highlighted(
  text = tm_const(),
  text.scale = tm_scale(),
  text.legend = tm_legend(),
  text.chart = tm_chart_none(),
  text.free = NA,
  size = tm_const(),
  size.scale = tm_scale(),
  size.legend = tm_legend(),
  size.chart = tm_chart_none(),
  size.free = NA,
  col = tm_const(),
  col.scale = tm_scale(),
  col.legend = tm_legend(),
  col.chart = tm_chart_none(),
  col.free = NA,
  col_alpha = tm_const(),
  col_alpha.scale = tm_scale(),
  col_alpha.legend = tm_legend(),
  col_alpha.chart = tm_chart_none(),
  col_alpha.free = NA,
  fontface = tm_const(),
  fontface.scale = tm_scale(),
  fontface.legend = tm_legend(),
  fontface.chart = tm_chart_none(),
  fontface.free = NA,
  fontfamily = "",
  bgcol = tm_const(),
  bgcol.scale = tm_scale(),
  bgcol.legend = tm_legend(),
  bgcol.chart = tm_chart_none(),
  bgcol.free = NA,
  bgcol_alpha = tm_const(),
  bgcol_alpha.scale = tm_scale(),
  bgcol_alpha.legend = tm_legend(),
  bgcol_alpha.chart = tm_chart_none(),
  bgcol_alpha.free = NA,
  xmod = 0,
  xmod.scale = tm_scale(),
  xmod.legend = tm_legend_hide(),
  xmod.chart = tm_chart_none(),
  xmod.free = NA,
  ymod = 0,
  ymod.scale = tm_scale(),
  ymod.legend = tm_legend_hide(),
  ymod.chart = tm_chart_none(),
  ymod.free = NA,
  angle = 0,
  angle.scale = tm_scale(),
  angle.legend = tm_legend_hide(),
  angle.chart = tm_chart_none(),
  angle.free = NA,
  plot.order = tm_plot_order("AREA", reverse = FALSE, na.order = "bottom"),
  zindex = NA,
  group = NA,
  group.control = "check",
  options = opt_tm_labels(),
  ...
)

opt_tm_text(
  points_only = "ifany",
  point_per = "feature",
  on_surface = FALSE,
  shadow = FALSE,
  shadow.col = NA,
  shadow.offset.x = 0.05,
  shadow.offset.y = 0.05,
  halo = FALSE,
  halo.col = NA,
  halo.width = 0.05,
  halo.blur = 0,
  halo.alpha = 0.8,
  just = "center",
  along_lines = FALSE,
  bg.padding = 0.4,
  bg.border = FALSE,
  bg.border.col = "black",
  bg.border.lwd = 1,
  clustering = FALSE,
  point.label = FALSE,
  point.label.gap = 0,
  point.label.method = "SANN",
  remove_overlap = FALSE
)

opt_tm_labels(
  points_only = "ifany",
  point_per = "feature",
  on_surface = FALSE,
  shadow = FALSE,
  shadow.col = NA,
  shadow.offset.x = 0.05,
  shadow.offset.y = 0.05,
  halo = FALSE,
  halo.col = NA,
  halo.width = 0.05,
  halo.blur = 0,
  halo.alpha = 0.8,
  just = "center",
  along_lines = TRUE,
  bg.padding = 0.4,
  bg.border = FALSE,
  bg.border.col = "black",
  bg.border.lwd = 1,
  clustering = FALSE,
  point.label = NA,
  point.label.gap = 0.4,
  point.label.method = "SANN",
  remove_overlap = FALSE
)
```

## Arguments

- text, text.scale, text.legend, text.chart, text.free:

  Map variable that determines the text. See details. *Unit:* Character
  string.

- size, size.scale, size.legend, size.chart, size.free:

  Map variable that determines the size. See details. *Unit:* Multiplier
  of the base font size. `size = 1` renders at the default font size,
  which is 12 pt in plot mode (`par("ps")`) and 12 px in view mode
  (consistent by design). `size = 1.5` renders at 18 pt / px, etc.

- col, col.scale, col.legend, col.chart, col.free:

  Map variable that determines the color. See details. *Unit:* Color – a
  color name, hex string.

- col_alpha, col_alpha.scale, col_alpha.legend, col_alpha.chart,
  col_alpha.free:

  Map variable that determines the color transparency. See details.
  *Unit:* Proportion – numeric 0-1 (0 = fully transparent, 1 = fully
  opaque).

- fontface, fontface.scale, fontface.legend, fontface.chart,
  fontface.free:

  Map variable that determines the font face. See details. *Unit:*
  "plain", "bold", "italic", or "bold.italic".

- fontfamily:

  The font family. See [gpar()](https://rdrr.io/r/grid/gpar.html) for
  details.

- bgcol, bgcol.scale, bgcol.legend, bgcol.chart, bgcol.free:

  Map variable that determines the background color. See Details.
  *Unit:* Color – a color name, hex string.

- bgcol_alpha, bgcol_alpha.scale, bgcol_alpha.legend, bgcol_alpha.chart,
  bgcol_alpha.free:

  Map variable that determines the background color transparency. See
  Details. *Unit:* Proportion – numeric 0-1 (0 = fully transparent, 1 =
  fully opaque).

- xmod, xmod.scale, xmod.legend, xmod.chart, xmod.free:

  Transformation variable that determines the x offset. See details.
  *Unit:* Line heights, relative to the label anchor. Positive = right.

- ymod, ymod.scale, ymod.legend, ymod.chart, ymod.free:

  Transformation variable that determines the y offset. See details.
  *Unit:* Line heights, relative to the label anchor. Positive = up. the
  text. See details.

- angle, angle.scale, angle.legend, angle.chart, angle.free:

  Rotation angle *Unit:* Degrees, clockwise from north (0-360).

- plot.order:

  Specification in which order the spatial features are drawn. See
  [`tm_plot_order()`](https://r-tmap.github.io/tmap/reference/tm_plot_order.md)
  for details.

- zindex:

  Controls the stacking order of map layers. Should be set to a value
  above 400. By default, layers are stacked in call order, starting
  at 401. See details.

- group:

  Name of the group to which this layer belongs. This is only relevant
  in view mode, where layer groups can be switched (see `group.control`)

- group.control:

  In view mode, the group control determines how layer groups can be
  switched on and off. Options: `"radio"` for radio buttons (meaning
  only one group can be shown), `"check"` for check boxes (so multiple
  groups can be shown), and `"none"` for no control (the group cannot be
  (de)selected).

- blend:

  Compositing operator for layer blending. Default `"over"` applies no
  blending. See the "Layer blending" section for the supported values.

- options:

  options passed on to the corresponding `opt_<layer_function>` function

- ...:

  to catch deprecated arguments from version \< 4.0

- points_only:

  should only point geometries of the shape object (defined in
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md))
  be plotted? By default `"ifany"`, which means `TRUE` in case a
  geometry collection is specified.

- point_per:

  specification of how spatial points are mapped when the geometry is a
  multi line or a multi polygon. One of `"feature"`, `"segment"` or
  `"largest"`. The first generates a spatial point for every feature,
  the second for every segment (i.e. subfeature), the third only for the
  largest segment (subfeature). Note that the last two options can be
  significant slower.

- on_surface:

  In case of polygons, centroids are computed. Should the points be on
  the surface? If `TRUE`, which is slower than the default `FALSE`,
  centroids outside the surface are replaced with points computed with
  [`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

- shadow:

  Shadow behind the text. Logical.

- shadow.col:

  Color of the shadow.

- shadow.offset.x, shadow.offset.y:

  Shadow offset in line heights

- halo:

  Halo behind the text. In plot mode, it is just an outline, in view
  mode also a subtle glow.

- halo.col:

  Color of the halo.

- halo.width:

  Width (thickness) of the halo outline. In line heights

- halo.blur:

  Blur radius of the halo glow (view mode only). In line heights. Should
  be sufficiently larger than `halo.width` in order to see the effect.

- halo.alpha:

  Alpha transparency of the halo

- just:

  justification of the text relative to the point coordinates. Either
  one of the following values: `"left"` , `"right"`, `"center"`,
  `"bottom"`, and `"top"`, or a vector of two values where first value
  specifies horizontal and the second value vertical justification.
  Besides the mentioned values, also numeric values between 0 and 1 can
  be used. 0 means left justification for the first value and bottom
  justification for the second value. Note that in view mode, only one
  value is used.

- along_lines:

  logical that determines whether labels are rotated along the spatial
  lines. Only applicable if a spatial lines shape is used.

- bg.padding:

  The padding of the background in terms of line heights.

- bg.border:

  Should the background have borders?

- bg.border.col:

  Color of the borders

- bg.border.lwd:

  Line width of the borders

- clustering:

  in interactive modes (e.g. `"view"` mode), should clustering be
  applied at lower zoom levels? Either `FALSE` (default), `TRUE`, or a
  mode specific specification, e.g. for `"view"` mode
  [`markerClusterOptions`](https://rstudio.github.io/leaflet/reference/map-options.html).

- point.label:

  logical that determines whether the labels are placed automatically.
  By default `FALSE` for `tm_text`, and `TRUE` for `tm_labels` if the
  number of labels is less than 500 (otherwise it will be too slow).

- point.label.gap:

  numeric that determines the gap between the point and label

- point.label.method:

  the optimization method, either `"SANN"` for simulated annealing (the
  default) or `"GA"` for a genetic algorithm.

- remove_overlap:

  logical that determines whether the overlapping labels are removed

## Details

The map variable arguments (e.g. `col`) can be specified with a data
variable name (e.g., a spatial vector attribute or a raster layer of the
object specified in
[`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md)),
with a visual value (for `col`, a color is expected), or with a
geometry-derived variable (see below). See vignette: [Map
variables](https://r-tmap.github.io/tmap/articles/basics_vv).

Multiple values can be specified: in that case facets are created. These
facets can be combined with other faceting data variables, specified
with
[`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md).
See vignette:
[Facets](https://r-tmap.github.io/tmap/articles/basics_facets).

- The `*.scale` arguments determine the used scale to map the data
  values to map variable values. These can be specified with one of the
  available `tm_scale_*()` functions. The default is specified by the
  tmap option
  ([`tm_options()`](https://r-tmap.github.io/tmap/reference/tm_options.md))
  `scales.var`. See \`rvignette:
  [Scales](https://r-tmap.github.io/tmap/articles/basics_scales)

- The `*.legend` arguments determine the used legend, specified with
  [`tm_legend()`](https://r-tmap.github.io/tmap/reference/tm_legend.md).
  The default legend and its settings are determined by the tmap options
  ([`tm_options()`](https://r-tmap.github.io/tmap/reference/tm_options.md))
  `legend.` . See \`rvignette:
  [Legends](https://r-tmap.github.io/tmap/articles/basics_legends)

- The `*.chart` arguments specify additional charts, specified with
  `tm_chart_`, e.g.
  [`tm_chart_histogram()`](https://r-tmap.github.io/tmap/reference/tm_chart.md).
  See \`rvignette:
  [Charts](https://r-tmap.github.io/tmap/articles/basics_charts)

- The `*.free` arguments determine whether scales are applied freely
  across facets, or shared. A logical value is required. They can also
  be specified with a vector of three logical values; these determine
  whether scales are applied freely per facet dimension. This is only
  useful when facets are applied (see
  [`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md)).
  There are maximally three facet dimensions: rows, columns, and pages.
  This only applies for a facet grid
  ([`tm_facets_grid()`](https://r-tmap.github.io/tmap/reference/tm_facets.md)).
  For instance, `col.free = c(TRUE, FALSE, FALSE)` means that for the
  map variable `col`, each row of facets will have its own scale, and
  therefore its own legend. For facet wraps and stacks
  ([`tm_facets_wrap()`](https://r-tmap.github.io/tmap/reference/tm_facets.md)
  and
  [`tm_facets_stack()`](https://r-tmap.github.io/tmap/reference/tm_facets.md))
  there is only one facet dimension, so the `*.free` argument requires
  only one logical value.

Currently, three geometry-derived variables are implemented:

- `"AREA"` (polygons only), which uses the feature area;

- `"LENGTH"` (lines only), which uses the feature length; and

- `"MAP_COLORS"`, which assigns values so that adjacent features receive
  different values, making it particularly suitable for coloring
  neighbouring polygons.

Note that geometry-derived variables do not generate a legend
automatically. If a legend is required, compute the corresponding
variable explicitly, for example with
[`sf::st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.html),
[`sf::st_length()`](https://r-spatial.github.io/sf/reference/geos_measures.html),
or
[`tmaptools::map_coloring()`](https://r-tmap.github.io/tmaptools/reference/map_coloring.html),
and use the resulting values instead.

### Map variable units

Every map variable maps data values to a specific output unit. Knowing
the unit matters when supplying constant values via
[`tm_const()`](https://r-tmap.github.io/tmap/reference/tm_const.md), or
output ranges via `values.range` / `values.scale` in the scale
functions.

|  |  |  |
|----|----|----|
| Variable | Output unit | Notes |
| `fill`, `col`, `bgcol` | color | name, hex, or palette string |
| `fill_alpha`, `col_alpha`, `bgcol_alpha` | proportion 0-1 | 0 = transparent, 1 = opaque |
| `size` (symbols, bubbles, squares, dots) | typographic lines | 1 line approx. 1/6 inch; scaled by `values.scale` |
| `size` (circles) | meters | plain numeric or a `units` object |
| `size` (text, labels) | multiplier | 1 = 12 pt (plot) / 12 px (view) |
| `lwd` | lwd | base R units; 1 lwd approx. 0.75 pt at 96 dpi |
| `lty` | – | integer 1-6 or name ("solid", "dashed", ...) |
| `shape` | – | integer `pch` 1-25 or single character |
| `angle` | degrees | 0-360, clockwise from north |
| `fontface` | – | "plain", "bold", "italic", "bold.italic" |

#### Symbol size (`size` in `tm_symbols`, `tm_bubbles`, `tm_squares`, `tm_dots`)

"Lines" is a typographic unit: one line is approximately 1/6 inch (the
default base line-height in R graphics). The global multiplier
`tmap_options(values.scale = list(size.bubbles = 1.5))` scales all
symbol sizes without changing the data mapping.

#### Circle size (`size` in `tm_circles`)

The value is a geographic radius in meters. A plain numeric vector is
interpreted as meters; a `units` object (from the **units** package) is
automatically converted, so `units::as_units(1, "mi")` gives a 1-mile
radius. Because the radius is geographic, circles scale with zoom in
interactive (view) mode – unlike bubble symbols which keep a fixed
screen size.

#### Text size (`size` in `tm_text`, `tm_labels`)

The value is a multiplier of the base font size. `size = 1` renders at
12 pt in plot mode (R's default `par("ps")`) and at 12 px in view mode
(`gp$cex * 12` px, see `tmapLeafletDataPlot.tm_data_text`); the two
modes are consistent by design.

### Layer blending (`blend`)

Blend modes control how a layer's pixels are combined with the pixels
beneath it. For each pixel, let \\S\\ be the source (top layer) RGB
value and \\D\\ be the destination (bottom layer) RGB value, both
normalised to \\\[0, 1\]\\.

|  |  |  |
|----|----|----|
| `blend` | Formula | Use case |
| `"over"` | \\S \cdot \alpha + D \cdot (1 - \alpha)\\ | Standard alpha compositing (default) |
| `"multiply"` | \\S \times D\\ | Hillshading over colour raster; both layers darken each other |
| `"screen"` | \\1 - (1 - S)(1 - D)\\ | Inverse of multiply; brightens |
| `"overlay"` | multiply if \\D \< 0.5\\, screen if \\D \geq 0.5\\ | Boosts contrast; preserves midtones |
| `"darken"` | \\\min(S, D)\\ | Keeps the darker of the two layers per channel |
| `"lighten"` | \\\max(S, D)\\ | Keeps the lighter of the two layers per channel |

Requires R \>= 4.2 and a compatible graphics device (e.g.
`png(type = "cairo")`,
[`svg()`](https://rdrr.io/r/grDevices/cairo.html)). In view mode,
blending is applied via CSS `mix-blend-mode`. See
[`grid::groupGrob()`](https://rdrr.io/r/grid/grid.group.html) for the
full list of supported operators.

### zindex and pane names

In view mode, each layer is rendered in a Leaflet pane named
`"tmap{zindex}"` (e.g., `"tmap401"`, `"tmap402"`), with base tile layers
placed in the standard `"tile"` pane.

## See also

[Map variables](https://r-tmap.github.io/tmap/articles/basics_vv),
[Scales](https://r-tmap.github.io/tmap/articles/basics_scales),
[Legends](https://r-tmap.github.io/tmap/articles/basics_legends),
[Facets](https://r-tmap.github.io/tmap/articles/basics_facets),
[Units](https://r-tmap.github.io/tmap/articles/foundations_units),
[Layer blending](https://r-tmap.github.io/tmap/articles/adv_blend)

[Map variables](https://r-tmap.github.io/tmap/articles/basics_vv),
[Scales](https://r-tmap.github.io/tmap/articles/basics_scales),
[Legends](https://r-tmap.github.io/tmap/articles/basics_legends),
[Facets](https://r-tmap.github.io/tmap/articles/basics_facets),
[Units](https://r-tmap.github.io/tmap/articles/foundations_units),
[Layer blending](https://r-tmap.github.io/tmap/articles/adv_blend),
[Topographic map
(Africa)](https://r-tmap.github.io/tmap/articles/examples_topo_Africa),
[Terrain map](https://r-tmap.github.io/tmap/articles/examples_terrain)

## Examples

``` r
tm_shape(World, bbox = World) +
  tm_text("name", size="pop_est", col="continent",
      col.scale = tm_scale_categorical(values = "seaborn.dark"),
      col.legend = tm_legend_hide(),
      size.scale = tm_scale_continuous(values.scale = 4),
      size.legend = tm_legend_hide())


metro$upside_down = ifelse(sf::st_coordinates(metro)[,2] < 0, 180, 0)
tm_shape(metro) +
  tm_text(text = "name", size = "pop2020",
      angle = "upside_down", size.legend = tm_legend_hide(),
      col = "upside_down",
      col.scale = tm_scale_categorical(values = c("#9900BB", "#228822")),
      col.legend = tm_legend_hide()) +
  tm_title_out("Which Hemisphere?", position = tm_pos_out("center", "top", pos.v = "bottom"))

```
