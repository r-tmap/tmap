# Map layer: lines

Map layer that draws lines. Supported visual variables are: `col` (the
color), `lwd` (line width), `lty` (line type), and `col_alpha` (color
alpha transparency).

## Usage

``` r
tm_lines(
  col = tm_const(),
  col.scale = tm_scale(),
  col.legend = tm_legend(),
  col.chart = tm_chart_none(),
  col.free = NA,
  lwd = tm_const(),
  lwd.scale = tm_scale(),
  lwd.legend = tm_legend(),
  lwd.chart = tm_chart_none(),
  lwd.free = NA,
  lty = tm_const(),
  lty.scale = tm_scale(),
  lty.legend = tm_legend(),
  lty.chart = tm_chart_none(),
  lty.free = NA,
  col_alpha = tm_const(),
  col_alpha.scale = tm_scale(),
  col_alpha.legend = tm_legend(),
  col_alpha.chart = tm_chart_none(),
  col_alpha.free = NA,
  linejoin = "round",
  lineend = "round",
  plot.order = tm_plot_order("lwd", reverse = TRUE, na.order = "bottom"),
  zindex = NA,
  group = NA,
  group.control = "check",
  popup = tm_popup(),
  popup.vars = NA,
  popup.format = tm_label_format(),
  hover = NA,
  id = "",
  blend = "over",
  options = opt_tm_lines(),
  ...
)

opt_tm_lines(lines.only = "ifany", hitbox = "auto")
```

## Arguments

- col, col.scale, col.legend, col.chart, col.free:

  Visual variable that determines the color. See details. *Unit:* Color
  – a color name, hex string.

- lwd, lwd.scale, lwd.legend, lwd.chart, lwd.free:

  Visual variable that determines the line width. See details. *Unit:*
  Base R line-width units; 1 lwd is approx. 0.75 pt at 96 dpi.

- lty, lty.scale, lty.legend, lty.chart, lty.free:

  Visual variable that determines the line type. See details. *Unit:*
  Integer (1-6) or name: "solid", "dashed", "dotted", "dotdash",
  "longdash", "twodash".

- col_alpha, col_alpha.scale, col_alpha.legend, col_alpha.chart,
  col_alpha.free:

  Visual variable that determines the color transparency. See details.
  *Unit:* Proportion – numeric 0-1 (0 = fully transparent, 1 = fully
  opaque).

- linejoin, lineend:

  line join and line end. See [gpar()](https://rdrr.io/r/grid/gpar.html)
  for details.

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

- popup:

  popup specification for `"view"` mode, the output of
  [`tm_popup()`](https://r-tmap.github.io/tmap/reference/tm_popup.md).
  It determines the data variables shown in the popup table, the popup
  title, and (in the future) the popup layout. This replaces the
  deprecated arguments `popup.vars` and `popup.format`.

- popup.vars:

  (Deprecated.) Use `popup` with
  [`tm_popup()`](https://r-tmap.github.io/tmap/reference/tm_popup.md)
  instead (via its `vars` argument). Names of data variables that are
  shown in the popups in `"view"` mode. Set `popup.vars` to `TRUE` to
  show all variables in the shape object. Set `popup.vars` to `FALSE` to
  disable popups. Set `popup.vars` to a character vector of variable
  names to show those variables in the popups. The default (`NA`)
  depends on whether visual variables (e.g. `fill`) are used. If so,
  only those are shown. If not, all variables in the shape object are
  shown.

- popup.format:

  (Deprecated.) Use `popup` with
  [`tm_popup()`](https://r-tmap.github.io/tmap/reference/tm_popup.md)
  instead (via its `format` argument). List of formatting options for
  the popup values. Output of
  [`tm_label_format()`](https://r-tmap.github.io/tmap/reference/tm_label_format.md).
  Only applicable for numeric data variables. If one list of formatting
  options is provided, it is applied to all numeric variables of
  `popup.vars`. Also, a (named) list of lists can be provided. In that
  case, each list of formatting options is applied to the named
  variable.

- hover:

  name of the data variable that specifies the hover labels (view mode
  only). Set to `FALSE` to disable hover labels. By default `FALSE`,
  unless `id` is specified. In that case, it is set to `id`,

- id:

  name of the data variable that specifies the indices of the spatial
  features. Only used for `"view"` mode.

- blend:

  Compositing operator for layer blending. Default `"over"` applies no
  blending. See the "Layer blending" section for the supported values.

- options:

  options passed on to the corresponding `opt_<layer_function>` function

- ...:

  to catch deprecated arguments from version \< 4.0

- lines.only:

  should only line geometries of the shape object (defined in
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md))
  be plotted, or also other geometry types (like polygons)? By default
  `"ifany"`, which means `TRUE` in case a geometry collection is
  specified.

- hitbox:

  Controls whether an invisible interaction layer with a larger
  clickable area (\\hitbox\\) is added on top of the lines.

  This can improve click and popup behaviour for thin or densely packed
  lines by increasing the effective mouse interaction width.

  Possible values:

  "none"

  :   No additional hitbox layer is added. Lines are clickable only at
      their visible width.

  "plusX"

  :   Adds `X` pixels to the visible line width to compute the
      interaction width: `line_width + X`. For example, `"plus8"` widens
      the clickable area by 4 pixels on each side.

  "pmaxX"

  :   Ensures a minimum interaction width of `X` pixels:
      `pmax(line_width, X)`. For example, `"pmax8"` guarantees at least
      8 pixels. Useful for very thin lines.

  "auto"

  :   `"pmax8"` if and only if interactive features are enabled (popup
      or hover), lines are thin (median line width \< 4), and there are
      fewer than 10000 features. Otherwise `"none"`.

  `plus` and `pmax` can be combined, e.g. `"plus4pmax8"` means
  `pmax(line_width + 4, 8)`.

  Adding a hitbox improves usability for thin lines but may reduce
  performance for very large datasets, as an additional invisible layer
  is rendered.

## Details

The visual variable arguments (e.g. `col`) can be specified with a data
variable name (e.g., a spatial vector attribute or a raster layer of the
object specified in
[`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md)),
with a visual value (for `col`, a color is expected), or with a
geometry-derived variable (see below). See vignette: [Visual
variables](https://r-tmap.github.io/tmap/articles/basics_vv).

Multiple values can be specified: in that case facets are created. These
facets can be combined with other faceting data variables, specified
with
[`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md).
See vignette:
[Facets](https://r-tmap.github.io/tmap/articles/basics_facets).

- The `*.scale` arguments determine the used scale to map the data
  values to visual variable values. These can be specified with one of
  the available `tm_scale_*()` functions. The default is specified by
  the tmap option
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
  visual variable `col`, each row of facets will have its own scale, and
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

### Visual variable units

Every visual variable maps data values to a specific output unit.
Knowing the unit matters when supplying constant values via
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

[Visual variables](https://r-tmap.github.io/tmap/articles/basics_vv),
[Scales](https://r-tmap.github.io/tmap/articles/basics_scales),
[Legends](https://r-tmap.github.io/tmap/articles/basics_legends),
[Facets](https://r-tmap.github.io/tmap/articles/basics_facets),
[Units](https://r-tmap.github.io/tmap/articles/foundations_units),
[Layer blending](https://r-tmap.github.io/tmap/articles/adv_blend),
[Terrain map](https://r-tmap.github.io/tmap/articles/examples_terrain)

## Examples

``` r
tm_shape(World_rivers) +
  tm_lines(lwd = "strokelwd",
       lwd.scale = tm_scale_asis(values.scale = 0.2, value.neutral = 2),
       col = "scalerank",
       col.scale = tm_scale_categorical(values = "seaborn.dark"))


tm_shape(World) +
  tm_lines(col = "continent",
       col.scale = tm_scale_categorical(values = "seaborn.dark"),
       lty = "continent",
       lwd = 1.5,
       lty.legend = tm_legend_combine("col"))
```
