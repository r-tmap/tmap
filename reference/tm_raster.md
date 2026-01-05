# Map layer: raster

Map layer that draws rasters. Supported visual variable is: `col` (the
color).

## Usage

``` r
tm_raster(
  col = tm_vars(),
  col.scale = tm_scale(),
  col.legend = tm_legend(),
  col.chart = tm_chart_none(),
  col.free = NA,
  col_alpha = tm_const(),
  col_alpha.scale = tm_scale(),
  col_alpha.legend = tm_legend(),
  col_alpha.chart = tm_chart_none(),
  col_alpha.free = NA,
  zindex = NA,
  group = NA,
  group.control = "check",
  options = opt_tm_raster(),
  ...
)

opt_tm_raster(interpolate = FALSE)
```

## Arguments

- col, col.scale, col.legend, col.chart, col.free:

  Visual variable that determines the color. See details.

- col_alpha, col_alpha.scale, col_alpha.legend, col_alpha.chart,
  col_alpha.free:

  Visual variable that determines the color transparency. See details.

- zindex:

  Map layers are drawn on top of each other. The `zindex` numbers (one
  for each map layer) determines the stacking order. By default the map
  layers are drawn in the order they are called.

- group:

  Name of the group to which this layer belongs. This is only relevant
  in view mode, where layer groups can be switched (see `group.control`)

- group.control:

  In view mode, the group control determines how layer groups can be
  switched on and off. Options: `"radio"` for radio buttons (meaning
  only one group can be shown), `"check"` for check boxes (so multiple
  groups can be shown), and `"none"` for no control (the group cannot be
  (de)selected).

- options:

  options passed on to the corresponding `opt_<layer_function>` function

- ...:

  to catch deprecated arguments from version \< 4.0

- interpolate:

  Should the raster image be interpolated? Currently only applicable in
  view mode (passed on to
  [`grid`](https://rdrr.io/r/grid/grid.raster.html))

## Details

The visual variable arguments (e.g. `col`) can be specified with either
a data variable name (e.g., a spatial vector attribute or a raster layer
of the object specified in
[`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md)), or
with a visual value (for `col`, a color is expected). See [vignette
about visual
variables](https://r-tmap.github.io/tmap/articles/basics_vv).

Multiple values can be specified: in that case facets are created. These
facets can be combined with other faceting data variables, specified
with
[`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md).
See [vignette about
facets](https://r-tmap.github.io/tmap/articles/basics_facets).

- The `*.scale` arguments determine the used scale to map the data
  values to visual variable values. These can be specified with one of
  the available `tm_scale_*()` functions. The default is specified by
  the tmap option
  ([`tm_options()`](https://r-tmap.github.io/tmap/reference/tm_options.md))
  `scales.var`. See [vignette about
  scales](https://r-tmap.github.io/tmap/articles/basics_scales).

- The `*.legend` arguments determine the used legend, specified with
  [`tm_legend()`](https://r-tmap.github.io/tmap/reference/tm_legend.md).
  The default legend and its settings are determined by the tmap options
  ([`tm_options()`](https://r-tmap.github.io/tmap/reference/tm_options.md))
  `legend.` . See [vignette about
  legends](https://r-tmap.github.io/tmap/articles/basics_legends).

- The `*.chart` arguments specify additional charts, specified with
  `tm_chart_`, e.g.
  [`tm_chart_histogram()`](https://r-tmap.github.io/tmap/reference/tm_chart.md).
  See [vignette about
  charts](https://r-tmap.github.io/tmap/articles/basics_charts).

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

## Examples

``` r
if (FALSE) { # \dontrun{
# load land data
data(land, World)

tm_shape(land) +
  tm_raster("cover")

tm_shape(land) +
  tm_raster("elevation", col.scale = tm_scale_continuous(values = terrain.colors(9))) +
  tm_shape(World) +
  tm_borders()
} # }
```
