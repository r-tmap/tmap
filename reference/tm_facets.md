# Specify facets

- `tm_facets_wrap()` specify facets for one grouping variable (so one
  faceting dimension)

- `tm_facets_(hv)stack()` stacks the facets either horizontally or
  vertically (one grouping variable).

- `tm_facets_grid()` specify facets for two grouping variables in a grid
  of rows and columns.

- `tm_facets_pagewise()` same as wrap, but the facets are drawn on
  different plots (pages). Replaces the `along` argument from version 3.

- `tm_facets()` is the core function, but it is recommended to use the
  other functions.

## Usage

``` r
tm_facets(
  by = NULL,
  rows = NULL,
  columns = NULL,
  pages = NULL,
  as.layers = FALSE,
  nrow = NA,
  ncol = NA,
  byrow = TRUE,
  orientation = NA,
  free.coords = NA,
  drop.units = TRUE,
  drop.empty.facets = TRUE,
  drop.NA.facets = FALSE,
  sync = TRUE,
  na.text = NA,
  scale.factor = 2,
  type = NA,
  free.scales = NULL,
  ...
)

tm_facets_grid(rows = NULL, columns = NULL, pages = NULL, ...)

tm_facets_wrap(by = "VARS__", nrow = NA, ncol = NA, byrow = TRUE, ...)

tm_facets_pagewise(by = "VARS__", byrow = TRUE, ...)

tm_facets_stack(by = "VARS__", orientation = NA, ...)

tm_facets_hstack(by = "VARS__", ...)

tm_facets_vstack(by = "VARS__", ...)

tm_facets_flip(...)
```

## Arguments

- by:

  Group by variable (only for a facet wrap or facet stack)

- rows:

  Variable that specifies the rows (only for a facet grid)

- columns:

  Variable that specifies the columns (only for a facet grid)

- pages:

  Variable that specifies the pages (only for a facet grid)

- as.layers:

  show facets as layers?

- nrow:

  Number of rows

- ncol:

  Number of columns

- byrow:

  Should facets be wrapped by row?

- orientation:

  For facet stack: horizontal or vertical?

- free.coords:

  Logical. If the `by` argument is specified, should each map has its
  own coordinate ranges? By default `TRUE`, unless facets are shown in
  as different layers (`as.layers = TRUE`)

- drop.units:

  Logical. If the `by` argument is specified, should non-selected
  spatial units be dropped? If `FALSE`, they are plotted where mapped
  aesthetics are regarded as missing values. Not applicable for raster
  shapes. By default `TRUE`.

- drop.empty.facets:

  Logical. If the `by` argument is specified, should empty facets be
  dropped? Empty facets occur when the `by`-variable contains unused
  levels. When `TRUE` and two `by`-variables are specified, empty rows
  and columns are dropped.

- drop.NA.facets:

  Logical. If the `by` argument is specified, and all data values for
  specific facets are missing, should these facets be dropped? `FALSE`
  by default. In v3, it was called `showNA`.

- sync:

  Logical. Should the navigation in view mode (zooming and panning) be
  synchronized? By default `TRUE` if the facets have the same bounding
  box. This is generally the case when rasters are plotted, or when
  `free.coords` is `FALSE`.

- na.text:

  Text used for facets of missing values. In v3, it was `textNA`.

- scale.factor:

  Number that determines how the elements (e.g. font sizes, symbol
  sizes, line widths) of the small multiples are scaled in relation to
  the scaling factor of the shapes. The elements are scaled to the
  `scale.factor`th root of the scaling factor of the shapes. So, for
  `scale.factor = 1`, they are scaled proportional to the scaling of the
  shapes. Since elements, especially text, are often too small to read,
  a higher value is recommended. By default, `scale.factor = 2`.

- type:

  `"grid"`, `"wrap"` or `"stack"`

- free.scales:

  deprecated. Please use the `.free` arguments in the layer functions,
  e.g. `fill.free` in `tm_polygons`.

- ...:

  passed on to `tm_facets()`

## See also

[`tm_animate()`](https://r-tmap.github.io/tmap/reference/tm_animate.md)

[Vignette about
facets](https://r-tmap.github.io/tmap/articles/basics_facets)

## Examples

``` r
if (FALSE) { # \dontrun{
tm_shape(NLD_dist) +
  tm_polygons("edu_appl_sci",
    fill.scale = tm_scale_intervals(values = "pu_gn", style = "kmeans", n = 7)) +
  tm_facets(by = "province") +
tm_shape(NLD_muni) +
  tm_borders(lwd = 3) +
  tm_facets(by = "province") +
tm_title("Population with a univeristy degree (incl appl. sciences), percentages")

tm_shape(World) +
  tm_polygons(c("gender", "press"),
    fill.scale = list(tm_scale_intervals(values = "bu_br_div", midpoint = 0.5),
      tm_scale_intervals(values = "pu_gn_div", midpoint = 50)),
    fill.legend = tm_legend("")) +
tm_layout(panel.labels = c("Gender Inequality Index (GII)", "World Press Freedom Index"))
} # }
```
