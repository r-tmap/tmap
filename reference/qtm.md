# Quick thematic map plot

Draw a thematic map quickly. This function is a convenient wrapper of
the main plotting method of stacking
[`tmap-element`](https://r-tmap.github.io/tmap/reference/tmap-element.md)s.
Without arguments or with a search term, this functions draws an
interactive map.

## Usage

``` r
qtm(
  shp = NULL,
  fill = tmap::tm_const(),
  col = tmap::tm_const(),
  size = tmap::tm_const(),
  shape = tmap::tm_const(),
  lwd = tmap::tm_const(),
  lty = tmap::tm_const(),
  fill_alpha = tmap::tm_const(),
  col_alpha = tmap::tm_const(),
  text = tmap::tm_const(),
  text_col = tmap::tm_const(),
  text_size = tmap::tm_const(),
  by = NULL,
  scale = NULL,
  title = NULL,
  crs = NULL,
  bbox = NULL,
  basemaps = NA,
  overlays = NA,
  zindex = NA,
  group = NA,
  group.control = "check",
  style = NULL,
  format = NULL,
  ...
)
```

## Arguments

- shp:

  One of:

  - shape object, which is an object from a class defined by the
    [`sf`](https://r-spatial.github.io/sf/reference/sf.html) or
    [`stars`](https://r-spatial.github.io/stars/reference/st_as_stars.html)
    package. Objects from the packages `sp` and `raster` are also
    supported, but discouraged.

  - Not specified, i.e. `qtm()` is executed. In this case a plain
    interactive map is shown.

  - An OpenStreetMap search string, e.g. `qtm("Amsterdam")`. In this
    case a plain interactive map is shown positioned according to the
    results of the search query (from OpenStreetMap nominatim)

- fill, col, size, shape, lwd, lty, fill_alpha, col_alpha:

  Visual variables.

- text, text_col, text_size:

  Visual variables.

- by:

  data variable name by which the data is split, or a vector of two
  variable names to split the data by two variables (where the first is
  used for the rows and the second for the columns). See also
  [`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md).

- scale:

  numeric value that serves as the global scale parameter. All font
  sizes, symbol sizes, border widths, and line widths are controlled by
  this value. The parameters `symbols.size`, `text.size`, and
  `lines.lwd` can be scaled separately with respectively
  `symbols.scale`, `text.scale`, and `lines.scale`. See also `...`.

- title:

  main title. For legend titles, use `X.legend`, where X is the layer
  name (see `...`).

- crs:

  Either a [`crs`](https://r-spatial.github.io/sf/reference/st_crs.html)
  object or a character value (`PROJ.4` character string). By default,
  the projection is used that is defined in the `shp` object itself.

- bbox:

  bounding box. Argument passed on to
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md)

- basemaps:

  name(s) of the provider or an URL of a tiled basemap. It is a shortcut
  to
  [`tm_basemap()`](https://r-tmap.github.io/tmap/reference/tm_basemap.md).
  Set to `NULL` to disable basemaps. By default, it is set to the tmap
  option `basemaps`.

- overlays:

  name(s) of the provider or an URL of a tiled overlay map. It is a
  shortcut to
  [`tm_tiles()`](https://r-tmap.github.io/tmap/reference/tm_basemap.md).

- zindex:

  zindex

- group:

  group

- group.control:

  group.control

- style:

  Layout options (see
  [`tm_layout()`](https://r-tmap.github.io/tmap/reference/tm_layout.md))
  that define the style. See
  [`tmap_style()`](https://r-tmap.github.io/tmap/reference/tmap_style.md)
  for details.

- format:

  Deprecated, see
  [`tm_format()`](https://r-tmap.github.io/tmap/reference/tmap-deprecated.md)
  for alternatives

- ...:

  arguments associated with the visual variables are passed on to the
  layer functions
  [`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md),
  [`tm_lines()`](https://r-tmap.github.io/tmap/reference/tm_lines.md),
  [`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md),
  and [`tm_text()`](https://r-tmap.github.io/tmap/reference/tm_text.md).
  For instance, `fill.scale` is the scale specifications of the fill
  color of polygons (see
  [`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md)).

## Value

A
[`tmap-element`](https://r-tmap.github.io/tmap/reference/tmap-element.md)

## Details

The first argument is a shape object (normally specified by
[`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md)).
The next arguments, from `fill` to `raster`, are the aesthetics from the
main layers. The remaining arguments are related to the map layout. Any
argument from any main layer function, such as
[`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md),
can be specified (see `...`). It is also possible to stack
[`tmap-element`](https://r-tmap.github.io/tmap/reference/tmap-element.md)s
on a `qtm` plot. See examples.

By default, a scale bar is shown. This option can be set with
[`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md)
(argument `qtm.scalebar`). A minimap is shown by default when `qtm` is
called without arguments of with a search term. This option can be set
with
[`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md)
(argument `qtm.minimap`).

## References

Tennekes, M., 2018, tmap: Thematic Maps in R, Journal of Statistical
Software, 84(6), 1-39,
[doi:10.18637/jss.v084.i06](https://doi.org/10.18637/jss.v084.i06)

## Examples

``` r
data(World, World_rivers, metro)

# just the map
qtm(World)
#> [tip] Consider a suitable map projection, e.g. by adding `+ tm_crs("auto")`.
#> This message is displayed once per session.


# choropleth
qtm(World, fill = "economy", style = "cobalt", crs = "+proj=eck4")


qtm(World, col = NULL) +
qtm(metro, size = "pop2010",
  size.legend = tm_legend("Metropolitan Areas"))


# dot map
if (FALSE) { # \dontrun{
current.mode <- tmap_mode("view")
qtm(metro, bbox = "China")
tmap_mode(current.mode) # restore mode
} # }

if (FALSE) { # \dontrun{
# without arguments, a plain interactive map is shown (the mode is set to view)
qtm()

# search query for OpenStreetMap nominatim
qtm("Amsterdam")
} # }
```
