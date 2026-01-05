# Changelog

## tmap 4.2.0.9000 (dev version)

## tmap 4.2

CRAN release: 2025-09-10

- \[!\] improved legend (labeling
  [\#1174](https://github.com/r-tmap/tmap/issues/1174) and
  continuous-style intervals
  [\#1175](https://github.com/r-tmap/tmap/issues/1175))
- \[!\] added tm_insets: inset maps, ggplot2 plots etc. excepted
- \[!\] added tm_minimap for plot mode
- improved animations
  ([\#1160](https://github.com/r-tmap/tmap/issues/1160))
- tmap_overview added, which provides an overview of all map elements
  (layers and components)
- tm_layout arguments limited to the useful ones
- tm_components (was tm_comp_group) improved: now also applicable to map
  component functions
- bbox added to tm_crs. Special value is “FULL” which refers to the
  whole earth

## tmap 4.1

CRAN release: 2025-05-12

This fixes a few regressions in the tmap v4 release and adds
improvements.

- \[!\] basemaps in plot mode much sharper, added
  [`tmap_providers()`](https://r-tmap.github.io/tmap/reference/tmap_providers.md)
  ([\#1041](https://github.com/r-tmap/tmap/issues/1041),
  [\#1042](https://github.com/r-tmap/tmap/issues/1042))
- \[!\] tm_comp_group() added to specify grouped components (incl
  legends)
- \[!\] added tm_animate() (main use case: cartograms)
- for the background and frame we introduce: `bg`/`bg.color` and
  `frame`/`frame.color`
- [`tm_remove_layer()`](https://r-tmap.github.io/tmap/reference/renderTmap.md)
  works again in Shiny
  ([\#1044](https://github.com/r-tmap/tmap/issues/1044))
- [`tm_logo()`](https://r-tmap.github.io/tmap/reference/tm_logo.md)
  works again in view mode
  ([\#1038](https://github.com/r-tmap/tmap/issues/1038)).
- few fixes in legend computing and size
  ([\#1039](https://github.com/r-tmap/tmap/issues/1039),
  [\#1032](https://github.com/r-tmap/tmap/issues/1032))

## tmap 4.0

CRAN release: 2025-01-27

tmap v4 is a major release and has been rewritten from scratch. It
contains tons of new features. Although we did our best to make changes
backwards compatible, some things may not work as expected.

#### New syntax

- The arguments of layer functions such as
  [`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md)
  have been reorganized. [Visual
  variables](https://r-tmap.github.io/tmap/articles/01_basics_vv.html)
  are used with explicit
  [scales](https://r-tmap.github.io/tmap/articles/basics_scales),
  [legends](https://r-tmap.github.io/tmap/articles/basics_legends), and
  [charts](https://r-tmap.github.io/tmap/articles/basics_charts).
- The names of visual variables are consistent across standard map layer
  functions. For vector data, `fill` is the visual variable for fill
  color and `col` for border color.
- New [vignettes](https://r-tmap.github.io/tmap/index.html) available to
  explain how to upgrade your code to tmap v4.

#### Updated datasets

- `rivers` has been renamed to `World_rivers` due to the name clash with
  `rivers` from `datasets`.
- The Netherlands datasets have been updated: from 2022 `NLD_prov`,
  `NLD_muni`, and (new) `NLD_dist` (district level) have been included
  with new demographic variables.
- `land` has included color tables

#### Extensions

- tmap can now be
  [extended](https://r-tmap.github.io/tmap/articles/adv_extensions) in
  several ways.

#### Backwards compatibility

- All tmap v3 code should be backwards compatible with v4.
- We added ‘v3’ styles to make the layout (almost) identical to v3
- We introduced messages to make it easier for you to upgrade your code

## tmap 3.3-4

CRAN release: 2023-09-12

- (!) last version of tmap 3.x. Next CRAN version will be tmap 4.x
- fixed bug (some stars appeared upside down in plot mode)
- fixed newly introduced shiny bug
  ([\#767](https://github.com/r-tmap/tmap/issues/767))

## tmap 3.3-3

CRAN release: 2022-03-02

- added device to
  [`tmap_save()`](https://r-tmap.github.io/tmap/reference/tmap_save.md)
- fixed many small bugs (see GitHub issue list)

## tmap 3.3-2

CRAN release: 2021-06-16

- `World`s has been fixed (it is now ‘s2’ proof, see GitHub issue
  [\#564](https://github.com/r-tmap/tmap/issues/564))

## tmap 3.3-1

CRAN release: 2021-03-15

- fixed view titles
- added html.escape to `popup.format` to control whether html code is
  escaped in view popups
- fixed many small bugs (see GitHub issue list)

## tmap 3.3

CRAN release: 2021-01-19

- [`tmap_grob()`](https://r-tmap.github.io/tmap/reference/tmap_leaflet.md)
  added, which exports tmap plots to `grob` objects (grid package)
- `SpatRaster` objects (terra package) supported
- fixed many small bugs (see GitHub issue list)

## tmap 3.2

CRAN release: 2020-09-15

- [`tmap_animation()`](https://r-tmap.github.io/tmap/reference/tmap_animation.md)
  improved: now using av and gifski under the hood; added progress bar;
  lists of tmap objects supported; improved default settings
- improved
  [`tmapOutput()`](https://r-tmap.github.io/tmap/reference/renderTmap.md);
  it now works with reactive shiny objects
- improved internally used margins; also small exported maps look good
  (see example
  [`tmap_save()`](https://r-tmap.github.io/tmap/reference/tmap_save.md))
- improved
  [`tmap_design_mode()`](https://r-tmap.github.io/tmap/reference/tmap_design_mode.md);
  `tmap.design.mode` is now a global option (and not a tmap option
  anymore)
- reexported providers from `leafet`
- added `show.warnings` to
  [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md)
- added `width` argument to
  [`tm_credits()`](https://r-tmap.github.io/tmap/reference/tm_credits.md)
- [`tm_text()`](https://r-tmap.github.io/tmap/reference/tm_text.md)
  improved: added “id” argument, and
  [`tm_remove_layer()`](https://r-tmap.github.io/tmap/reference/renderTmap.md)
  can be applied to it

## tmap 3.1

CRAN release: 2020-07-09

- interactive maps in origin CRS working:
  `tmap_options(projection = 0, basemaps = NULL)`
- added
  [`tm_mouse_coordinates()`](https://r-tmap.github.io/tmap/reference/tm_mouse_coordinates.md)
  to show mouse coordinates in view mode
- added
  [`tmap_design_mode()`](https://r-tmap.github.io/tmap/reference/tmap_design_mode.md)
  to toggle the design mode.
- made background symbol grob shapes transparent
- added in.iframe and `selfcontained` to
  [`tmap_save()`](https://r-tmap.github.io/tmap/reference/tmap_save.md)
- improved
  [`tm_add_legend()`](https://r-tmap.github.io/tmap/reference/tm_add_legend.md):
  added `type = "title"` for title only legend elements
- added [`ttmp()`](https://r-tmap.github.io/tmap/reference/tmap_mode.md)
  which shows the last map in the other mode.
- fixed many small bugs (see GitHub issue list)

## tmap 3.0

CRAN release: 2020-04-09

- \[!\] spatial rasters/arrays now implemented using the stars package
  (instead of the raster package)
- `as.count` argument added to layer functions; numeric variables can be
  treated as count variables (integers)
- drop.levels argument added to layer functions which allows to drop
  unused levels
- new style added for color classes: “dpih” and “headtails”; also added
  style.args to pass on arguments to
  [`classInt::classIntervals()`](https://r-spatial.github.io/classInt/reference/classIntervals.html)
- grid labels specification per axis
- fixed bug with geometry collection

## tmap 2.3-2

CRAN release: 2020-01-19

- probably last version before release of tmap 3.0
- many small improvements and bug fixes (see GitHub)
- first stars supported (full support expected in tmap 3.0)
- replaced projection shortcuts with `"epsg/proj4strings"` in examples

## tmap 2.3-1

CRAN release: 2019-09-17

- improved popups in view mode
- added validity checks for `sf` objects
- minor bugs fixed: see GitHub issue list

## tmap 2.3

CRAN release: 2019-07-18

- \[!\] shiny integration: added
  [`tmapOutput()`](https://r-tmap.github.io/tmap/reference/renderTmap.md),
  [`renderTmap()`](https://r-tmap.github.io/tmap/reference/renderTmap.md),
  [`tmapProxy()`](https://r-tmap.github.io/tmap/reference/renderTmap.md),
  and,
  [`tm_remove_layer()`](https://r-tmap.github.io/tmap/reference/renderTmap.md)
- improved
  [`tmap_save()`](https://r-tmap.github.io/tmap/reference/tmap_save.md)
  (regarding default values)
- improved
  [`tm_rgb()`](https://r-tmap.github.io/tmap/reference/tm_rgb.md) (added
  r g b and max.value arguments)
- added
  [`tm_graticules()`](https://r-tmap.github.io/tmap/reference/tm_grid.md)
- supported TMS servers
- in view mode, layers can be hidden from the legend with `group = NULL`
- replaced `mapview` by `leafsync`
- minor bugs fixed: see GitHub issue list

## tmap 2.2

CRAN release: 2019-01-05

- improved
  [`tm_rgb()`](https://r-tmap.github.io/tmap/reference/tm_rgb.md)
- improved breaks in interactive maps
- added `bbox` argument to
  [`qtm()`](https://r-tmap.github.io/tmap/reference/qtm.md)
- improved projection shortcut codes (using EPSG database)
- fixed several bugs (see GitHub)

## tmap 2.1-1

CRAN release: 2018-08-09

- fixed two small bugs

## tmap 2.1

CRAN release: 2018-08-06

- added option max.raster (maximum raster size)
- made rasters in view mode faster
- bug fixed for geometry collection objects without data in view mode
- improved handling of manual palettes
- [`tmap_arrange()`](https://r-tmap.github.io/tmap/reference/tmap_arrange.md)
  can take a list of tmap objects

## tmap 2.0

CRAN release: 2018-07-15

- \[!\] tmap migrated from sp to sf. sp objects are still supported, but
  all internal functions are based on sf
- \[!\] added
  [`tm_sf()`](https://r-tmap.github.io/tmap/reference/tm_sf.md) which
  draws sf objects
- \[!\] renamed all `*_tmap()` functions (e.g. `save_tmap()`) to
  `tmap_*()` for consistency
- \[!\] added
  [`tm_basemap()`](https://r-tmap.github.io/tmap/reference/tm_basemap.md)
  and
  [`tm_tiles()`](https://r-tmap.github.io/tmap/reference/tm_basemap.md):
  now, multiple (overlay) tiled maps can be shown
- \[!\] rearranged tmap options, styles and formats. See
  [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md),
  [`tmap_style()`](https://r-tmap.github.io/tmap/reference/tmap_style.md),
  [`tmap_format()`](https://r-tmap.github.io/tmap/reference/tmap-deprecated.md),
  [`tm_style()`](https://r-tmap.github.io/tmap/reference/tm_layout.md)
  and
  [`tm_format()`](https://r-tmap.github.io/tmap/reference/tmap-deprecated.md)
- \[!\] added `filter` argument to
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md)
  and added `colorNULL` to layer functions
- added
  [`tm_minimap()`](https://r-tmap.github.io/tmap/reference/tm_minimap.md)
- added `remove.NA.facets` to
  [`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md)
- units data columns are supported
- [`tmap_animation()`](https://r-tmap.github.io/tmap/reference/tmap_animation.md)
  enhanced with loop options
- added `tmap_tips()`
- all changes are backwards compatible with 1.11-2
- vignettes rewritten, including a vignette that describes the changes
  of 2.0

## tmap 1.11-2

CRAN release: 2018-04-10

- added references to JSS paper (including citation)

## tmap 1.11-1

CRAN release: 2018-02-13

- fixed bug in labels argument of
  [`tm_fill()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md)
- fixed bug regarding legend symbols in facets with free scales
- fixed bug in rasters in view mode
- improved popup width in view mode

## tmap 1.11

CRAN release: 2017-11-24

- added `text.align` and `text.to.columns` to `legend.format`
- `legend.(aes).reverse` added to the layer functions
- symbol shapes can be put in a list for small multiples, and named
  according the values of the variable specified with the shape argument
- grid labels can now be rotated and formatted
- changed default values of `free.coords` and `drop.units` in
  [`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md)
- fixed bug with all NAs in view mode
- fixed bug with `dismo::gmap()` raster shapes
- fixed bug for ambiguous values for aesthetics (e.g. when “blue” is
  also a variable name)
- NOTE: this will be the last version before the major update (in which
  sf fully replaces sp)

## tmap 1.10

CRAN release: 2017-05-11

- \[!\]
  [`tm_scale_bar()`](https://r-tmap.github.io/tmap/reference/tm_scale_bar.md),
  [`tm_text()`](https://r-tmap.github.io/tmap/reference/tm_text.md), and
  [`tm_grid()`](https://r-tmap.github.io/tmap/reference/tm_grid.md) now
  supported in view mode (requires leaflet 1.1.0)
- added `along` argument to
  [`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md),
  which enables faceting along multiple pages (or animation frames)
- added `main.title` argument to tm_layout
- added
  [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md),
  including the new option `tmap.unit` (“metric” or “imperial”“)
- improved automatic labelling of breaks
- improved legend in view mode: missing values are shown by default
  (like in plot mode)
- improved bubble scaling in view mode
- fixed bug aspect ratio bug

## tmap 1.8-1

CRAN release: 2017-01-29

- added [`tm_rgb()`](https://r-tmap.github.io/tmap/reference/tm_rgb.md)
  (shortcut for rgb rasters)
- fixed interactive maps in R Markdown
- fixed bug in [`qtm()`](https://r-tmap.github.io/tmap/reference/qtm.md)
  called without arguments

## tmap 1.8

CRAN release: 2017-01-03

- \[!\] package split: non-plotting functions migrated to tmaptools
- added
  [`tmap_arrange()`](https://r-tmap.github.io/tmap/reference/tmap_arrange.md)
  for arranging custom small multiples
- added `simplify` argument to
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md)
- added error message for NA-projected shapes in view mode
- improved unit handling, which now can be set to metric or imperial
- NA values allowed in direct color variables
- sf (simple features) objects supported

## tmap 1.6-1

CRAN release: 2016-10-29

- improved projections: code is more efficient now and shortcuts are
  renewed. Also, CRS objects are now supported for all project arguments
  (e.g. [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md),
  set_projection), and as.CRS is added to get_projection and get_proj4.
- Rscript works: methods is moved from imports to depends

## tmap 1.6

CRAN release: 2016-10-21

- \[!\]
  [`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md)
  added, to create proportional symbol maps: besides bubbles, it also
  supports other symbol shapes, png icons, and small ggplot2 plots
- \[!\] imported cartogram function from cartogram package (thanks
  Sebastian!)
- \[!\] reverse geocoding function
  [`tmaptools::rev_geocode_OSM()`](https://r-tmap.github.io/tmaptools/reference/rev_geocode_OSM.html)
  added
- \[!\]
  [`tm_logo()`](https://r-tmap.github.io/tmap/reference/tm_logo.md)
  added
- added `popup.vars` to base layer functions
- bounding box (argument `bbox` in
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md))
  working in view mode
- [`tmaptools::geocode_OSM()`](https://r-tmap.github.io/tmaptools/reference/geocode_OSM.html)
  improved: a `SpatialPointsDataFrame` can now be returned
- `last_tmap()` added, which retrieves the last produced map
- crop_shape can also handle polygons as cropping area
- append_data improved; the under and over coverage information can be
  retrieved with under_coverage and over_coverage
- interpolate parameter added to
  [`tm_raster()`](https://r-tmap.github.io/tmap/reference/tm_raster.md)
- added support for custom legend formatting functions
- legend items can now be stacked horizontally
- legend width and height can be determined fully manually (with
  negative values for `legend.height` and `legend.width`)
- argument `interval.closure` added to layer functions to determine
  where intervals are closed
- added warnings for non-supported elements in view mode
- fixed raster brick bug
- fixed save_tmap/get_asp_ratifo bug

## tmap 1.4-1

CRAN release: 2016-05-07

- add `just` argument as anchor for text labels, legend and attributes
  position
- map attributes (such as scale bar) can be placed outside the frame
- added
  [`tm_xlab()`](https://r-tmap.github.io/tmap/reference/tm_xlab.md) and
  [`tm_ylab()`](https://r-tmap.github.io/tmap/reference/tm_xlab.md)
- RGB raster support, see tm_raster
- static text (i.e titles, credits, legend titles, labels) support
  expressions
- updated Europe shape: projection and bounding box are now consistent
  with Eurostat publications
- added crop_shape, a convenient wrapper around
  [`raster::crop()`](https://rspatial.github.io/terra/reference/crop.html)

## tmap 1.4

CRAN release: 2016-03-18

- \[!\] interactive mapping added. Now, tmap has two modes: “plot”
  (graphics device) and “view” (interactive viewer, which is a leaflet
  widget)
- \[!\] facets (small multiples) improved: group by two variables
  possible, panel layout added, missing data can be shown separately
- added
  [`geocode_OSM()`](https://r-tmap.github.io/tmaptools/reference/geocode_OSM.html),
  a function to find coordinates (now
  [`tmaptools::geocode_OSM()`](https://r-tmap.github.io/tmaptools/reference/geocode_OSM.html))
- improved set_projection for raster objects
- added `double_line` and `offset_line`
- added insets option in `save_tmap`
- improved default settings for contrast argument regarding seq and div
  palettes
- improved automatic positions of legend and map attributes (more
  specifically, legend snaps to right-hand side without need to adjust
  legend.width, and legend and attributes position improved when double
  frame is enabled)
- improved `tm_layouts()`’s design.mode output
- `lwd` parameter added to
  [`tm_scale_bar()`](https://r-tmap.github.io/tmap/reference/tm_scale_bar.md)
  and
  [`tm_compass()`](https://r-tmap.github.io/tmap/reference/tm_compass.md)
- fixed `bb` bug

## tmap 1.2-1

- fixed bug with PROJ.4 version \< 4.9.1
- improved error messages
- rd projection reset to `"+init=epsg:28992"`, since the latter now has
  a `"+towgs"` item
- fixed bug of
  [`print.tmap()`](https://r-tmap.github.io/tmap/reference/print.tmap.md)
  returned object

## tmap 1.2

CRAN release: 2015-12-11

- added
  [`read_osm()`](https://r-tmap.github.io/tmaptools/reference/read_osm.html),
  a function to read Open Street Map data (now
  [`tmaptools::read_osm()`](https://r-tmap.github.io/tmaptools/reference/read_osm.html))
- added `bb`, a handy bounding box function
- added layout themes with tm_style and tm_format
- added automatic text labeling
- added interactive SVG, that can be opened in RStudio (see itmap)
- added map compass
  ([`tm_compass()`](https://r-tmap.github.io/tmap/reference/tm_compass.md))
- added text aesthetics `size` and `col`
- added automatic map coloring
- added earth boundary, which is useful for projected world maps `World`
- added warped grid lines (e.g. long-lat lines can be shown in projected
  maps)
- added new color scale modes (enabled with `style="cat"`, “cont”, and
  “order”)
- added `save_tmap()`
- added smooth_map, smooth_raster_cover, sample_dots, points_to_raster
  and poly_to_raster
- added read_GPX
- improved automatic layout regarding title, legend, and map attributes
- layer arguments, e.g. palette, can take multiple values for small
  multiples
- improved grid lines (labels can also be placed outside frame)
- arguments `free.coords` and `drop.shapes` of
  [`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md)
  working
- updated examples and vignette
- fixed `viewport` bug
- see [`?tmap`](https://r-tmap.github.io/tmap/reference/tmap-package.md)
  for a structured overview of all tmap functions

## tmap 1.0

CRAN release: 2015-05-28

- added
  [`tm_credits()`](https://r-tmap.github.io/tmap/reference/tm_credits.md)
  and
  [`tm_scale_bar()`](https://r-tmap.github.io/tmap/reference/tm_scale_bar.md)
- added `is.master` argument to
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md),
  that determines which shape is the master
- added `unit` and `unit.size` arguments to
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md)
  that correlates the coordinates with the desired units
- added
  [`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md),
  which is a combination of
  [`tm_fill()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md)
  and
  [`tm_borders()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md)
- fixed several small bugs

## tmap 0.8

- added
  [`tm_raster()`](https://r-tmap.github.io/tmap/reference/tm_raster.md)
- improved legend behavior: legend titles should be set in the layer
  functions (instead of in
  [`tm_layout()`](https://r-tmap.github.io/tmap/reference/tm_layout.md))
- removed
  [`crop_shape()`](https://r-tmap.github.io/tmaptools/reference/crop_shape.html),
  since
  [`raster::crop()`](https://rspatial.github.io/terra/reference/crop.html)
  does the same job, but, but faster
- out-of-scope functions migrated to spatialToolbox package, available
  on <https://github.com/mtennekes/spatialToolbox>

## tmap 0.7.1

- fixed bug in `scale` parameter (global scale set in
  [`tm_layout()`](https://r-tmap.github.io/tmap/reference/tm_layout.md))

## tmap 0.7

CRAN release: 2015-03-27

- enhanced small multiples
  ([`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md))
  with respect to scaling and free coordinate scales
- added `alpha` argument to `tm_layers()` for transparency
- added text shadow argument
- added function
  [`split()`](https://rspatial.github.io/terra/reference/split.html) to
  split a shape object and `sbind()` to combine shape objects.
- added automatic legend positioning when `legend.position = NULL`
- improved number formatting in legend
- improved `inner.margins` and `outer.margins`. Both can take one value
  (e.g. `outer.margins=0` rather than `outer.margins=c(0,0,0,0))`
- improved [`qtm()`](https://r-tmap.github.io/tmap/reference/qtm.md):
  [`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md)
  parameters working, and also fixed scale parameter bug
- improved all examples for `approx_areas`, `calc_densities`
- fixed bug regarding drawing a frame with `outer.margins=0`.
- fixed bug in plotting bubbles
- improved temp file handling in `animation_tmap()`
- added functions to get and set polygon directions, and to calculate
  the intersection ratios of polygons (intersection_shapes)

## tmap 0.6

CRAN release: 2014-07-30

- this is the the first CRAN version
- a newer version may be available on <https://github.com/r-tmap/tmap>
- to get started, see the package vignette ‘tmap in a nutshell’ and the
  help files
- if you have any questions or suggestions, please contact me (mtennekes
  at gmail dot com)
