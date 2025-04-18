# tmap 4.1 (in development)

This fixes a few regressions in the tmap v4 release and adds improvements.

- [!] basemaps in plot mode much sharper, added `tmap_providers()` (#1041, #1042)
- [!] tm_comp_group() added to specify grouped components (incl legends)
- [!] added tm_animate() (main use case: cartograms)
- for the background and frame we introduce: `bg`/`bg.color` and `frame`/`frame.color`
- `tm_remove_layer()` works again in Shiny (#1044)
- `tm_logo()` works again in view mode (#1038).
- few fixes in legend computing and size (#1039, #1032)

# tmap 4.0

tmap v4 is a major release and has been rewritten from scratch. It contains tons of new features. Although we did our best to make changes backwards compatible, some things may not work as expected.

### New syntax

* The arguments of layer functions such as `tm_symbols()` have been reorganized. [Visual variables](https://r-tmap.github.io/tmap/articles/01_basics_vv.html) are used with explicit [scales](https://r-tmap.github.io/tmap/articles/basics_scales), [legends](https://r-tmap.github.io/tmap/articles/basics_legends), and [charts](https://r-tmap.github.io/tmap/articles/basics_charts).
* The names of visual variables are consistent across standard map layer functions. For vector data, `fill` is the visual variable for fill color and `col` for border color.
* New [vignettes](https://r-tmap.github.io/tmap/index.html) available to explain how to upgrade your code to tmap v4.

### Updated datasets

* `rivers` has been renamed to `World_rivers` due to the name clash with `rivers` from `datasets`. 
* The Netherlands datasets have been updated: from 2022  `NLD_prov`, `NLD_muni`, and (new) `NLD_dist` (district level) have been included with new demographic variables.
* `land` has included color tables

### Extensions

* tmap can now be [extended](https://r-tmap.github.io/tmap/articles/adv_extensions) in several ways.

### Backwards compatibility

* All tmap v3 code should be backwards compatible with v4.
* We added 'v3' styles to make the layout (almost) identical to v3
* We introduced messages to make it easier for you to upgrade your code

# tmap 3.3-4
- (!) last version of tmap 3.x. Next CRAN version will be tmap 4.x
- fixed bug (some stars appeared upside down in plot mode)
- fixed newly introduced shiny bug (#767)

# tmap 3.3-3
- added device to `tmap_save()`
- fixed many small bugs (see GitHub issue list)

# tmap 3.3-2
- `World`s has been fixed (it is now 's2' proof, see GitHub issue #564)

# tmap 3.3-1
- fixed view titles
- added html.escape to `popup.format` to control whether html code is escaped in view popups
- fixed many small bugs (see GitHub issue list)

# tmap 3.3
- `tmap_grob()` added, which exports tmap plots to `grob` objects (grid package)
- `SpatRaster` objects (terra package) supported
- fixed many small bugs (see GitHub issue list)

# tmap 3.2
- `tmap_animation()` improved: now using av and gifski under the hood; added progress bar; lists of tmap objects supported; improved default settings
- improved `tmapOutput()`; it now works with reactive shiny objects
- improved internally used margins; also small exported maps look good (see example `tmap_save()`)
- improved `tmap_design_mode()`; `tmap.design.mode` is now a global option (and not a tmap option anymore)
- reexported providers from `leafet`
- added `show.warnings` to `tmap_options()`
- added `width` argument to `tm_credits()`
- `tm_text()` improved: added "id" argument, and `tm_remove_layer()` can be applied to it

# tmap 3.1
- interactive maps in origin CRS working: `tmap_options(projection = 0, basemaps = NULL)`
- added `tm_mouse_coordinates()` to show mouse coordinates in view mode
- added `tmap_design_mode()` to toggle the design mode.
- made background symbol grob shapes transparent
- added in.iframe and `selfcontained` to `tmap_save()`
- improved `tm_add_legend()`: added `type = "title"` for title only legend elements
- added `ttmp()` which shows the last map in the other mode. 
- fixed many small bugs (see GitHub issue list)

# tmap 3.0
- [!] spatial rasters/arrays now implemented using the stars package (instead of the raster package)
- `as.count` argument added to layer functions; numeric variables can be treated as count variables (integers)
- drop.levels argument added to layer functions which allows to drop unused levels
- new style added for color classes: "dpih" and "headtails"; also added style.args to pass on arguments to `classInt::classIntervals()`
- grid labels specification per axis
- fixed bug with geometry collection

# tmap 2.3-2
- probably last version before release of tmap 3.0
- many small improvements and bug fixes (see GitHub)
- first stars supported (full support expected in tmap 3.0)
- replaced projection shortcuts with `"epsg/proj4strings"` in examples

# tmap 2.3-1
- improved popups in view mode
- added validity checks for `sf` objects
- minor bugs fixed: see GitHub issue list

# tmap 2.3
- [!] shiny integration: added `tmapOutput()`, `renderTmap()`, `tmapProxy()`, and, `tm_remove_layer()`
- improved `tmap_save()` (regarding default values)
- improved `tm_rgb()` (added r g b and max.value arguments)
- added `tm_graticules()`
- supported TMS servers
- in view mode, layers can be hidden from the legend with `group = NULL`
- replaced `mapview` by `leafsync`
- minor bugs fixed: see GitHub issue list

# tmap 2.2
- improved `tm_rgb()`
- improved breaks in interactive maps
- added `bbox` argument to `qtm()`
- improved projection shortcut codes (using EPSG database)
- fixed several bugs (see GitHub)

# tmap 2.1-1
- fixed two small bugs

# tmap 2.1
- added option max.raster (maximum raster size)
- made rasters in view mode faster
- bug fixed for geometry collection objects without data in view mode
- improved handling of manual palettes
- `tmap_arrange()` can take a list of tmap objects

# tmap 2.0
- [!] tmap migrated from sp to sf. sp objects are still supported, but all internal functions are based on sf
- [!] added `tm_sf()` which draws sf objects
- [!] renamed all `*_tmap()` functions (e.g. `save_tmap()`) to `tmap_*()` for consistency
- [!] added `tm_basemap()` and `tm_tiles()`: now, multiple (overlay) tiled maps can be shown
- [!] rearranged tmap options, styles and formats. See `tmap_options()`, `tmap_style()`, `tmap_format()`, `tm_style()` and `tm_format()`
- [!] added `filter` argument to `tm_shape()` and added `colorNULL` to layer functions
- added `tm_minimap()`
- added `remove.NA.facets` to `tm_facets()`
- units data columns are supported
- `tmap_animation()` enhanced with loop options
- added `tmap_tips()`
- all changes are backwards compatible with 1.11-2
- vignettes rewritten, including a vignette that describes the changes of 2.0

# tmap 1.11-2
- added references to JSS paper (including citation)

# tmap 1.11-1
- fixed bug in labels argument of `tm_fill()`
- fixed bug regarding legend symbols in facets with free scales
- fixed bug in rasters in view mode
- improved popup width in view mode

# tmap 1.11
- added `text.align` and `text.to.columns` to `legend.format`
- `legend.(aes).reverse` added to the layer functions
- symbol shapes can be put in a list for small multiples, and named according the values of the variable specified with the shape argument
- grid labels can now be rotated and formatted
- changed default values of `free.coords` and `drop.units` in `tm_facets()`
- fixed bug with all NAs in view mode
- fixed bug with `dismo::gmap()` raster shapes
- fixed bug for ambiguous values for aesthetics (e.g. when "blue" is also a variable name)
- NOTE: this will be the last version before the major update (in which sf fully replaces sp)

# tmap 1.10
- [!] `tm_scale_bar()`, `tm_text()`, and `tm_grid()` now supported in view mode (requires leaflet 1.1.0)
- added `along` argument to `tm_facets()`, which enables faceting along multiple pages (or animation frames)
- added `main.title` argument to tm_layout
- added `tmap_options()`, including the new option `tmap.unit` ("metric" or "imperial"")
- improved automatic labelling of breaks
- improved legend in view mode: missing values are shown by default (like in plot mode)
- improved bubble scaling in view mode
- fixed bug aspect ratio bug

# tmap 1.8-1
- added `tm_rgb()` (shortcut for rgb rasters)
- fixed interactive maps in R Markdown
- fixed bug in `qtm()` called without arguments

# tmap 1.8
- [!] package split: non-plotting functions migrated to tmaptools
- added `tmap_arrange()` for arranging custom small multiples
- added `simplify` argument to `tm_shape()`
- added error message for NA-projected shapes in view mode
- improved unit handling, which now can be set to metric or imperial
- NA values allowed in direct color variables
- sf (simple features) objects supported

# tmap 1.6-1
- improved projections: code is more efficient now and shortcuts are renewed. Also, CRS objects are now supported for all project arguments (e.g. `tm_shape()`, set_projection), and as.CRS is added to get_projection and get_proj4.
- Rscript works: methods is moved from imports to depends

# tmap 1.6
- [!] `tm_symbols()` added, to create proportional symbol maps: besides bubbles, it also supports other symbol shapes, png icons, and small ggplot2 plots
- [!] imported cartogram function from cartogram package (thanks Sebastian!)
- [!] reverse geocoding function `tmaptools::rev_geocode_OSM()` added
- [!] `tm_logo()` added
- added `popup.vars` to base layer functions 
- bounding box (argument `bbox` in `tm_shape()`) working in view mode
- `tmaptools::geocode_OSM()` improved: a `SpatialPointsDataFrame` can now be returned
- `last_tmap()` added, which retrieves the last produced map
- crop_shape can also handle polygons as cropping area
- append_data improved; the under and over coverage information can be retrieved with under_coverage and over_coverage
- interpolate parameter added to `tm_raster()`
- added support for custom legend formatting functions
- legend items can now be stacked horizontally
- legend width and height can be determined fully manually (with negative values for `legend.height` and `legend.width`)
- argument `interval.closure` added to layer functions to determine where intervals are closed
- added warnings for non-supported elements in view mode
- fixed raster brick bug
- fixed save_tmap/get_asp_ratifo bug

# tmap 1.4-1
- add `just` argument as anchor for text labels, legend and attributes position
- map attributes (such as scale bar) can be placed outside the frame
- added `tm_xlab()` and `tm_ylab()`
- RGB raster support, see tm_raster
- static text (i.e titles, credits, legend titles, labels) support expressions
- updated Europe shape: projection and bounding box are now consistent with Eurostat publications
- added crop_shape, a convenient wrapper around `raster::crop()`

# tmap 1.4
- [!] interactive mapping added. Now, tmap has two modes: "plot" (graphics device) and "view" (interactive viewer, which is a leaflet widget)
- [!] facets (small multiples) improved: group by two variables possible, panel layout added, missing data can be shown separately
- added `geocode_OSM()`, a function to find coordinates (now `tmaptools::geocode_OSM()`)
- improved set_projection for raster objects
- added `double_line` and `offset_line`
- added insets option in `save_tmap`
- improved default settings for contrast argument regarding seq and div palettes
- improved automatic positions of legend and map attributes (more specifically, legend snaps to right-hand side without need to adjust legend.width, and legend and attributes position improved when double frame is enabled)
- improved `tm_layouts()`'s design.mode output
- `lwd` parameter added to `tm_scale_bar()` and `tm_compass()`
- fixed `bb` bug

# tmap 1.2-1
- fixed bug with PROJ.4 version < 4.9.1
- improved error messages
- rd projection reset to `"+init=epsg:28992"`, since the latter now has a `"+towgs"` item
- fixed bug of `print.tmap()` returned object

# tmap 1.2
- added `read_osm()`, a function to read Open Street Map data (now `tmaptools::read_osm()`)
- added `bb`, a handy bounding box function
- added layout themes with tm_style and tm_format
- added automatic text labeling
- added interactive SVG, that can be opened in RStudio (see itmap)
- added map compass (`tm_compass()`)
- added text aesthetics `size` and `col`
- added automatic map coloring
- added earth boundary, which is useful for projected world maps `World`
- added warped grid lines (e.g. long-lat lines can be shown in projected maps)
- added new color scale modes (enabled with `style="cat"`, "cont", and "order")
- added `save_tmap()`
- added smooth_map, smooth_raster_cover, sample_dots, points_to_raster and poly_to_raster
- added read_GPX
- improved automatic layout regarding title, legend, and map attributes
- layer arguments, e.g. palette, can take multiple values for small multiples
- improved grid lines (labels can also be placed outside frame)
- arguments `free.coords` and `drop.shapes` of `tm_facets()` working
- updated examples and vignette
- fixed `viewport` bug
- see `?tmap` for a structured overview of all tmap functions

# tmap 1.0
- added `tm_credits()` and `tm_scale_bar()`
- added `is.master` argument to `tm_shape()`, that determines which shape is the master
- added `unit` and `unit.size` arguments to `tm_shape()` that correlates the coordinates with the desired units
- added `tm_polygons()`, which is a combination of `tm_fill()` and `tm_borders()`
- fixed several small bugs

# tmap 0.8
- added `tm_raster()`
- improved legend behavior: legend titles should be set in the layer functions (instead of in `tm_layout()`)
- removed `crop_shape()`, since `raster::crop()` does the same job, but, but faster
- out-of-scope functions migrated to spatialToolbox package, available on https://github.com/mtennekes/spatialToolbox

# tmap 0.7.1
- fixed bug in `scale` parameter (global scale set in `tm_layout()`)

# tmap 0.7
- enhanced small multiples (`tm_facets()`) with respect to scaling and free coordinate scales 
- added `alpha` argument to `tm_layers()` for transparency
- added text shadow argument
- added function `split()` to split a shape object and `sbind()` to combine shape objects.
- added automatic legend positioning when `legend.position = NULL`
- improved number formatting in legend
- improved `inner.margins` and `outer.margins`. Both can take one value (e.g. `outer.margins=0` rather than `outer.margins=c(0,0,0,0))`
- improved `qtm()`: `tm_facets()` parameters working, and also fixed scale parameter bug
- improved all examples for `approx_areas`, `calc_densities`
- fixed bug regarding drawing a frame with `outer.margins=0`. 
- fixed bug in plotting bubbles
- improved temp file handling in `animation_tmap()`
- added functions to get and set polygon directions, and to calculate the intersection ratios of polygons (intersection_shapes)

# tmap 0.6
- this is the the first CRAN version
- a newer version may be available on https://github.com/r-tmap/tmap
- to get started, see the package vignette 'tmap in a nutshell' and the help files
- if you have any questions or suggestions, please contact me (mtennekes at gmail dot com)
