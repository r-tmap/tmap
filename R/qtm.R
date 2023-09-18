#' Quick thematic map plot
#' 
#' Draw a thematic map quickly. This function is a convenient wrapper of the main plotting method of stacking [tmap-element()]s. Without arguments or with a search term, this functions draws an interactive map.
#' 
#' The first argument is a shape object (normally specified by [tm_shape()]). The next arguments, from `fill` to `raster`, are the aesthetics from the main layers. The remaining arguments are related to the map layout. Any argument from any main layer function, such as [tm_polygons()], can be specified (see `...`). It is also possible to stack [tmap-element()]s on a `qtm` plot. See examples.
#' 
#' By default, a scale bar is shown. This option can be set with [tmap_options()] (argument `qtm.scalebar`). A minimap is shown by default when `qtm` is called without arguments of with a search term. This option can be set with [tmap_options()] (argument `qtm.minimap`).
#' 
#' @param shp One of
#' \itemize{
#' \item shape object, which is an object from a class defined by the [`sf`][`sf::sf`] or [`stars`][stars::st_as_stars()] package. Objects from the packages `sp` and `raster` are also supported, but discouraged.
#' \item Not specified, i.e. `qtm()` is executed. In this case a plain interactive map is shown.
#' \item A OSM search string, e.g. `qtm("Amsterdam")`. In this case a plain interactive map is shown positioned according to the results of the search query (from OpenStreetMap nominatim)
#' }
#' @param fill,col,size,shape,lwd,lty,fill_alpha,col_alpha Visual variables.
#' @param by data variable name by which the data is split, or a vector of two variable names to split the data by two variables (where the first is used for the rows and the second for the columns). See also [tm_facets()]
#' @param scale numeric value that serves as the global scale parameter. All font sizes, symbol sizes, border widths, and line widths are controlled by this value. The parameters `symbols.size`, `text.size`, and `lines.lwd` can be scaled seperately with respectively `symbols.scale`, `text.scale`, and `lines.scale`. See also `...`.
#' @param title main title. For legend titles, use `X.style`, where X is the layer name (see `...`).
#' @param crs Either a [`crs`][sf::st_crs()] object or a character value (`PROJ.4` character string). By default, the projection is used that is defined in the `shp` object itself.
#' @param bbox bounding box. Arugment passed on to [tm_shape()]
#' @param basemaps name(s) of the provider or an URL of a tiled basemap. It is a shortcut to [tm_basemap()]. Set to `NULL` to disable basemaps. By default, it is set to the tmap option `basemaps`.
#' @param overlays name(s) of the provider or an URL of a tiled overlay map. It is a shortcut to [tm_tiles()].
#' @param zindex zindex
#' @param group group
#' @param group.control group.control
#' @param style Layout options (see [tm_layout()]) that define the style. See [tmap_style()] for details.
#' @param format Layout options (see [tm_layout()]) that define the format. See [tmap_format()] for details.
#' @param ... arguments passed on to the `tm_*` functions. The prefix of these arguments should be with the layer function name without `"tm_"` and a period. For instance, the palette for polygon fill color is called `fill.palette`. The following prefixes are supported: `shape.`, `fill.`, `borders.`, `polygons.`, `symbols.`, `dots.`, `lines.`, `raster.`, `text.`, `layout.`, `grid.`, `facets.`, and `view.`. Arguments that have a unique name, i.e. that does not exist in any other layer function, e.g. `convert2density`, can also be called without prefix.
#' @return [tmap-element()]
#' @example ./examples/qtm.R
#' @seealso [`vignette("tmap-getstarted")`](../doc/tmap-getstarted.html)
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \doi{10.18637/jss.v084.i06}
#' @export
qtm <- function(shp, 
				fill = tm_const(),
				col = tm_const(),
				size = tm_const(),
				shape = tm_const(),
				lwd = tm_const(),
				lty = tm_const(),
				fill_alpha = tm_const(),
				col_alpha = tm_const(),
				by = NULL,
				scale = NULL,
				title = NULL,
				crs = NULL,
				bbox = NULL,
				basemaps = NULL,
				overlays = NULL,
				zindex = NA,
				group = NA,
				group.control = "check",
				style=NULL,
				format=NULL,
				...) {
	args <- list(...)
	shp_name <- deparse(substitute(shp))[1]
	called <- names(match.call(expand.dots = TRUE)[-1])
	
	tmapOptions <- tmap_options_mode()
	show.warnings = tmapOptions$show.warnings
	
	if (missing(shp) || is.character(shp)) {
		
		viewargs <- args[intersect(names(args), names(formals(tm_view)))]
		
		if (!missing(shp)) viewargs$bbox <- shp
		
		g <- c(tm_basemap(basemaps), tm_tiles(overlays), do.call("tm_view", viewargs))
		attr(g, "qtm_shortcut") <- TRUE
		class(g) <- "tmap"
		return(g)
	}

	
	lst = c(list(size = size, 
				 fill = fill, 
				 col = col,
				 shape = shape,
				 lwd = lwd,
				 lty = lty,
				 fill_alpha = fill_alpha,
				 col_alpha = col_alpha,
				 zindex = zindex,
				 group = group,
				 group.control = group.control), args)
	
	# if shape is specified at tm_sf, symbols are drawn instead of dots
	if (!"shape" %in% called) lst$shape = NULL
	
	g = tm_shape(shp) + do.call(tm_sf, lst)
	
	assign("last_map_new", match.call(), envir = .TMAP)
	attr(g, "qtm_shortcut") <- FALSE
	g
}
