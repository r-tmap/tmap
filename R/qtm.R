#' Quick thematic map plot
#' 
#' Draw a thematic map quickly. This function is a convenient wrapper of the main plotting method of stacking \code{\link{tmap-element}}s. Without arguments or with a search term, this functions draws an interactive map.
#' 
#' The first argument is a shape object (normally specified by \code{\link{tm_shape}}). The next arguments, from \code{fill} to \code{raster}, are the aesthetics from the main layers. The remaining arguments are related to the map layout. Any argument from any main layer function, such as \code{\link{tm_polygons}}, can be specified (see \code{...}). It is also possible to stack \code{\link{tmap-element}}s on a \code{qtm} plot. See examples.
#' 
#' By default, a scale bar is shown. This option can be set with \code{\link{tmap_options}} (argument \code{qtm.scalebar}). A minimap is shown by default when \code{qtm} is called without arguments of with a search term. This option can be set with \code{\link{tmap_options}} (argument \code{qtm.minimap}).
#' 
#' @param shp One of
#' \itemize{
#' \item shape object, which is an object from a class defined by the \code{\link[sf:sf]{sf}} or \code{\link[stars:st_as_stars]{stars}} package. Objects from the packages \code{sp} and \code{raster} are also supported, but discouraged.
#' \item Not specified, i.e. \code{qtm()} is executed. In this case a plain interactive map is shown.
#' \item A OSM search string, e.g. \code{qtm("Amsterdam")}. In this case a plain interactive map is shown positioned according to the results of the search query (from OpenStreetMap nominatim)
#' }
#' @param fill either a color to fill the polygons, or name of the data variable in \code{shp} to draw a choropleth. Only applicable when \code{shp} contains polygons.  Set \code{fill = NULL} to draw only polygon borders. See also argument \code{borders}.
#' @param symbols.size either the size of the symbols or a name of the data variable in \code{shp} that specifies the sizes of the symbols.  See also the \code{size} argument of \code{\link{tm_symbols}}. Only applicable when \code{shp} contains spatial points, lines, or polygons.
#' @param symbols.col either the color of the symbols or a name of the data variable in \code{shp} that specifies the colors of the symbols. See also the \code{col} arugment of \code{\link{tm_symbols}}. Only applicable when \code{shp} contains spatial points, lines, or polygons.
#' @param symbols.shape  either the shape of the symbols or a name of the data variable in \code{shp} that specifies the shapes of the symbols. See also the \code{shape} arugment of \code{\link{tm_symbols}}. Only applicable when \code{shp} contains spatial points, lines, or polygons.
#' @param dots.col name of the data variable in \code{shp} for the dot map that specifies the colors of the dots. If \code{dots.col} is specified instead \code{symbols.col}, dots instead of bubbles are drawn (unless \code{symbols.shape} is specified).
#' @param text Name of the data variable that contains the text labels. Only applicable when \code{shp} contains spatial points, lines, or polygons.
#' @param text.size Font size of the text labels. Either a constant value, or the name of a numeric data variable. Only applicable when \code{shp} contains spatial points, lines, or polygons.
#' @param text.col name of the data variable in \code{shp} for the that specifies the colors of the text labels. Only applicable when \code{shp} contains spatial points, lines, or polygons.
#' @param lines.lwd either a line width or a name of the data variable that specifies the line width. Only applicable when \code{shp} contains spatial lines.
#' @param lines.col either a line color or a name of the data variable that specifies the line colors. Only applicable when \code{shp} contains spatial lines.
#' @param raster either a color or a name of the data variable that specifices the raster colors. Only applicable when \code{shp} is a spatial raster.
#' @param borders color of the polygon borders. Use \code{NULL} to omit the borders.
#' @param by data variable name by which the data is split, or a vector of two variable names to split the data by two variables (where the first is used for the rows and the second for the columns). See also \code{\link{tm_facets}}
#' @param scale numeric value that serves as the global scale parameter. All font sizes, symbol sizes, border widths, and line widths are controlled by this value. The parameters \code{symbols.size}, \code{text.size}, and \code{lines.lwd} can be scaled seperately with respectively \code{symbols.scale}, \code{text.scale}, and \code{lines.scale}. See also \code{...}.
#' @param title main title. For legend titles, use \code{X.style}, where X is the layer name (see \code{...}).
#' @param crs Either a \code{\link[sf:st_crs]{crs}} object or a character value (\code{PROJ.4} character string). By default, the projection is used that is defined in the \code{shp} object itself.
#' @param bbox bounding box. Arugment passed on to \code{\link{tm_shape}}
#' @param basemaps name(s) of the provider or an URL of a tiled basemap. It is a shortcut to \code{\link{tm_basemap}}. Set to \code{NULL} to disable basemaps. By default, it is set to the tmap option \code{basemaps}.
#' @param overlays name(s) of the provider or an URL of a tiled overlay map. It is a shortcut to \code{\link{tm_tiles}}.
#' @param style Layout options (see \code{\link{tm_layout}}) that define the style. See \code{\link{tmap_style}} for details.
#' @param format Layout options (see \code{\link{tm_layout}}) that define the format. See \code{\link{tmap_format}} for details.
#' @param ... arguments passed on to the \code{tm_*} functions. The prefix of these arguments should be with the layer function name without \code{"tm_"} and a period. For instance, the palette for polygon fill color is called \code{fill.palette}. The following prefixes are supported: \code{shape.}, \code{fill.}, \code{borders.}, \code{polygons.}, \code{symbols.}, \code{dots.}, \code{lines.}, \code{raster.}, \code{text.}, \code{layout.}, \code{grid.}, \code{facets.}, and \code{view.}. Arguments that have a unique name, i.e. that does not exist in any other layer function, e.g. \code{convert2density}, can also be called without prefix.
#' @return \code{\link{tmap-element}}
#' @example ./examples/qtm.R
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \doi{10.18637/jss.v084.i06}
#' @export
qtm <- function(shp, 
				size = tm_const(),
				fill = tm_const(),
				col = tm_const(),
				shape = tm_const(),
				lwd = tm_const(),
				lty = tm_const(),
				fill_alpha = tm_const(),
				col_alpha = tm_const(),
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
