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
#' \item shape object, which is an object from a class defined by the \code{\link[sf:sf]{sf}}, \code{\link[sp:sp]{sp}}, or \code{\link[raster:raster-package]{raster}} package. For instance, an \code{\link[sf:sf]{sf}} object, an \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}, or a \code{\link[raster:Raster-class]{RasterBrick}}.
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
#' @param projection Either a \code{\link[sf:st_crs]{crs}} object or a character value. If it is a character, it can either be a \code{PROJ.4} character string or a shortcut. See \code{\link[tmaptools:get_proj4]{get_proj4}} for a list of shortcut values. By default, the projection is used that is defined in the \code{shp} object itself, which can be obtained with \code{\link[tmaptools:get_projection]{get_projection}}.
#' @param bbox bounding box. Arugment passed on to \code{\link{tm_shape}}
#' @param basemaps name(s) of the provider or an URL of a tiled basemap. It is a shortcut to \code{\link{tm_basemap}}. Set to \code{NULL} to disable basemaps. By default, it is set to the tmap option \code{basemaps}.
#' @param overlays name(s) of the provider or an URL of a tiled overlay map. It is a shortcut to \code{\link{tm_tiles}}.
#' @param style Layout options (see \code{\link{tm_layout}}) that define the style. See \code{\link{tmap_style}} for details.
#' @param format Layout options (see \code{\link{tm_layout}}) that define the format. See \code{\link{tmap_format}} for details.
#' @param ... arguments passed on to the \code{tm_*} functions. The prefix of these arguments should be with the layer function name without \code{"tm_"} and a period. For instance, the palette for polygon fill color is called \code{fill.palette}. The following prefixes are supported: \code{shape.}, \code{fill.}, \code{borders.}, \code{polygons.}, \code{symbols.}, \code{dots.}, \code{lines.}, \code{raster.}, \code{text.}, \code{layout.}, \code{grid.}, \code{facets.}, and \code{view.}. Arguments that have a unique name, i.e. that does not exist in any other layer function, e.g. \code{convert2density}, can also be called without prefix.
#' @return \code{\link{tmap-element}}
#' @example ./examples/qtm.R
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
#' @export
qtm <- function(shp, 
				fill=NA,
				symbols.size=NULL,
				symbols.col=NULL,
				symbols.shape=NULL,
				dots.col=NULL,
				text=NULL,
				text.size=1,
				text.col=NA,
				lines.lwd=NULL,
				lines.col=NULL,
				raster=NA,
				borders=NA,
				by=NULL,
				scale=NA,
				title=NA,
				projection=NULL,
				bbox = NULL,
				basemaps = NA,
				overlays = NA,
				style=NULL,
				format=NULL,
				...) {
	args <- list(...)
	shp_name <- deparse(substitute(shp))[1]
	called <- names(match.call(expand.dots = TRUE)[-1])

	if (missing(shp) || is.character(shp)) {
		
		viewargs <- args[intersect(names(args), names(formals(tm_view)))]
		
		if (!missing(shp)) viewargs$bbox <- shp
		
		g <- c(tm_basemap(basemaps), tm_tiles(overlays), do.call("tm_view", viewargs))
		attr(g, "qtm_shortcut") <- TRUE
		class(g) <- "tmap"
		return(g)
	} else if (inherits(shp, "Spatial") && !(inherits(shp, c("Raster", "SpatialPixels", "SpatialGrid")))) {
		shp <- as(shp, "sf")
	}
	
	if ("bubble.size" %in% names(args)) {
		warning("bubble.size is deprecated. Please use symbols.size instead", call.=FALSE)
	}
	if ("bubble.col" %in% names(args)) {
		warning("bubble.col is deprecated. Please use symbols.col instead", call.=FALSE)
	}
	
	
	isRaster <- (inherits(shp, c("SpatialGrid", "SpatialPixels", "Raster")))
	
	if (isRaster) {
		fill <- NULL
		borders <- NULL
		showPoints <- FALSE
	} else {
		
		
		if (any(st_geometry_type(shp) == "GEOMETRYCOLLECTION")) {
			geom <- split_geometry_collection(st_geometry(shp))
			shp <- shp[attr(geom, "ids"), ]
			shp <- st_set_geometry(shp, geom)
		}
		
		types <- get_types(st_geometry(shp))
		
		raster <- NULL
		hasPolys <- any(types == "polygons")
		hasLines <- any(types == "lines")
		hasPoints <- any(types == "points")
		
		showPoints <- (hasPoints || !missing(symbols.size) || !missing(symbols.shape) || !missing(symbols.col) || !missing(dots.col))


		if (showPoints) {
			dots_instead_of_symbols <- missing(symbols.size) && missing(symbols.shape) && missing(symbols.col)
			if (dots_instead_of_symbols) {
				symbols.size <- if ("dots.size" %in% names(args)) args$dots.size else .02
				symbols.shape <- if ("dots.shape" %in% names(args)) args$dots.shape else 16
			}
			if (missing(symbols.col) && missing(dots.col)) {
				symbols.col <- NA
			} else if (!missing(dots.col)) {
				symbols.col <- dots.col
			}
		}
		
		if (hasPolys) {
			if (!("fill" %in% called) && "kernel_density" %in% names(attributes(shp))) fill <- "level"	
		} else {
			fill <- NULL
			borders <- NULL	
		}
		
		if (hasLines) {
			isolines <- "isolines" %in% names(attributes(shp))
			if (missing(lines.lwd)) lines.lwd <- 1
			if (missing(lines.col)) lines.col <- NA
			if (missing(text) && isolines) text <- "level"
			if (missing(text.size) && isolines) text.size <- .5
			if (!"auto.placement" %in% called && isolines) args$auto.placement <- FALSE
			if (!"remove.overlap" %in% called && isolines) args$remove.overlap <- TRUE
			if (!"along.lines" %in% called && isolines) args$along.lines <- TRUE
			if (!"overwrite.lines" %in% called && isolines) args$overwrite.lines <- TRUE
		}
	}
	

	fns <- c("tm_shape", "tm_fill", "tm_borders", "tm_polygons", "tm_symbols", "tm_dots", "tm_lines", "tm_raster", "tm_text", "tm_layout", "tm_grid", "tm_facets", "tm_view")
	fns_prefix <- c("shape", "fill", "borders", "polygons", "symbols", "dots", "lines", "raster", "text", "layout", "grid", "facets", "view")
	
	argnames <- unlist(lapply(fns, function(f) names(formals(f))))
	dupl <- setdiff(unique(argnames[duplicated(argnames)]), "...")
	
	skips <- list(tm_shape=c("shp", "projection", "bbox"), tm_fill="col", tm_borders="col", tm_polygons="col", tm_symbols=c("size", "col", "shape"), tm_dots=c("size", "col", "shape"), tm_lines=c("col", "lwd"), tm_raster="raster", tm_text=c("text", "size", "col"), tm_layout="scale", tm_grid=NULL, tm_facets="by", tm_view = NULL)
	
	args2 <- mapply(function(f, pre, sk, args, dupl){
		
		# get function argument names
		fnames <- names(formals(f))
		
		if (f=="tm_dots") fnames <- c(fnames, names(formals("tm_symbols")))
		if (f=="tm_polygons") fnames <- c(fnames, names(formals("tm_fill")))
		
		lnames <- setdiff(fnames, sk)
		
		# add prefix
		lnames_pre <- paste(pre, lnames, sep=".")
		
		# which ones can be called without prefix? add prefix to them
		lunique <- setdiff(lnames, dupl)
		names_args <- names(args)
		names_args[names_args %in% lunique] <- paste(pre, names_args[names_args %in% lunique], sep=".")
		names(args) <- names_args
		
		# match
		arg <- args[intersect(names_args, lnames_pre)]
		
		# cut off the prefix
		if (length(arg)) names(arg) <- lnames[match(names(arg), lnames_pre)]
		
		arg
	}, fns, fns_prefix, skips, MoreArgs = list(args=args, dupl=dupl), SIMPLIFY=FALSE)

	g <- do.call("tm_shape", c(list(shp=shp, projection=projection, bbox = bbox), args2[["tm_shape"]]))
	g$tm_shape$shp_name <- shp_name
	

	g <- g + tm_basemap(basemaps)
	
	if (!is.null(borders)) {
		if ("border.col" %in% names(args2$tm_polygons)) {
			borders <- args2$tm_polygons$border.col
		}
		if ("border.alpha" %in% names(args2$tm_polygons)) {
			args2$tm_borders$alpha <- args2$tm_polygons$border.alpha
		}
		g <- g + do.call("tm_borders", c(list(col=borders), args2[["tm_borders"]]))
	}
	if (!is.null(fill)) {
		args2$tm_polygons[c("border.col", "border.alpha")] <- NULL
		args2$tm_fill <- c(args2$tm_fill, args2$tm_polygons)
		g <- g + do.call("tm_fill", c(list(col=fill), args2[["tm_fill"]]))
	}

	if (!missing(lines.lwd) || !missing(lines.col)) g <- g + do.call("tm_lines", c(list(lwd=lines.lwd, col=lines.col), args2[["tm_lines"]]))
	
	if (showPoints) {
		
		adots <- args2$tm_dots
		names(adots)[names(adots)=="title"] <- "title.col"
		names(adots)[names(adots)=="legend.show"] <- "legend.col.show"
		names(adots)[names(adots)=="legend.is.portrait"] <- "legend.col.is.portrait"
		names(adots)[names(adots)=="legend.z"] <- "legend.col.z"
		
		symbolLst <- c(if (!missing(symbols.size)) list(size=symbols.size) else list(),
					   if (!missing(symbols.col)) list(col=symbols.col) else list(),
					   if (!missing(symbols.shape)) list(shape=symbols.shape) else list())
		g <- g + do.call("tm_symbols", c(symbolLst, args2[["tm_symbols"]], adots))	
		if (dots_instead_of_symbols) g$tm_symbols$are.dots <- TRUE
	} 
	
	if (!missing(text)) g <- g + do.call("tm_text", c(list(text=text, size=text.size, col=text.col), args2[["tm_text"]]))

	is.OSM <- attr(shp, "is.OSM")
	is.OSM <- !is.null(is.OSM) && is.OSM
	is.RGB <- ifelse(is.OSM, TRUE, NA)
	
	if (!("interpolate" %in% names(args2[["tm_raster"]]))) args2$tm_raster <- c(args2$tm_raster, list(interpolate=is.RGB)) 
	if (!is.null(raster)) {
		g <- g + do.call("tm_raster", c(list(col=raster), args2[["tm_raster"]]))
		g$tm_raster$is.RGB <- is.RGB
	}

	
	if (length(args2[["tm_facets"]]) || !missing(by)) {
		g <- g + do.call("tm_facets", c(list(by=by), args2[["tm_facets"]]))	
	} 

	scaleLst <- if (!is.na(scale) && !is.na(title[1])) list(title=title, scale=scale) else if (!is.na(scale)) list(scale=scale) else if (!is.na(title[1])) list(title=title) else list()
	
	if (!missing(style)) {
		.tmapOptions <- get(".tmapOptions", envir = .TMAP_CACHE)	
		check_style(style)
		g <- g + tm_style(style)
	}
	
	if (!missing(format)) {
		.tmapFormats <- get(".tmapFormats", envir = .TMAP_CACHE)
		if (!(format %in% names(.tmapFormats))) stop("Unknown format. Please check tmap_format() for available formats")
		g <- g + tm_format(format)
	}
	
	g <- g + tm_tiles(overlays)
	
	glayout <- do.call("tm_layout", c(scaleLst, args2[["tm_layout"]]))
	gview <- do.call("tm_view", args2[["tm_view"]])
	g <- g + glayout + gview
	
	assign(".last_map_new", match.call(), envir = .TMAP_CACHE)
	attr(g, "qtm_shortcut") <- FALSE
	g
}
