#' Quick thematic map plot
#' 
#' Draw a thematic map quickly.
#' 
#' This function is a convenient wrapper of the main plotting method of stacking \code{\link{tmap-element}}s. The first argument is a shape object (normally specified by \code{\link{tm_shape}}). The next arguments, from \code{fill} to \code{raster}, are the aesthetics from the main layers. The remaining arguments are related to the map layout. Any argument from any main layer can be specified (see \code{...}). It is also possible to stack \code{\link{tmap-element}}s on a \code{qtm} plot. See examples.
#' 
#' For \code{format}, any character value, say "xxx" can be used if the wrapper function \code{"tm_format_xxx"} exists. The same applies for the argument \code{style}.
#' 
#' @param shp shape object, which is one of
#' \enumerate{
#'  \item{\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPixelsDataFrame]{SpatialPixels(DataFrame)}}}
#'  \item{\code{\link[raster:Raster-class]{RasterLayer, RasterStack, or RasterBrick}}}
#' }
#' In \code{"view"} mode (see \code{\link{tmap_mode}}) there are two other options. 1) If omitted, an interactive map without thematic layers is opened. 2) In addition, if a character is provided, this character is used as a search query for OpenStreetMap nominatim. This will position the interactive map accordingly. Arguments of \code{\link{tm_view}}, such as \code{set.view} can be passed on directly.tm_
#' @param fill either a color to fill the polygons, or name of the data variable in \code{shp} to draw a choropleth. Only applicable when \code{shp} is type 1 (see above).  Set \code{fill=NULL} to draw only polygon borders. See also argument \code{borders}.
#' @param symbols.size either the size of the symbols or a name of the data variable in \code{shp} that specifies the sizes of the symbols.  See also the \code{size} arugment of \code{\link{tm_symbols}}. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param symbols.col either the color of the symbols or a name of the data variable in \code{shp} that specifies the colors of the symbols. See also the \code{col} arugment of \code{\link{tm_symbols}}. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param symbols.shape  either the shape of the symbols or a name of the data variable in \code{shp} that specifies the shapes of the symbols. See also the \code{shape} arugment of \code{\link{tm_symbols}}. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param dots.col name of the data variable in \code{shp} for the dot map that specifies the colors of the dots. If \code{dots.col} is specified instead \code{symbols.col}, dots instead of bubbles are drawn (unless \code{symbols.shape} is specified).
#' @param text Name of the data variable that contains the text labels. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param text.size Font size of the text labels. Either a constant value, or the name of a numeric data variable. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param text.col name of the data variable in \code{shp} for the that specifies the colors of the text labels. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param lines.lwd either a line width or a name of the data variable that specifies the line width. Only applicable when \code{shp} is type 3 (see above).
#' @param lines.col either a line color or a name of the data variable that specifies the line colors. Only applicable when \code{shp} is type 3 (see above).
#' @param raster either a color or a name of the data variable that specifices the raster colors. Only applicable when \code{shp} is type 4, 5, or 6 (see above).
#' @param borders color of the polygon borders. Use \code{NULL} to omit the borders.
#' @param by data variable name by which the data is split, or a vector of two variable names to split the data by two variables (where the first is used for the rows and the second for the columns). See also \code{\link{tm_facets}}
#' @param scale numeric value that serves as the global scale parameter. All font sizes, sy mbol sizes, border widths, and line widths are controled by this value. The parameters \code{symbols.size}, \code{text.size}, and \code{lines.lwd} can be scaled seperately with respectively \code{symbols.scale}, \code{text.scale}, and \code{lines.scale}. See also \code{...}.
#' @param title main title. For legend titles, use \code{X.style}, where X is the layer name (see \code{...}).
#' @param projection Either a \code{\link[sp:CRS]{CRS}} object or a character value. If it is a character, it can either be a \code{PROJ.4} character string or a shortcut. See \code{\link[tmaptools:get_proj4]{get_proj4}} for a list of shortcut values. By default, the projection is used that is defined in the \code{shp} object itself, which can be obtained with \code{\link[tmaptools:get_projection]{get_projection}}.
#' @param format \code{\link{tm_layout}} wrapper used for format. Currently available in tmap: "World", "Europe", "NLD", "World_wide", "Europe_wide", "NLD_wide". Own wrappers can be used as well (see details).
#' @param style \code{\link{tm_layout}} wrapper used for style. Available in tmap: "bw", "classic". Own wrappers can be used as well (see details).
#' @param basemaps basemaps for the view mode. See \code{\link{tm_view}}
#' @param bubble.size deprecated. Please use symbols.size.
#' @param bubble.col deprecated. Please use symbols.col.
#' @param ... arguments passed on to the \code{tm_*} functions. The prefix of these arguments should be with the layer function name without \code{"tm_"} and a period. For instance, the palette for polygon fill color is called \code{fill.palette}. The following prefixes are supported: \code{shape.}, \code{fill.}, \code{borders.}, \code{polygons.}, \code{symbols.}, \code{dots.}, \code{lines.}, \code{raster.}, \code{text.}, \code{layout.}, \code{grid.}, \code{facets.}, and \code{view.}. Arguments that have a unique name, i.e. that does not exist in any other layer function, e.g. \code{convert2density}, can also be called without prefix.
#' @return \code{\link{tmap-element}}
#' @example ./examples/qtm.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
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
				format=NULL,
				style=NULL,
				basemaps=NA,
				bubble.size=NULL,
				bubble.col=NULL,
				...) {
	args <- list(...)
	shp_name <- deparse(substitute(shp))[1]
	called <- names(match.call(expand.dots = TRUE)[-1])
	
	if (missing(shp)) {
		# return minimal list required for leaflet basemap tile viewing
		#basemaps <- if (is.na(basemaps)[1]) tm_style_white()$tm_layout$basemaps else basemaps
		viewargs <- args[intersect(names(args), names(formals(tm_view)))]
		g <- c(list(tm_shortcut=list()), do.call("tm_view", c(list(basemaps=basemaps), viewargs)))
		class(g) <- "tmap"
		return(g)
	} else if (is.character(shp)) {
		# return minimal list required for leaflet basemap tile viewing
		res <- geocode_OSM(shp)
		#basemaps <- if (is.na(basemaps)[1]) tm_style_white()$tm_layout$basemaps else basemaps
		viewargs <- args[intersect(names(args), names(formals(tm_view)))]
		g <- c(list(tm_shortcut=list(bbx=res$bbox, center=res$coords)), do.call("tm_view", c(list(basemaps=basemaps), viewargs))) 
			
		#list(tm_shortcut=list(basemaps=basemaps, bg.overlay.alpha=0, bbx=res$bbox, center=res$coords))
		class(g) <- "tmap"
		return(g)
	} else if (inherits(shp, c("sf", "sfc"))) {
		shp <- as(shp, "Spatial")
	}
	
	if (!missing(bubble.size)) {
		if (missing(symbols.size)) symbols.size <- bubble.size
		warning("bubble.size is deprecated. Please use symbols.size instead", call.=FALSE)
	}
	if (!missing(bubble.col)) {
		if (missing(symbols.col)) symbols.col <- bubble.col
		warning("bubble.col is deprecated. Please use symbols.col instead", call.=FALSE)
	}
	
	# show symbol/dot map if shape is spatial points or if symbols. ... is defined
	show_symbols <- (!inherits(shp, "SpatialPixels")) && 
		(inherits(shp, "SpatialPoints") || 
		 	!missing(symbols.size) || !missing(symbols.shape) || 
		 	!missing(symbols.col) || !missing(dots.col))
	if (show_symbols) {
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
	
	
	if (inherits(shp, "SpatialPolygons")) {
		if (!("fill" %in% called) && "kernel_density" %in% names(attributes(shp))) fill <- "level"
	} else {
		fill <- NULL
		borders <- NULL
		
		if (inherits(shp, "SpatialLines")) {
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
	if (!inherits(shp, c("SpatialGrid", "SpatialPixels", "Raster"))) {
		raster <- NULL
	}
	

	fns <- c("tm_shape", "tm_fill", "tm_borders", "tm_polygons", "tm_symbols", "tm_dots", "tm_lines", "tm_raster", "tm_text", "tm_layout", "tm_grid", "tm_facets", "tm_view")
	fns_prefix <- c("shape", "fill", "borders", "polygons", "symbols", "dots", "lines", "raster", "text", "layout", "grid", "facets", "view")
	
	argnames <- unlist(lapply(fns, function(f) names(formals(f))))
	dupl <- setdiff(unique(argnames[duplicated(argnames)]), "...")
	
	skips <- list(tm_shape=c("shp", "projection"), tm_fill="col", tm_borders="col", tm_polygons="col", tm_symbols=c("size", "col", "shape"), tm_dots=c("size", "col", "shape"), tm_lines=c("col", "lwd"), tm_raster="raster", tm_text=c("text", "size", "col"), tm_layout="scale", tm_grid=NULL, tm_facets="by", tm_view="basemaps")
	
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

	g <- do.call("tm_shape", c(list(shp=shp, projection=projection), args2[["tm_shape"]]))
	g$tm_shape$shp_name <- shp_name
	

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
	
	if (show_symbols) {
		
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
	is_raster <- !is.null(is.OSM) && is.OSM
	is.RGB <- ifelse(is_raster, TRUE, NA)
	
	if (!("interpolate" %in% names(args2[["tm_raster"]]))) args2$tm_raster <- c(args2$tm_raster, list(interpolate=is.RGB)) 
	if (!is.null(raster)) {
		g <- g + do.call("tm_raster", c(list(col=raster), args2[["tm_raster"]]))
		g$tm_raster$is.RGB <- is.RGB
	}

	
	if (length(args2[["tm_facets"]]) || !missing(by)) {
		g <- g + do.call("tm_facets", c(list(by=by), args2[["tm_facets"]]))	
	} 

	scaleLst <- if (!missing(scale)) list(title=title, scale=scale) else list(title=title)
	if (!missing(format)) {
		fname <- paste("tm_format", format, sep="_")
		if (exists(fname)) {
			g <- g + do.call(fname, list())
		} else warning("function ", fname, " does not exist", call. = FALSE)
	}
	if (!missing(style)) {
		fname <- paste("tm_style", style, sep="_")
		if (exists(fname)) {
			g <- g + do.call(fname, list())
		} else {
			warning("function ", fname, " does not exist", call. = FALSE)
		}
	}
	
	glayout <- do.call("tm_layout", c(scaleLst, args2[["tm_layout"]]))
	glayoutcall <- c(intersect(called, c("title", "scale")), names(args2[["tm_layout"]]))
	glayout$tm_layout["call"] <- list(call=if(length(glayoutcall)==0) NULL else glayoutcall)
	
	gview <- do.call("tm_view", c(list(basemaps=basemaps), args2[["tm_view"]]))
	gviewcall <- c(intersect(called, "basemaps"), names(args2[["tm_view"]]))
	gview$tm_view["call"] <- list(call=if(length(gviewcall)==0) NULL else gviewcall)
	
	g <- g + glayout + gview
	assign(".last_map_new", match.call(), envir = .TMAP_CACHE)
	g
}