#' Quick thematic map plot
#' 
#' Draw a thematic map quickly.
#' 
#' This function is a convenient wrapper of the main plotting method of stacking \code{\link{tmap-element}}s. The first argument is a shape object (normally specified by \code{\link{tm_shape}}). The next arguments, from \code{fill} to \code{raster}, are the aesthetics from the main layers. The remaining arguments are related to the map layout. Any argument from any main layer can be specified (see \code{...}). It is also possible to stack \code{\link{tmap-element}}s on a \code{qtm} plot. See examples.
#' 
#' For \code{format} any character value, say "xxx" can be used if the wrapper function \code{"tm_format_xxx"} exists. The same applies for the arguments \code{colors}, and \code{style}.
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
#' In \code{"view"} mode (see \code{\link{tmap_mode}}) there are two other options. 1) If omitted, an interactive map without thematic layers is opened. 2) In addition, if a character is provided, this character is used as a search query for OpenStreetMap nominatim. This will position the interactive map accordingly. Arguments of \code{\link{tm_view}}, such as \code{set.view} can be passed on directly.
#' @param fill either a color to fill the polygons, or name of the data variable in \code{shp} to draw a choropleth. Only applicable when \code{shp} is type 1 (see above).
#' @param symbol.size either the size of the symbols or a name of the data variable in \code{shp} that specifies the sizes of the symbols.  See also the \code{size} arugment of \code{\link{tm_symbols}}. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param symbol.col either the color of the symbols or a name of the data variable in \code{shp} that specifies the colors of the symbols. See also the \code{col} arugment of \code{\link{tm_symbols}}. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param symbol.shape  either the shape of the symbols or a name of the data variable in \code{shp} that specifies the shapes of the symbols. See also the \code{shape} arugment of \code{\link{tm_symbols}}. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param dot.col name of the data variable in \code{shp} for the dot map that specifies the colors of the dots. 
#' @param text Name of the data variable that contains the text labels. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param text.size Font size of the text labels. Either a constant value, or the name of a numeric data variable. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param text.col name of the data variable in \code{shp} for the that specifies the colors of the text labels. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param line.lwd either a line width or a name of the data variable that specifies the line width. Only applicable when \code{shp} is type 3 (see above).
#' @param line.col either a line color or a name of the data variable that specifies the line colors. Only applicable when \code{shp} is type 3 (see above).
#' @param raster either a color or a name of the data variable that specifices the raster colors. Only applicable when \code{shp} is type 4, 5, or 6 (see above).
#' @param borders color of the polygon borders. Use \code{NA} to omit the borders.
#' @param scale numeric value that serves as the global scale parameter. All font sizes, symbol sizes, border widths, and line widths are controled by this value. The parameters \code{symbol.size}, \code{text.size}, and \code{line.lwd} can be scaled seperately with respectively \code{symbol.scale}, \code{text.scale}, and \code{line.scale}.
#' @param title main title. For legend titles, use \code{X.style}, where X is layer name (see \code{...}).
#' @param projection character that determines the projection. Either a \code{PROJ.4} character string or a shortcut. See \code{\link{get_proj4}} for a list of shortcut values. By default, the projection is used that is defined in the \code{shp} object itself, which can be obtained with \code{\link{get_projection}}.
#' @param format \code{\link{tm_layout}} wrapper used for format. Currently available in tmap: "World", "Europe", "NLD", "World_wide", "Europe_wide", "NLD_wide". Own wrappers can be used as well (see details).
#' @param style \code{\link{tm_layout}} wrapper used for style. Available in tmap: "bw", "classic". Own wrappers can be used as well (see details).
#' @param basemaps basemaps for the view mode. See \code{\link{tm_view}}
#' @param bubble.size deprecated. Please use symbol.size.
#' @param bubble.col deprecated. Please use symbol.col.
#' @param ... arguments passed on to the \code{tm_*} functions. If an argument name is not unique for a particular \code{tm_} function, then it should be prefixed with the function name without \code{"tm_"}. For instance, \code{style} is an argument of \code{\link{tm_fill}}, \code{\link{tm_symbols}}, and \code{\link{tm_lines}}. Therefore, in order to define the \code{style} for a choropleth, its arugment name should be \code{fill.style}.  
#' @return \code{\link{tmap-element}}
#' @example ../examples/qtm.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @export
qtm <- function(shp, 
				fill=NA,
				symbol.size=NULL,
				symbol.col=NULL,
				symbol.shape=NULL,
				dot.col=NULL,
				text=NULL,
				text.size=1,
				text.col=NA,
				line.lwd=NULL,
				line.col=NULL,
				raster=NA,
				borders=NA,
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
	shp_name <- deparse(substitute(shp))
	called <- names(match.call(expand.dots = TRUE)[-1])
	
	if (missing(shp)) {
		# return minimal list required for leaflet basemap tile viewing
		#basemaps <- if (is.na(basemaps)[1]) tm_style_white()$tm_layout$basemaps else basemaps
		viewargs <- args[intersect(names(args), names(formals(tm_view)))]
		g <- c(list(tm_shortcut=list()), do.call("tm_view", c(list(basemaps=basemaps, bg.overlay.alpha=0), viewargs)))
		class(g) <- "tmap"
		return(g)
	} else if (is.character(shp)) {
		# return minimal list required for leaflet basemap tile viewing
		res <- geocode_OSM(shp)
		#basemaps <- if (is.na(basemaps)[1]) tm_style_white()$tm_layout$basemaps else basemaps
		viewargs <- args[intersect(names(args), names(formals(tm_view)))]
		g <- c(list(tm_shortcut=list(bbx=res$bbox, center=res$coords)), do.call("tm_view", c(list(basemaps=basemaps, bg.overlay.alpha=0), viewargs))) 
			
		#list(tm_shortcut=list(basemaps=basemaps, bg.overlay.alpha=0, bbx=res$bbox, center=res$coords))
		class(g) <- "tmap"
		return(g)
	}
	
	if (!missing(bubble.size)) {
		if (missing(symbol.size)) symbol.size <- bubble.size
		warning("bubble.size is deprecated. Please use symbol.size instead", call.=FALSE)
	}
	if (!missing(bubble.col)) {
		if (missing(symbol.col)) symbol.col <- bubble.col
		warning("bubble.col is deprecated. Please use symbol.col instead", call.=FALSE)
	}
	
	show_symbols <- (!inherits(shp, "SpatialPixels")) && 
		(inherits(shp, "SpatialPoints") || 
		 	!missing(symbol.size) || !missing(symbol.shape) || 
		 	!missing(symbol.col) || !missing(dot.col))
	
	if (show_symbols) {
		dots_instead_of_symbols <- missing(symbol.size) && missing(symbol.shape) && missing(symbol.col)
		if (dots_instead_of_symbols) {
			symbol.size <- .02
			symbol.shape <- 21
		}
		if (missing(symbol.col) && missing(dot.col)) {
			symbol.col <- NA
		} else if (!missing(dot.col)) {
			symbol.col <- dot.col
		}
	}
	
	
	if (inherits(shp, "SpatialPolygons")) {
		if (!("fill" %in% called) && "kernel_density" %in% names(attributes(shp))) fill <- "level"
	} else {
		fill <- NULL
		borders <- NULL
		
		if (inherits(shp, "SpatialLines")) {
			isolines <- "isolines" %in% names(attributes(shp))
			if (missing(line.lwd)) line.lwd <- 1
			if (missing(line.col)) line.col <- NA
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
	

	fns <- c("tm_shape", "tm_fill", "tm_borders", "tm_symbols", "tm_dots", "tm_lines", "tm_raster", "tm_text", "tm_layout", "tm_grid", "tm_facets", "tm_view")
	fns_prefix <- c("shape", "fill", "borders", "symbol", "dot", "line", "raster", "text", "layout", "grid", "facets", "view")
	
	argnames <- unlist(lapply(fns, function(f) names(formals(f))))
	dupl <- setdiff(unique(argnames[duplicated(argnames)]), "...")
	
	skips <- list(tm_shape=c("shp", "projection"), tm_fill="col", tm_borders="col", tm_symbols=c("size", "col", "shape"), tm_dots="col", tm_lines=c("col", "lwd"), tm_raster="raster", tm_text=c("text", "size"), tm_layout="scale", tm_grid=NULL, tm_facets=NULL, tm_view="basemaps")
	
	args2 <- mapply(function(f, pre, sk, args, dupl){
	  if (pre=="dot") {
	    lnames <- c(setdiff(names(formals(f)), sk),
	                setdiff(names(formals("tm_symbols")), c(sk, "size")))
	  } else lnames <- setdiff(names(formals(f)), sk)
		isD <- lnames %in% dupl
		lnames2 <- lnames
		lnames2[isD] <- paste(pre, lnames2[isD], sep=".")
		arg <- args[intersect(names(args), lnames2)]
		if (length(arg)) names(arg) <- lnames[match(names(arg), lnames2)]
		
		if (pre=="dot") {
		  dotnames <- names(arg)
		  rename <- dotnames %in% c("size", "title", "legend.show", "legend.is.portrait", " legend.z")
		  if (any(rename)) names(arg)[rename] <- c("size", "title.col", "legend.col.show", "legend.col.is.portrait", " legend.col.z")[match(dotnames[rename], c("size", "title", "legend.show", "legend.is.portrait", " legend.z"))]
		}
		arg
	}, fns, fns_prefix, skips, MoreArgs = list(args=args, dupl=dupl), SIMPLIFY=FALSE)

	# merge tm_dots and tm_symbols arguments
	if ("size" %in% names(args2$tm_dots)) {
		symbol.size <- args2$tm_dots$size
		args2$tm_dots$size <- NULL
	}
	args2$tm_symbols <- c(args2$tm_symbols, args2$tm_dots)
	args2$tm_dots <- NULL
	
	g <- do.call("tm_shape", c(list(shp=shp, projection=projection), args2[["tm_shape"]]))
	g$tm_shape$shp_name <- shp_name
	if (!is.null(borders)) g <- g + do.call("tm_borders", c(list(col=borders), args2[["tm_borders"]]))
	if (!is.null(fill)) g <- g + do.call("tm_fill", c(list(col=fill), args2[["tm_fill"]]))

	if (!missing(line.lwd) || !missing(line.col)) g <- g + do.call("tm_lines", c(list(lwd=line.lwd, col=line.col), args2[["tm_lines"]]))
	
	if (show_symbols) {
		symbolLst <- c(if (!missing(symbol.size)) list(size=symbol.size) else list(),
					   if (!missing(symbol.col)) list(col=symbol.col) else list())
		g <- g + do.call("tm_symbols", c(symbolLst, args2[["tm_symbols"]]))	
		if (dots_instead_of_symbols) g$tm_symbols$are.dots <- TRUE
	} 
	
	if (!missing(text)) g <- g + do.call("tm_text", c(list(text=text, size=text.size, col=text.col), args2[["tm_text"]]))

	if (!is.null(raster)) g <- g + do.call("tm_raster", c(list(col=raster), args2[["tm_raster"]]))
	
	if (length(args2[["tm_facets"]])) g <- g + do.call("tm_facets", args2[["tm_facets"]])

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
	g <- g + do.call("tm_layout", c(scaleLst, args2[["tm_layout"]]))
	g <- g + do.call("tm_view", c(list(basemaps=basemaps), args2[["tm_view"]]))
	assign(".last_map_new", match.call(), envir = .TMAP_CACHE)
	g
}