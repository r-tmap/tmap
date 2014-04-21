#' Specify the shape object
#' 
#' This layer specifies the shape object, which is one of \code{\link[sp:SpatialPolygons]{SpatialPolygons}}, \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}, \code{\link[sp:SpatialPoints]{SpatialPoints}}, and \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}}.
#' 
#' @param shp shape object. For \code{\link{geo_choropleth}} and \code{\link{geo_bubblemap}}, a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} or a \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} is requied. \code{\link[sp:SpatialPoints]{SpatialPoints}} and \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} are only used for \code{\link{geo_bubblemap}} and \code{\link{geo_bubbles}}.
#' @param projection character that determines the projectino. Either a \code{PROJ.4} character string (see \url{http://trac.osgeo.org/proj/}), of one of the following shortcuts: 
#' \describe{
#'    	\item{\code{"longlat"}}{Not really a projection, but a plot of the longitude-latitude coordinates.} 
#'    	\item{\code{"wintri"}}{Winkel Tripel (1921). Popular projection that is useful in world maps. It is the standard of world maps made by the National Geographic Society. Type: compromise} 
#'    	\item{\code{"robin"}}{Robinson (1963). Another popular projection for world maps. Type: compromise}
#'    	\item{\code{"eck4"}}{Eckert IV (1906). Projection useful for world maps. Area sizes are preserved, which makes it particularly useful for truthful choropleths. Type: equal-area}
#'    	\item{\code{"hd"}}{Hobo-Dyer (2002). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"gall"}}{Gall (Peters) (1855). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"merc"}}{Mercator (1569). Projection in which shapes are locally preserved. However, areas close to the poles are inflated. Used by Google Maps. Type: conformal}
#'    	\item{\code{"mill"}}{Miller (1942). Projetion based on Mercator, in which poles are displayed. Type: compromise}
#'    	\item{\code{"eqc0"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The equator is the standard parallel. Also known as Plate Carr\'ee. Type: equidistant}
#'    	\item{\code{"eqc30"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 30 is the standard parallel. Type: equidistant}
#'    	\item{\code{"eqc45"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 45 is the standard parallel. Also known as Gall isographic. Type: equidistant}
#'    	\item{\code{"rd"}}{Rijksdriehoekstelsel. Triangulation coordinate system used in the Netherlands.}}
#'    	See \url{http://en.wikipedia.org/wiki/List_of_map_projections} for a overview of projections.
#'    	By default, the projection is used that is defined in the \code{shp} object itself.
#' @param xlim limits of the x-axis
#' @param ylim limits of the y-axis
#' @param relative boolean that determines whether relative values are used for \code{xlim} and \code{ylim} or absolute. Note: relative values will depend on the current bounding box (bbox) of the first shape object.
#' @param bbox bounding box, which is a 2x2 matrix that consists absolute \code{xlim} and \code{ylim} values. If specified, it overrides the \code{xlim} and \code{ylim} parameters.
#' @export
#' @return \code{\link{geo-object}}
geo_shape <- function(shp, 
					  projection=NULL, 
					  xlim = NULL,
					  ylim = NULL,
					  relative = TRUE,
					  bbox = NULL) {
	shp_name <- deparse(substitute(shp))
	g <- list(geo_shape=as.list(environment()))
	class(g) <- "geo"
	g
}



#' Draw polygon borders
#' 
#' This layer defines the borders of the polygons. Color, line width and line type can be set.
#' 
#' @param col line color
#' @param lwd line width (see \code{\link[graphics:par]{par}})
#' @param lty line type (see \code{\link[graphics:par]{par}})
#' @export
#' @return \code{\link{geo-object}}
geo_borders <- function(col="black", lwd=1, lty="solid") {
	g <- list(geo_borders=as.list(environment()))
	class(g) <- "geo"
	g
}

#' Fill polygons
#' 
#' This layer defines the fill colors of the polygons. The colors in this layer are directly specified. Use \code{\link{geo_choropleth}} to map colors to a data variable.
#' 
#' @param col a single color value, or a vector of colors (specifying a color per polygon).
#' @export
#' @seealso \code{\link{geo_choropleth}}
#' @return \code{\link{geo-object}}
geo_fill <- function(col="lightgray") {
	g <- list(geo_fill=as.list(environment()))
	class(g) <- "geo"
	g
}

#' Draw bubbles
#' 
#' This layer speficies the drawing of bubbles. The colors and sizes of the bubbles are directly specified in this layer. Use \code{\link{geo_bubblemap}} to map bubbles colors and/or sizes to data.
#' 
#' @param size relative sizes of the bubbles
#' @param col a single color value, or a vector of colors (specifying a color per polygon).
#' @param border color of the bubble borders. If \code{NA}, no borders are drawn.
#' @param scale scale multiplier to adjust the bubble sizes
#' @export
#' @seealso \code{\link{geo_choropleth}}
#' @return \code{\link{geo-object}}
geo_bubbles <- function(size=1, col="red", border=NA, scale=1) {
	g <- list(geo_bubbles=list(bubble.size=size, bubble.col=col, bubble.border=border, bubble.scale=scale))
	class(g) <- "geo"
	g
}

#' Add text labels
#' 
#' This layer adds text labels
#' 
#' @param text name of the variable in the shape object that contains the text labels
#' @param cex relative size of the text labels. Eiter one number, a name of a numeric variable that is used to scale the sizes proportionally, or one of the following special cases
#' \describe{
#' 		\item{\code{"AREA"}:}{text size is proportional to the squared root of the area size of the polygons.}
#' 		\item{\code{"AREAx"}:}{text size is proportional to the x-th root of the area size of the polygons.}}
#' @param fontcolor relative size of the text labels
#' @param fontface font face of the text labels
#' @param fontfamily font family of the text labels
#' @param bg.color background color of the text labels
#' @param bg.alpha number between 0 and 255 that specifies the transparancy of the text background (0 is totally transparent, 255 is solid background)
#' @param cex.lowerbound lowerbound for \code{cex}. Needed to ignore the tiny labels in case \code{cex} is a variable.
#' @param print.tiny boolean that determines if tiny labels (which size is smaller than \code{cex.lowerbound}) are print at size \code{cex.lowerbound}
#' @param scale scalar needed in case cex is based 
#' @export
#' @return \code{\link{geo-object}}
geo_text <-  function(text, cex=1, fontcolor="black", fontface="plain", fontfamily="sans", bg.color="#888888", bg.alpha=100, cex.lowerbound=.2, print.tiny=FALSE, scale=1) {
	g <- list(geo_text=list(text=text, text.cex=cex, text.fontcolor=fontcolor, text.fontface=fontface, text.fontfamily=fontfamily, text.bg.color=bg.color, text.bg.alpha=bg.alpha,
							text.cex.lowerbound=cex.lowerbound, text.print.tiny=print.tiny, text.scale=scale))
	class(g) <- "geo"
	g
}

#' Draw choropleth
#' 
#' This layer speficies a choropleth. A color palette is mapped to a data variable. By default, a divering color palette is used for numeric variables and a qualitative palette for categorical variables.
#' 
#' @param col name of the data variable that is contained in \code{shp}
#' @param palette palette name. See \code{RColorBrewer::display.brewer.all()} for options. Use a \code{"-"} as prefix to reverse the palette. By default, \code{"RdYlGn"} is taken for  numeric variables that contain both negative and positive values, \code{"Blues"} for numeric variables that contain non-negative values, and \code{"Dark2"} for categorical variables.
#' @param n preferred number of classes (in case \code{col} is a numeric variable)
#' @param convert2density boolean that determines whether \code{col} is converted to a density variable. Should be \code{TRUE} when \code{col} consists of absolute numbers. Note that the conversion to densities is an approximation where the total area size is given by the argument \code{total.area.km2}.
#' @param style method to cut the color scale (in case \code{col} is a numeric variable): "fixed", "equal", "pretty", "quantile", "kmeans"
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast number between 0 and 1 (default) that determines the contrast of the palette. Only applicable when \code{auto.palette.mapping=TRUE}
#' @param colorNA color used to missing values
#' @param total.area.km2 total area size in km2. Needed if \code{convert2density=TRUE}.
#' @export
#' @seealso \code{\link{geo_fill}}
#' @return \code{\link{geo-object}}	
geo_choropleth <- function(col, 
						    palette = NULL,
						    convert2density = FALSE,
						    n = 5,
						    style = "pretty",
							breaks = NULL,
						    labels = NULL,
							auto.palette.mapping = TRUE,
							contrast = 1,
							colorNA = "#BBBBBB",
							total.area.km2=NA) {
	
	g <- list(geo_choropleth=as.list(environment()))
	class(g) <- "geo"
	g
}	

#' Draw bubblemap
#' 
#' This layer speficies a bubblemap. Both colors and sizes of the bubbles can be mapped to data variables. 
#' @param size \code{shp} data variable that determines the bubble sizes. Multiple variable names create small multiples
#' @param col color(s) of the bubble. Either a color (vector), or categorical variable name(s). Multiple variable names create small multiples
#' @param border color of the bubble borders. If \code{NA} (default), no bubble borders are drawn.
#' @param scale bubble size multiplier number. 
#' @param n preferred number of color scale classes. Only applicable when \code{col} is a numeric variable name.
#' @param style method to cut the color scale: "fixed", "equal", "pretty", "quantile", "kmeans". Only applicable when \code{col} is a numeric variable name.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param palette color palette (see \code{RColorBrewer::display.brewer.all}) for the bubbles. Only when \code{col} is set to a variable.
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast number between 0 and 1 (default) that determines the contrast of the palette. Only applicable when \code{auto.palette.mapping=TRUE} and \code{col} is a numeric variable name. 
#' @param colorNA colour for missing values
#' @export
#' @seealso \code{\link{geo_bubblemap}}
#' @return \code{\link{geo-object}}
geo_bubblemap <- function(size = NULL, col = NULL,
						  border=NA,
						  scale=1,
						  n = 5, style = "pretty",
						  breaks = NULL,
						  palette = NULL,
						  labels = NULL,
						  auto.palette.mapping = TRUE,
						  contrast = 1,
						  colorNA = "#FF1414") {
	g <- list(geo_bubblemap=list(bubble.size=size, bubble.col=col, bubble.border=border,
								 bubble.scale=scale,
								 n=n, style=style, breaks=breaks, palette=palette, labels=labels,
								 auto.palette.mapping=auto.palette.mapping, contrast=contrast,
								 colorNA=colorNA))
	class(g) <- "geo"
	g
}


#' Small multiples grid
#' 
#' This layer specifies how the different plots are placed in a grid. The number of rows and columns can be specified here, as well as whether the scales are free (i.e. independent of each other).
#' 
#' @param ncol number of columns of the small multiples grid
#' @param nrow number of rows of the small multiples grid
#' @param free.scales logical. Should all scales of the plotted data variables be free, i.e. independent of each other? Possible data variables are color from \code{\link{geo_choropleth}} and color and size from \code{\link{geo_bubblemap}}.
#' @param free.scales.choro logical. Should the color scale for the choropleth be free?
#' @param free.scales.bubble.size logical. Should the bubble size scale for the bubblemap be free?
#' @param free.scales.bubble.col logical. Should the color scale for the bubblemap be free?
#' @export
#' @return \code{\link{geo-object}}
geo_grid <- function(ncol=NULL, nrow=NULL, 
					 free.scales=FALSE,
					 free.scales.choro=free.scales,
					 free.scales.bubble.size=free.scales,
					 free.scales.bubble.col=free.scales
					 )	{
	g <- list(geo_grid=as.list(environment()))
	class(g) <- "geo"
	g
}

#' Theme elements of cartographic maps
#' 
#' This layer specifies thematic layout options for the maps.
#' 
#' @name geo_theme
#' @rdname geo_theme
#' @param title Title of the map(s)
#' @param title.cex Relative size of the title
#' @param bg.color Background color
#' @param draw.frame Boolean that determines whether a frama is drawn. 
#' @param title.position Position of the title. Vector of two values, specifing the x and y coordinates. Either this vector contains "left", "center" or "right" for the first value and "top", "center", or "right" for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y value of the left bottom corner of the legend.
#' @param asp Aspect ratio. The aspect ratio of the map (width/height). If \code{NA}, it is determined by the bounding box (see argument \code{bbox} of \code{\link{geo_shape}}) and the argument \code{frame.margins}. If \code{0}, then the aspect ratio is adjusted to the aspect ratio of the device.
#' @param margins Relative margins. Vector of four values specifying the bottom, left, top, and right margin. Values are between 0 and 1.
#' @param frame.lwd Width of the frame
#' @param frame.margins Frame margins
#' @param legend.profile Character that specifies which legend elements are drawn (if applicable):
#' \describe{
#' 	\item{\code{"full"}:}{All of them. (Which are: choropleth text, choropleth histogram, bubble size text, and bubble color text.)}
#' 	\item{\code{"text"}:}{Only the choropleth text, bubble size text, and bubble color text.}
#' 	\item{\code{"hist"}:}{Only the choropleth histogram.}
#' 	\item{\code{"none"}:}{None of them.}}
#' Alternatively, \code{legend.config} can be used to specify the elements directly.
#' @param legend.only logical. Only draw the legend (without map)? Particularly useful for small multiples with a common legend.
#' @param legend.choro.title title of the choropleth legend
#' @param legend.bubble.size.title title of the bubblemap legend associated with the size of the bubbles
#' @param legend.bubble.col.title title of the bubblemap legend associated with the color of the bubbles
#' @param legend.position Position of the legend. Vector of two values, specifing the x and y coordinates. Either this vector contains "left", "center" or "right" for the first value and "top", "center", or "right" for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y value of the left bottom corner of the legend.
#' @param legend.width width of the legend
#' @param legend.height total maximum height of the legend. Heights of single legend elements are specified by \code{legend.choro.height}, \code{legend.choro.hist.height}, \code{legend.bubble.size.height}, and \code{legend.bubble.col.height}. If their total exceeds \code{legend.height}, then there are downscaled linearly.
#' @param legend.choro.height see \code{legend.height}
#' @param legend.choro.hist.height see \code{legend.height}
#' @param legend.bubble.size.height see \code{legend.height}
#' @param legend.bubble.col.height see \code{legend.height}
#' @param legend.config character vector that specifies which legend elements are drawn and at what position. The legend elements are called \code{"choro"}, \code{"hist"}, \code{"bubble.size"}, and \code{"bubble.col"}. The \code{legend.config} vector should only contain these elements (it can also be a subset). The order corresponds to the order in which the legend elements are stacked from top to bottom.
#' @param legend.in.frame logical. Should the legend be drawn inside the frame?
#' @param legend.title.cex Relative font size for the legend title
#' @param legend.text.cex Relative font size for the legend text elements
#' @param legend.hist.cex Relative font size for the choropleth histogram
#' @param legend.digits Number of digits for the legend labels
#' @param legend.bg.color Background color of the legend.
#' @param ... other arguments from \code{geo_theme}
#' @export
geo_theme <- function(title=NA,
					  title.cex=1.0,
					  bg.color="grey85",
					  draw.frame=FALSE,
					  title.position = c("left", "top"),
					  asp = NA,
					  frame.lwd=1,
					  outer.margins = rep(0, 4),
					  inner.margins=rep(0.02, 4),
					  legend.profile = "full",
					  legend.only = FALSE,
					  legend.choro.title = NA,
					  legend.bubble.size.title = NA,
					  legend.bubble.col.title = NA,
					  legend.position = c("left", "top"),
					  legend.width = 0.3,
					  legend.height = 0.8,
					  legend.choro.height = 0.3,
					  legend.choro.hist.height = 0.25,
					  legend.bubble.size.height = 0.15,
					  legend.bubble.col.height = 0.3,
					  legend.config = c("hist", "choro", "bubble.size", "bubble.col"),
					  legend.in.frame = TRUE,
					  legend.title.cex=1.0,
					  legend.text.cex=0.7,
					  legend.hist.cex=0.7,
					  legend.digits = 2L,
					  legend.bg.color = NA) {
	g <- list(geo_theme=as.list(environment()))
	class(g) <- "geo"
	g
}


#' @rdname geo_theme
#' @export
geo_theme_World <- function(title=NA,
							title.cex=1,
							draw.frame=TRUE, 
							title.position = c("left", "bottom"),
							asp=2,
							outer.margins=rep(.02, 4),
							inner.margins=c(0, 0.02, 0.02, 0.02),
							legend.position=c("left", "bottom"), 
							legend.width=.2,
							legend.height = .5,
							legend.bg.color="grey85",
							...) {
	args <- c(as.list(environment()), list(...))
	do.call("geo_theme", args)
}

#' @rdname geo_theme
#' @export
geo_theme_Europe <- function(title=NA,
							 draw.frame=TRUE, 
							legend.position=c("left", "top"), 
							outer.margins=rep(0.02, 4),
							inner.margins=c(0, 0.2, 0, 0),
							...) {
	args <- c(as.list(environment()), list(...))
	do.call("geo_theme", args)
}

	
#' @rdname geo_theme
#' @export
geo_theme_NLD <- function(title=NA,
						  draw.frame=FALSE, 
							legend.in.frame=FALSE, 
							legend.position=c("left", "top"), 
							legend.width=.3,
							...) {
	args <- c(as.list(environment()), list(...))
	do.call("geo_theme", args)
}


#' Stacking of geo layers
#' 
#' The plus operator allows you to stack geo layers (created by any geo_* function). Always start with geo_shape to specify the shape object (i.e. the SpatialPolygonsDataframe). If multile shape objects are used, start each group of layers with geo_shape to specify the shape object of that group.
#' 
#' @param e1 first geo layer
#' @param e2 second geo layer
#' @export
"+.geo" <- function(e1, e2) {
	g <- c(e1,e2)
	class(g) <- "geo"
	g
}

#' Quick map plot
#' 
#' This function is a convenient wrapper for drawing quick cartographic maps.
#' 
#' @param shp shape object. For \code{\link{geo_choropleth}} and \code{\link{geo_bubblemap}}, a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} or a \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} is requied. \code{\link[sp:SpatialPoints]{SpatialPoints}} and \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} are only used for \code{\link{geo_bubblemap}} and \code{\link{geo_bubbles}}.
#' @param choro.fill name of the data variable in \code{shp} for the choropleth. If not specified, no choropleth is drawn.
#' @param bubble.size name of the data variable in \code{shp} for the bubblemap that specifies the sizes of the bubbles. If neither \code{bubble.size} nor \code{bubble.col} is specified, no bubblemap is drawn.
#' @param bubble.col name of the data variable in \code{shp} for the bubblemap that specifies the colors of the bubbles. If neither \code{bubble.size} nor \code{bubble.col} is specified, no bubblemap is drawn.
#' @param borders color of the polygon borders. Use \code{NA} to omit the borders.
#' @param theme one of "World", "Europe", or "NLD"
#' @param ... parameters passed on to \code{\link{geo_shape}}, \code{\link{geo_choropleth}}, \code{\link{geo_bubblemap}}, \code{\link{geo_borders}}, \code{\link{geo_theme}} and \code{\link{geo_grid}}.
#' @return \code{\link{geo-object}}
#' @export
geo <- function(shp, 
				choro.fill=NULL,
				bubble.size=NULL,
				bubble.col=NULL,
				borders="black",
				theme=NULL,
				...) {
	args <- list(...)
	shapeargs <- args[intersect(names(args), names(geo_shape()[[1]]))]
	choroargs <- args[setdiff(intersect(names(args), names(geo_choropleth()[[1]])), "col")]
	bubbleargs <- args[setdiff(intersect(names(args), names(geo_bubblemap()[[1]])), c("col", "size"))]
	borderargs <- args[setdiff(intersect(names(args), names(geo_borders()[[1]])), "col")]
	themeargs <- args[intersect(names(args), names(geo_theme()[[1]]))]
	gridargs <- args[intersect(names(args), names(geo_grid()[[1]]))]
	
	g <- do.call("geo_shape", c(list(shp=shp), shapeargs)) +
		do.call("geo_borders", c(list(col=borders), borderargs))
	if (!missing(choro.fill)) g <- g + do.call("geo_choropleth", c(list(col=choro.fill), choroargs))
	if (!missing(bubble.size) || !missing(bubble.col)) g <- g + do.call("geo_bubblemap", c(list(size=bubble.size, col=bubble.col), bubbleargs))
	if (missing(theme)) {
		if (length(themeargs)) g <- g + do.call("geo_theme", themeargs)	
	} else {
		if (!(theme %in% c("World", "Europe", "NLD"))) stop("Unknown theme")
		funct <- paste("geo_theme", theme, sep="_")
		g <- g + do.call(funct, themeargs)
	}
	
	g
}

