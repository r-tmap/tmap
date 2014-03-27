#' Specify the shape object
#' 
#' This layer specifies the shape object, which is one of \code{\link[sp:SpatialPolygons]{SpatialPolygons}}, \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}, \code{\link[sp:SpatialPoints]{SpatialPoints}}, and \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}}.
#' 
#' A 
#' 
#' @param shp shape object. For \code{\link{geo_choropleth}} and \code{\link{geo_bubblemap}}, a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} or a \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} is requied. \code{\link[sp:SpatialPoints]{SpatialPoints}} and \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} are only used for \code{\link{geo_bubblemap}} and \code{\link{geo_bubbles}}.
#' @param projection character that determines the projectino. Either a \code{PROJ.4} character string (see \url{http://trac.osgeo.org/proj/}), of one of the following shortcuts: 
#' \describe{
#'    	\item{\code{"longlat"}}{Not really a projection, but a plot of the longitude-latitude coordinates.} 
#'    	\item{\code{"wintri"}}{Winkel Tripel (1921). Popular projection that is useful in world maps. It is the standard of world maps made by the National Geographic Society. Type: compromise} 
#'    	\item{\code{"robin"}}{Robinson (1963). Another popular projection for world maps. Type: compromise}
#'    	\item{\code{"eck4"}}{Eckert IV (1906). Projection useful for world maps. Area sizes are preserved, which makes it particularly useful for truthful choropleths. Type: equal-area}
#'    	\item{\cpde{"hd"}}{Hobo-Dyer (2002). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
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
	g <- list(geo_shape=list(shp=shp, shp_name=shp_name, projection=projection, xlim=xlim, ylim=ylim, relative=relative, bbox=bbox))
	class(g) <- "geo"
	g
}

#' @export
#' @return \code{\link{geo-object}}
geo_projection <- function(projection=NULL, 
						   xlim = c(min=0, max=1),
						   ylim = c(min=0, max=1),
						   relative = TRUE,
						   bbox = NULL) {
	g <- list(geo_projection=list(projection=projection, xlim=xlim, ylim=ylim, relative=relative, bbox=bbox))
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
	g <- list(geo_borders=list(col=col, lwd=lwd, lty=lty))
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
	g <- list(geo_fill=list(col=col))
	class(g) <- "geo"
	g
}

#' Draw bubbles
#' 
#' This layer speficies the drawing of bubbles. The colors and sizes of the bubbles are directly specified in this layer. Use \code{\link{geo_bubblemap}} to map bubbles colors and/or sizes to data.
#' 
#' @param size
#' @param col a single color value, or a vector of colors (specifying a color per polygon).
#' @param border
#' @param scale
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
#' @param text
#' @param cex
#' @export
#' @return \code{\link{geo-object}}
geo_text <-  function(text, cex=1) {
	g <- list(geo_text=list(text=text, cex=cex))
	class(g) <- "geo"
	g
}

#' Draw choropleth
#' 
#' This layer speficies a choropleth. A color palette is mapped to a data variable. By default, a divering color palette is used for numeric variables and a qualitative palette for categorical variables.
#' 
#' @param col name of variable that is contained in \code{shp}
#' @param palette palette name. See \code{RColorBrewer::display.brewer.all()} for options. Use a \code{"-"} as prefix to reverse the palette. By default, \code{"RdYlBu"} is taken for numeric variables, and \code{"Dark2"} for categorical variables.
#' @param n preferred number of classes (in case \code{col} is a numeric variable)
#' @param convert2density boolean that determines whether \code{col} is converted to a density variable. Should be \code{TRUE} when \code{col} consists of absolute numbers.
#' @param style method to cut the color scale (in case \code{col} is a numeric variable): "fixed", "equal", "pretty", "quantile", "kmeans"
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast number between 0 and 1 (default) that determines the contrast of the palette. Only applicable when \code{auto.palette.mapping=TRUE}
#' @param colorNA color used to missing values
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
							colorNA = "#DDDDDD",
							total.area.km2=NA) {
	
	g <- list(geo_choropleth=list(col=col, convert2density=convert2density, n=n, style=style, breaks=breaks, palette=palette, labels=labels, 
							auto.palette.mapping=auto.palette.mapping, contrast=contrast, colorNA=colorNA, total.area.km2=total.area.km2))
	class(g) <- "geo"
	g
}	

#' Draw bubblemap
#' 
#' This layer speficies a bubblemap. Both colors and sizes of the bubbles can be mapped to data variables. 
#' @param size \code{shp} data variable that determines the bubble sizes. Multiple variable names create small multiples
#' @param col color(s) of the bubble. Either a color (vector), or categorical variable name(s). Multiple variable names create small multiples
#' @param borders color of the bubble borders. If \code{NA} (default), no bubble borders are drawn.
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
								 auto.palette.mapping=auto.palette.mapping, contrast=contrast))
	class(g) <- "geo"
	g
}


#' Small multiples grid
#' 
#' This layer specifies how the different plots are placed in a grid. The number of rows and columns can be specified here, as well as whether the scales are free (i.e. independent of each other).
#' 
#' @param ncol number of columns of the small multiples grid
#' @param nrow number of rows of the small multiples grid
#' @param free.scales should the scales of the plotted data variables be free, i.e. independent of each other? Possible data variables are color from \code{\link{geo_choropleth}} and color and size from \code{\link{geo_bubblemap}}.
#' @export
#' @return \code{\link{geo-object}}
geo_grid <- function(ncol=NULL, nrow=NULL, 
					 free.scales=FALSE)	{
	g <- list(geo_grid=list(ncol=ncol, nrow=nrow, free.scales=free.scales))
	class(g) <- "geo"
	g
}

#' Theme elements of map plots
#' 
#' This layer specifies thematic layout options for the maps.
#' 
#' @param title Title of the map(s)
#' @param title.cex
#' @param bg.color
#' @param draw.frame
#' @param crop boolean that determines whether the shape objects are cropped at the bounding box (see \code{\link[sp:bbox]{bbox}})
#' @param show.legend.text
#' @param type.legend.plot
#' @param legend.position
#' @param legend.plot.size
#' @param legend.cex
#' @param legend.digits
#' @param title.position
#' @param margins
#' @param frame.lwd
#' @param legend.only
geo_theme <- function(title=NULL,
					  title.cex=1.5,
					  bg.color="yellow",
					  draw.frame=FALSE,
					  crop=draw.frame,
					  show.legend.text=NULL,
					  type.legend.plot = NULL,
					  legend.position = c("left", "top"),
					  legend.plot.size = NA,
					  legend.in.frame = TRUE,
					  legend.cex = 0.8,
					  legend.digits = 2,
					  title.position = c("left", "top"),
					  margins = NA,
					  frame.lwd=1,
					  legend.only=FALSE) {
	g <- list(geo_theme=list(title=title, title.cex=title.cex, 
							 bg.color=bg.color,
							 show.legend.text=show.legend.text,
							 type.legend.plot=type.legend.plot, 
							 legend.position=legend.position,
							 legend.plot.size=legend.plot.size, 
							 legend.in.frame=legend.in.frame,
							 legend.cex=legend.cex,
							 legend.digits=legend.digits, 
							 title.position=title.position,
							 margins=margins, draw.frame=draw.frame, 
							 frame.lwd=frame.lwd, legend.only=legend.only))
	class(g) <- "geo"
	g
}

# geo_theme.NLD <- function(total.area.km2=33893,
# 						  ...) {
# 	
# }




"+.geo" <- function(e1, e2) {
	g <- c(e1,e2)
	class(g) <- "geo"
	g
}

