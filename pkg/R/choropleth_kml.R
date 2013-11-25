#' Create choropleeth
#' 
#' This function creates a choropleth.
#'
#' @param shp a shape object
#' @param x names of variables that are included in the \code{shp} data (see \code{names(shp)})
#' @param n preferred number of color scale classes
#' @param style method to cut the color scale: "fixed", "equal", "pretty", "quantile", "kmeans"
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param palette name of the used RColorBrewer palette (see \code{RColorBrewer::display.brewer.all})
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps values to colours such that negative values get a left-sided colour of the palette (for instance red), zeros get the middle colour, and positive values a right-sided colour (for instance blue).
#' @param contrast number between 0 and 1 (default) that determines the contrast of the palette. Only applicable when \code{auto.palette.mapping=TRUE}
#' @param legend.labels assign legend.labels. If not specified, legend labels will be automatically determined based on the break values (see also \code{legend.digits})
#' @param legend.digits number of printed digits of the break values
#' @param colorNA colour used for missing values
#' @param file filename of the kml file
#' @param ... other arguments
#' 
#' @keywords choropleth
#' @export

choropleth.kml <- function( shp, 
						x, n = 5,
						style = "pretty",
						breaks = NULL,
						palette = "RdBu",
						auto.palette.mapping = TRUE,
						contrast = 1,
						legend.labels = NULL,
						legend.digits = 2,
						colorNA = "#FF1414",
						file = "./temp.kml",
						time = NULL,
						...
){ 
	
	nx <- length(x)
	
	if (nx>1 & missing(time)) {
		time <- Sys.time() + 60*60 * (0:(nx))
	}
	
	
	X <- unlist(shp@data[, x])
	colsLeg <- num2pal(X, n, style=style, breaks=breaks, 
					   palette = palette,
					   auto.palette.mapping = auto.palette.mapping,
					   contrast = contrast, legend.labels=legend.labels,
					   legend.digits=legend.digits, colorNA=colorNA)
	colsList <- split(colsLeg[[1]], 
					  rep(1:nx, each=length(colsLeg[[1]])/nx))# for small multiples
	legend.labels <- colsLeg[[2]]
	legend.palette <- colsLeg[[3]]
	breaks <- colsLeg[[4]]
	
	
	shp2kmlOverlay(shp, colsList=colsList, file=file, time=time, ...)
	

}
