#' Theme elements of cartographic maps
#' 
#' This layer specifies thematic layout options for the maps.
#' 
#' @name geo_theme
#' @rdname geo_theme
#' @param title Title of the map(s)
#' @param title.cex Relative size of the title
#' @param bg.color Background color. By default it is light grey (\code{grey85}) for choropleths and white for other maps.
#' @param draw.frame Boolean that determines whether a frama is drawn. 
#' @param title.position Position of the title. Vector of two values, specifing the x and y coordinates. Either this vector contains "left", "center" or "right" for the first value and "top", "center", or "right" for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y value of the left bottom corner of the legend.
#' @param title.bg.color background color of the title. Use \code{TRUE} to match with the overall background color \code{bg.color}.
#' @param asp Aspect ratio. The aspect ratio of the map (width/height). If \code{NA}, it is determined by the bounding box (see argument \code{bbox} of \code{\link{geo_shape}}) and the argument \code{frame.margins}. If \code{0}, then the aspect ratio is adjusted to the aspect ratio of the device.
#' @param frame.lwd Width of the frame
#' @param outer.margins Relative margins between device and frame. Vector of four values specifying the bottom, left, top, and right margin. Values are between 0 and 1.
#' @param inner.margins Relative margins inside the frame. Vector of four values specifying the bottom, left, top, and right margin. Values are between 0 and 1.
#' @param outer.bg.color Background color outside the frame.
#' @param grid.show Boolean that determines whether grid lines are shown.
#' @param grid.n.x Prefered number of grid lines for the x axis.
#' @param grid.n.y Prefered number of grid lines for the y axis.
#' @param grid.color Color for the grid lines.
#' @param grid.on.top Boolean that determines whether the grid lines are drawn op top of the map (\code{TRUE}) or under the map (\code{FALSE}).
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
#' @param legend.max.height total maximum height of the legend. Heights of single legend elements are specified by \code{legend.choro.height}, \code{legend.choro.hist.height}, \code{legend.bubble.size.height}, and \code{legend.bubble.col.height}. If their total exceeds \code{legend.height}, then there are downscaled linearly.
#' @param legend.choro.height see \code{legend.max.height}
#' @param legend.choro.hist.height see \code{legend.max.height}
#' @param legend.bubble.size.height see \code{legend.max.height}
#' @param legend.bubble.col.height see \code{legend.max.height}
#' @param legend.config character vector that specifies which legend elements are drawn and at what position. The legend elements are called \code{"choro"}, \code{"hist"}, \code{"bubble.size"}, and \code{"bubble.col"}. The \code{legend.config} vector should only contain these elements (it can also be a subset). The order corresponds to the order in which the legend elements are stacked from top to bottom.
#' @param legend.title.cex Relative font size for the legend title
#' @param legend.text.cex Relative font size for the legend text elements
#' @param legend.hist.cex Relative font size for the choropleth histogram
#' @param legend.digits Number of digits for the legend labels
#' @param legend.NA.text Text to be used for missing values. Use \code{NA} to omit text for missing values 
#' @param legend.bg.color Background color of the legend. Use \code{TRUE} to match with the overall background color \code{bg.color}.
#' @param ... other arguments from \code{geo_theme}
#' @export
geo_theme <- function(title=NA,
					  title.cex=1.0,
					  bg.color=NULL,
					  draw.frame=TRUE,
					  title.position = c("left", "top"),
					  title.bg.color=NA,
					  asp = NA,
					  frame.lwd=1,
					  outer.margins = rep(0.02, 4),
					  inner.margins=rep(0.02, 4),
					  outer.bg.color="white",
					  grid.show=FALSE,
					  grid.n.x=8,
					  grid.n.y=8,
					  grid.color="grey50",
					  grid.on.top=TRUE,
					  legend.profile = "full",
					  legend.only = FALSE,
					  legend.titles = c(fill = NA, bubble.size = NA, bubble.col = NA, line.col = NA, line.lwd = NA),
					  legend.position = c("left", "top"),
					  legend.width = 0.3,
					  legend.max.height = 0.9,
					  legend.heights = c(fill = 0.3, fill_hist = 0.25, bubble.size = 0.15, bubble.col = 0.3, line.col = 0.3, line.lwd = 0.15),
					  legend.config = c("fill_hist", "fill", "bubble.size", "bubble.col", "line.col", "line.lwd"),
					  legend.title.cex=1.0,
					  legend.text.cex=0.7,
					  legend.hist.cex=0.7,
					  legend.digits = 2L,
					  legend.NA.text = "Missing",
					  legend.bg.color = NA) {
	g <- list(geo_theme=c(as.list(environment()), list(call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "geo"
	g
}


#' @rdname geo_theme
#' @export
geo_theme_World <- function(title=NA,
							title.cex=1,
							title.position = c("left", "bottom"),
							title.bg.color=TRUE,
							outer.margins=rep(.02, 4),
							inner.margins=c(0, 0.02, 0.02, 0.02),
							legend.position=c("left", "bottom"), 
							legend.width=.2,
							legend.max.height = .5,
							legend.text.cex=0.6,
							legend.hist.cex=0.6,
							legend.bg.color=TRUE,
							...) {
	args <- c(as.list(environment()), list(...))
	do.call("geo_theme", args)
}

#' @rdname geo_theme
#' @export
geo_theme_Europe <- function(title=NA,
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
						  inner.margins=c(.05, .3, .05, .05),
						  legend.position=c("left", "top"), 
						  legend.width=.3,
						  ...) {
	args <- c(as.list(environment()), list(...))
	do.call("geo_theme", args)
}
