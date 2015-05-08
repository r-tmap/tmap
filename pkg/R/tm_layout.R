#' Layout elements of cartographic maps
#' 
#' This element specifies layout options for the maps. The main function \code{tm_layout} can be seen as a general layout theme. The functions \code{tm_layout_World}, \code{tm_layout_Europe}, and \code{tm_layout_NLD} are layout themes specified for the world, Europe, and Netherlands maps (which are contained in this package). Tip: create a layout theme for your own map (see example below).
#' 
#' @name tm_layout
#' @rdname tm_layout
#' @param title Title(s). By default, the name of the statistical variable of which the legend is drawn at the top (see \code{legend.config}) is used as a title.
#' @param scale numeric value that serves as the global scale parameter. All font sizes, bubble sizes, border widths, and line widths are controled by this value. Each of these elements can be scaled independantly with the \code{scale}, \code{lwd}, or \code{size} arguments provided by the \code{\link{tmap-element}s}.
#' @param title.size Relative size of the title
#' @param bg.color Background color. By default it is light grey (\code{grey85}) for choropleths and white for other maps.
#' @param draw.frame Boolean that determines whether a frama is drawn. 
#' @param asp Aspect ratio. The aspect ratio of the map (width/height). If \code{NA}, it is determined by the bounding box (see argument \code{bbox} of \code{\link{tm_shape}}), the \code{outer.margins}, and the \code{inner.margins}. If \code{0}, then the aspect ratio is adjusted to the aspect ratio of the device.
#' @param frame.lwd Width of the frame
#' @param outer.margins Relative margins between device and frame. Vector of four values specifying the bottom, left, top, and right margin. Values are between 0 and 1.
#' @param inner.margins Relative margins inside the frame. Vector of four values specifying the bottom, left, top, and right margin. Values are between 0 and 1.
#' @param outer.bg.color Background color outside the frame.
#' @param legend.show Logical that determines whether the legend is shown. Use \code{legend.config} to configure which legend elements are shown.
#' @param legend.hist.show Logical that determines whether to show a histogram for the choropleth fill variable.
#' @param legend.only logical. Only draw the legend (without map)? Particularly useful for small multiples with a common legend.
#' @param legend.position Position of the legend. Vector of two values, specifing the x and y coordinates. Either this vector contains "left", "center" or "right" for the first value and "top", "center", or "bottom" for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y value of the left bottom corner of the legend. By default, it is automatically placed in the corner with most space based on the (first) shape object.
#' @param legend.width maximum width of the legend
#' @param legend.height maximum height of the legend.
#' @param legend.hist.height height of the histogram. This hight is initial. If the total legend is downscaled to \code{legend.height}, the histogram is downscaled as well.
#' @param legend.title.size Relative font size for the legend title
#' @param legend.text.size Relative font size for the legend text elements
#' @param legend.hist.size Relative font size for the choropleth histogram
#' @param legend.scientific logical. Should the numeric legend labels be formatted scientific?
#' @param legend.digits Number of digits for the legend labels
#' @param legend.bg.color Background color of the legend. Use \code{TRUE} to match with the overall background color \code{bg.color}.
#' @param legend.bg.alpha Transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{legend.bg.color} is used (normally 1).
#' @param title.position Position of the title. Vector of two values, specifing the x and y coordinates. Either this vector contains "left", "center" or "right" for the first value and "top", "center", or "bottom" for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y value of the left bottom corner of the legend. By default the title is placed on top of the legend (determined by \code{legend.position})
#' @param legend.frame either a logical that determines whether the legend is placed inside a frame, or a color that directly specifies the frame border color.
#' @param title.bg.color background color of the title. Use \code{TRUE} to match with the overall background color \code{bg.color}.
#' @param title.bg.alpha Transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{title.bg.color} is used (normally 1).
#' @param ... other arguments from \code{tm_layout}
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @example ../examples/tm_layout.R
#' @export
tm_layout <- function(title=NA,
					  scale=1,
					  title.size=1.5,
					  bg.color=NULL,
					  draw.frame=TRUE,
					  asp = NA,
					  frame.lwd=1,
					  outer.margins = rep(0.02, 4),
					  inner.margins = rep(0.02, 4),
					  outer.bg.color=NULL,
					  legend.show = TRUE,
					  legend.hist.show = FALSE,
					  legend.only = FALSE,
					  legend.position = NULL,
					  legend.width = 0.3,
					  legend.height = 0.9,
					  legend.hist.height = 0.3,
					  legend.title.size=1.3,
					  legend.text.size=0.8,
					  legend.hist.size=0.7,
					  legend.scientific = FALSE,
					  legend.digits = NA,
					  legend.frame = FALSE,
					  legend.bg.color = NA,
					  legend.bg.alpha = 1,
					  title.snap.to.legend = FALSE,
					  title.position = c("left", "top"),
					  title.bg.color=NA,
					  title.bg.alpha = 1) {
	g <- list(tm_layout=c(as.list(environment()), list(call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tm"
	g
}


#' @rdname tm_layout
#' @export
tm_layout_World <- function(title=NA,
							scale=.9,
							#title.bg.color=TRUE,
							inner.margins=c(0, 0.05, 0.075, 0.01),
							legend.position=c("left", "bottom"), 
							#legend.width=.25,
							#legend.height = .5,
							#legend.bg.color=TRUE,
							...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_layout", args)
}

#' @rdname tm_layout
#' @export
tm_layout_Europe <- function(title=NA,
							 title.position=c("left", "top"),
							 legend.position=c("left", "top"), 
							 inner.margins=c(0, 0.1, 0, 0),
							 ...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_layout", args)
}


#' @rdname tm_layout
#' @export
tm_layout_NLD <- function(title=NA,
						  draw.frame=FALSE, 
						  inner.margins=c(.02, .2, .06, .02),
						  legend.position=c("left", "top"), 
						  legend.width=.5,
						  ...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_layout", args)
}
