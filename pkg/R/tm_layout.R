#' Layout elements of cartographic maps
#' 
#' This element specifies layout options for the maps. The main function \code{tm_layout} is used by default as  general layout theme. The functions \code{tm_layout_World}, \code{tm_layout_Europe}, and \code{tm_layout_NLD} are layout themes for World, Europe, and Netherlands maps (which are contained in this package).
#' 
#' @name tm_layout
#' @rdname tm_layout
#' @param title Title(s). By default, the name of the statistical variable of which the legend is drawn at the top (see \code{legend.config}) is used as a title.
#' @param scale numeric value that serves as the global scale parameter. All font sizes, bubble sizes, border widths, and line widths are controled by this value. Each of these elements can be scaled independantly with the \code{scale}, \code{lwd}, or \code{cex} arguments provided by the \code{\link{tmap-element}s}.
#' @param title.cex Relative size of the title
#' @param bg.color Background color. By default it is light grey (\code{grey85}) for choropleths and white for other maps.
#' @param draw.frame Boolean that determines whether a frama is drawn. 
#' @param title.position Position of the title. Vector of two values, specifing the x and y coordinates. Either this vector contains "left", "center" or "right" for the first value and "top", "center", or "right" for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y value of the left bottom corner of the legend.
#' @param title.bg.color background color of the title. Use \code{TRUE} to match with the overall background color \code{bg.color}.
#' @param asp Aspect ratio. The aspect ratio of the map (width/height). If \code{NA}, it is determined by the bounding box (see argument \code{bbox} of \code{\link{tm_shape}}) and the argument \code{frame.margins}. If \code{0}, then the aspect ratio is adjusted to the aspect ratio of the device.
#' @param frame.lwd Width of the frame
#' @param outer.margins Relative margins between device and frame. Vector of four values specifying the bottom, left, top, and right margin. Values are between 0 and 1.
#' @param inner.margins Relative margins inside the frame. Vector of four values specifying the bottom, left, top, and right margin. Values are between 0 and 1.
#' @param outer.bg.color Background color outside the frame.
#' @param legend.show Logical that determines whether the legend is shown. Use \code{legend.config} to configure which legend elements are shown.
#' @param legend.hist.show Logical that determines whether to show a histogram for the choropleth fill variable.
#' @param legend.only logical. Only draw the legend (without map)? Particularly useful for small multiples with a common legend.
#' @param legend.titles titles of the legend elements. Named character vector, where the names correspond to the legend elements and the value to the titles of those elements. Possible legend element names are: \code{"fill"}, \code{"bubble.size"}, \code{"bubble.col"}, \code{"line.col"}, and \code{"line.lwd"}. For small multiples, a list of character vectors can be provided, where the list names correspond to the legend elements, and the character vectors to the legend titles of the small multiples per legend element. By default, the names of the corresponding statistical variables are used. For the legend element at the top, no legend title is used since the main title is used for this. A legend title for this element can be speficied.
#' @param legend.position Position of the legend. Vector of two values, specifing the x and y coordinates. Either this vector contains "left", "center" or "right" for the first value and "top", "center", or "right" for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y value of the left bottom corner of the legend.
#' @param legend.is.portrait logical vector that determines whether the orientation of the legend elements are portrait (\code{TRUE}) or landscape (\code{FALSE}). The vector should be named with the corresponding elements, which are \code{"fill"}, \code{"bubble.size"}, \code{"bubble.col"}, \code{"line.col"}, and \code{"line.lwd"}.
#' @param legend.width maximum width of the legend
#' @param legend.height maximum height of the legend.
#' @param legend.hist.height height of the histogram. This hight is initial. If the total legend is downscaled to \code{legend.height}, the histogram is downscaled as well.
#' @param legend.config character vector that specifies which legend elements are drawn and at what position. The legend elements are called \code{"fill"}, \code{"fill_hist"}, \code{"bubble.size"}, \code{"bubble.col"}, \code{"line.col"}, and \code{"line.lwd"}. The \code{legend.config} vector should only contain these elements (it can also be a subset). The order corresponds to the order in which the legend elements are stacked from top to bottom.
#' @param legend.title.cex Relative font size for the legend title
#' @param legend.text.cex Relative font size for the legend text elements
#' @param legend.hist.cex Relative font size for the choropleth histogram
#' @param legend.digits Number of digits for the legend labels
#' @param legend.bg.color Background color of the legend. Use \code{TRUE} to match with the overall background color \code{bg.color}.
#' @param ... other arguments from \code{tm_layout}
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @export
tm_layout <- function(title=NA,
					  scale=1,
					  title.cex=1.5,
					  bg.color=NULL,
					  draw.frame=TRUE,
					  title.position = c("left", "top"),
					  title.bg.color=NA,
					  asp = NA,
					  frame.lwd=1,
					  outer.margins = rep(0.02, 4),
					  inner.margins=rep(0.02, 4),
					  outer.bg.color="white",
					  legend.show = TRUE,
					  legend.hist.show = FALSE,
					  legend.only = FALSE,
					  legend.titles = c(fill = NA, bubble.size = NA, bubble.col = NA, line.col = NA, line.lwd = NA),
					  legend.position = c("left", "top"),
					  legend.is.portrait = c(fill = TRUE, bubble.size = FALSE, 
					  					   bubble.col = TRUE,
					  					   line.col = TRUE, line.lwd = FALSE),
					  legend.width = 0.3,
					  legend.height = 0.9,
					  legend.hist.height = 0.3,
					  legend.config = c("fill_hist", "fill", "bubble.size", "bubble.col", "line.col", "line.lwd"),
					  legend.title.cex=1.0,
					  legend.text.cex=0.7,
					  legend.hist.cex=0.7,
					  legend.digits = 2L,
					  legend.bg.color = NA) {
	g <- list(tm_layout=c(as.list(environment()), list(call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tm"
	g
}


#' @rdname tm_layout
#' @export
tm_layout_World <- function(title=NA,
							scale=.85,
							title.position = c("left", "bottom"),
							title.bg.color=TRUE,
							outer.margins=rep(.02, 4),
							inner.margins=c(0, 0.02, 0.02, 0.02),
							legend.position=c("left", "bottom"), 
							legend.width=.2,
							legend.height = .5,
							legend.bg.color=TRUE,
							...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_layout", args)
}

#' @rdname tm_layout
#' @export
tm_layout_Europe <- function(title=NA,
							 legend.position=c("left", "top"), 
							 outer.margins=rep(0.02, 4),
							 inner.margins=c(0, 0.25, 0, 0),
							 ...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_layout", args)
}


#' @rdname tm_layout
#' @export
tm_layout_NLD <- function(title=NA,
						  draw.frame=FALSE, 
						  inner.margins=c(.05, .3, .05, .05),
						  legend.position=c("left", "top"), 
						  legend.width=.3,
						  ...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_layout", args)
}
