#' Styling elements of cartographic maps
#' 
#' This element specifies the style of the map. There are two main styles: modern, which corresponds to the default values, and classic, which is loaded by calling \code{tm_style_classic}. See details for the difference with \code{\link{tm_layout}}.
#' 
#' The difference between \code{tm_layout} and \code{\link{tm_style}} is the following. Specifications regarding position and size, for instance margins and legend size, are controleld wtih \code{tm_layout}. These specifications are typically dependent on the shapes, and, to a lesser extent, on the type of thematic map. Therefore, map dependent wrappers such as \code{tm_layout_World} can be useful. On the other hand, \code{\link{tm_style}} controls the styling of the map, that is independent of the used shapes or thematic map type. There are two main flavours: modern (default) or classic (\code{\link{tm_style_classic}}).
#' 
#' @name tm_style
#' @rdname tm_style
#' @param sepia.intensity 
#' @param colorization
#' @param fontface
#' @param fontfamily
#' @param frame.lwd
#' @param frame.double.line
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @example ../examples/tm_layout.R
#' @export
tm_style <- function(sepia.intensity=0, 
					 saturation=1, 
					 fontface="plain", 
					 fontfamily="sans",
					 frame.lwd=1,
					 frame.double.line=FALSE) {
	g <- list(tm_style=as.list(environment()))
	class(g) <- "tmap"
	attr(g, "call") <- names(match.call(expand.dots = TRUE)[-1])
	g
}

#' @rdname tm_style
#' @export
tm_style_bw <- function(saturation=0, ...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_style", args)
}

#' @rdname tm_style
#' @export
tm_style_classic <- function(sepia.intensity=.7, fontfamily="serif", frame.double.line=TRUE, ...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_style", args)
}
