#' Styling elements of cartographic maps
#' 
#' This element specifies the style of the map. There are two main styles: modern, which corresponds to the default values, and classic, which is loaded by calling \code{tm_style_classic}. See details for the difference with \code{\link{tm_layout}}.
#' 
#' The difference between \code{tm_layout} and \code{\link{tm_style}} is the following. Specifications regarding position and size, for instance margins and legend size, are controleld wtih \code{tm_layout}. These specifications are typically dependent on the shapes, and, to a lesser extent, on the type of thematic map. Therefore, map dependent wrappers such as \code{tm_layout_World} can be useful. Furthermore, the default fixed colors (for background, title, attributes, and fixed aestethics) are defined with \code{tm_layout}. On the other hand, \code{\link{tm_style}} controls the styling of the map, that is independent of the used shapes, thematic map type, or used colors. There are two main flavours: modern (default) or classic (\code{\link{tm_style_classic}}).
#' 
#' @name tm_style
#' @rdname tm_style
#' @param sepia.intensity Number between 0 and 1 that defines the amount of sepia effect, which gives the map a brownish flavour. By default this effect is disabled (\code{sepia.intensity=0}).
#' @param saturation Number that determines how much saturation (also known as chroma) is used: \code{saturation=0} is greyscale and \code{saturation=1} is normal. A number larger than 1 results in very saturated maps. Hacking tip: use a negative number.
#' @param fontface font face of all text in the map.
#' @param fontfamily font family of the text labels.
#' @param frame.lwd width of the frame
#' @param frame.double.line draw a double frame line border?
#' @param compass.type type of compass, one of: \code{"arrow"}, \code{"4star"}, \code{"8star"}, \code{"radar"}, \code{"rose"}. Of course, only applicable if a compass is shown. The compass type can also be set within \code{\link{tm_compass}}.
#' @param ... arguments passed on to \code{tm_style}
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @example ../examples/tm_layout.R
#' @export
tm_style <- function(sepia.intensity=0, 
					 saturation=1, 
					 fontface="plain", 
					 fontfamily="sans",
					 frame.lwd=1,
					 frame.double.line=FALSE,
					 compass.type="arrow") {
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
tm_style_classic <- function(sepia.intensity=.7, fontfamily="serif", frame.double.line=TRUE, compass.type="rose", ...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_style", args)
}
