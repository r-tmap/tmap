#' Legend
#' 
#' Legend specification
#' 
#' @param title Legend title
#' @param show Show legend?
#' @param orientation Orientation of the legend: `"portrait"` or `"landscape"`
#' @param design PARAM_DESCRIPTION
#' @param reverse Should the legend be reversed?
#' @param na.show PARAM_DESCRIPTION
#' @param position PARAM_DESCRIPTION
#' @param width Width of the legend
#' @param height Height of the legend
#' @param stack PARAM_DESCRIPTION
#' @param z PARAM_DESCRIPTION
#' @param group.frame PARAM_DESCRIPTION
#' @param resize.as.group PARAM_DESCRIPTION
#' @param title.color Color of the legend title
#' @param title.size Size of the legend title
#' @param title.fontface Font face of the legend title
#' @param title.fontfamily Font family of the legend title
#' @param title.padding PARAM_DESCRIPTION
#' @param text.color Color of the legend text
#' @param text.size Size of the legend text
#' @param text.fontface Font face of the legend text
#' @param text.fontfamily Font family of the legend text
#' @param format PARAM_DESCRIPTION
#' @param frame PARAM_DESCRIPTION
#' @param frame.lwd PARAM_DESCRIPTION
#' @param frame.r PARAM_DESCRIPTION
#' @param bg.color Background color of the legend
#' @param bg.alpha Background transparency of the legend
#' @param item.height PARAM_DESCRIPTION
#' @param item.width PARAM_DESCRIPTION
#' @param item.space PARAM_DESCRIPTION
#' @param item.na.height PARAM_DESCRIPTION
#' @param item.na.width PARAM_DESCRIPTION
#' @param item.na.space PARAM_DESCRIPTION
#' @param item.shape PARAM_DESCRIPTION
#' @param ticks PARAM_DESCRIPTION
#' @param ticks.disable.na PARAM_DESCRIPTION
#' @param ticks.col PARAM_DESCRIPTION
#' @param ticks.lwd PARAM_DESCRIPTION
#' @param title.align PARAM_DESCRIPTION
#' @param margins PARAM_DESCRIPTION
#' @param margin.item.text PARAM_DESCRIPTION
#' @param ... passed on (?)
#' @param variable visual (or transformation) variable to combine the legend with: e.g. `"fill"` or `"size"`
#' @return OUTPUT_DESCRIPTION
#' @rdname tm_legend
#' @export 
tm_legend = function(title,
					 show,
					 orientation,
					 design,
					 reverse,
					 na.show,
					 position,
					 width,
					 height,
					 stack,
					 z,
					 group.frame,
					 resize.as.group,
					 title.color,
					 title.size,
					 title.fontface,
					 title.fontfamily,
					 title.padding,
					 text.color,
					 text.size,
					 text.fontface,
					 text.fontfamily,
					 format,
					 frame,
					 frame.lwd,
					 frame.r,
					 bg.color,
					 bg.alpha,
					 item.height,
					 item.width,
					 item.space,
					 item.na.height,
					 item.na.width,
					 item.na.space,
					 item.shape,
					 ticks,
					 ticks.disable.na,
					 ticks.col,
					 ticks.lwd,
					 title.align,
					 margins,
					 margin.item.text,
					 ...) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())

	if (!("title" %in% (names(args)))) args$title = NA
	if (!("xlab" %in% (names(args)))) args$xlab = NA
	if (!("ylab" %in% (names(args)))) args$ylab = NA
	if (!("z" %in% (names(args)))) args$z = as.integer(NA)
	structure(args, class = c("tm_legend", "tm_component", "list"))
}

#' @name tm_legend_hide
#' @rdname tm_legend
#' @export 
tm_legend_hide = function() {
	tm_legend(show = FALSE)
}

#' @name tm_legend_combine
#' @rdname tm_legend
#' @export 
tm_legend_combine = function(variable) {
	structure(list(FUN = "tmapLegend", title = NA, reverse = FALSE, show = FALSE, aes = variable), class = c("tm_legend", "tm_component", "list"))
}

tm_legend_bivariate = function(xlab, 
							   ylab,
							   xlab.color,
							   xlab.size,
							   xlab.fontface,
							   xlab.fontfamily,
							   xlab.padding,
							   xlab.align,
							   ylab.color,
							   ylab.size,
							   ylab.fontface,
							   ylab.fontfamily,
							   ylab.padding,
							   ylab.align,
							   ...) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	if (!("title" %in% (names(args)))) args$title = NA
	if (!("xlab" %in% (names(args)))) args$xlab = NA
	if (!("ylab" %in% (names(args)))) args$ylab = NA
	if (!("z" %in% (names(args)))) args$z = as.integer(NA)
	structure(args, class = c("tm_legend", "tm_component", "list"))
}

#' @param legend.show Logical that determines whether the legend is shown.
#' @param legend.only logical. Only draw the legend (without map)? Particularly useful for small multiples with a common legend.
#' @param legend.outside Logical that determines whether the legend is plot outside of the map/facets. Especially useful when using facets that have a common legend (i.e. with \code{free.scales=FALSE}).
#' @param legend.outside.position Character that determines the outside position of the legend. Only applicable when \code{legend.outside=TRUE}. One of: \code{"right"}, \code{"left"}, \code{"top"}, or \code{"bottom"}.
#' @param legend.outside.size Numeric value that determines the relative size of the legend, when \code{legend.outside=TRUE}. If the first value of \code{legend.outside.position} is \code{"top"} or \code{"bottom"}, then it is the width of the legend, else it is the height of the legend. Note that the actual height or width of the legend is determined by the content of the legend (and the used font sizes). This argument specifies the upperbound of the width or height.
#' @param legend.position Position of the legend. Vector of two values, specifying the x and y coordinates. Either this vector contains \code{"left"}, \code{"LEFT"}, \code{"center"}, \code{"right"}, or \code{"RIGHT"} for the first value and \code{"top"}, \code{"TOP"}, \code{"center"}, \code{"bottom"}, or \code{"BOTTOM"} for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y coordinates of the left bottom corner of the legend. The uppercase values correspond to the position without margins (so tighter to the frame). By default, it is automatically placed in the corner with most space based on the (first) shape object. If \code{legend.outside=TRUE}, this argument specifies the legend position within the outside panel.
#' @param legend.stack Value that determines how different legends are stacked: \code{"vertical"} or \code{"horizontal"}. To stack items within a same legend, look at \code{"legend.is.portrait"} in the specific layer calls. 
#' @param legend.just Justification of the legend relative to the point coordinates.  The first value specifies horizontal and the second value vertical justification. Possible values are: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}. Numeric values of 0 specify left/bottom alignment and 1 right/top alignment. This option is only used, if \code{legend.position} is specified by numeric coordinates.
#' @param legend.width width of the legend. This number is relative to the map area (so 1 means the whole map width). If it is a negative number, it will be the exact legend width. If it is a positive number (by default), it will be the maximum legend width; the actual legend width will be decreased automatically based on the legend content and font sizes.or Default color value for map attributes
#' @param legend.height height of the legend. If it is a negative number, it will be the exact legend height. If it is a positive number (by default), it will be the maximum legend height; the actual legend height will be decreased automatically based on the legend content and font sizes.
#' @param legend.hist.height height of the histogram. This height is initial. If the total legend is downscaled to \code{legend.height}, the histogram is downscaled as well.
#' @param legend.hist.width width of the histogram. By default, it is equal to the \code{legend.width}.
#' @param legend.title.color color of the legend titles
#' @param legend.title.size Relative font size for the legend title
#' @param legend.title.fontface font face for the legend title. By default, set to the global parameter \code{fontface}.
#' @param legend.title.fontfamily font family for the legend title. By default, set to the global parameter \code{fontfamily}.
#' @param legend.text.color color of the legend text
#' @param legend.text.size Relative font size for the legend text elements
#' @param legend.text.fontface font face for the legend text labels. By default, set to the global parameter \code{fontface}.
#' @param legend.text.fontfamily font family for the legend text labels. By default, set to the global parameter \code{fontfamily}.
#' @param legend.hist.size Relative font size for the choropleth histogram
#' @param legend.format list of formatting options for the legend numbers. Only applicable for layer functions (such as \code{\link{tm_fill}}) where \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientifically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, \code{text.or.more}, and \code{big.num.abbr} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientifically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{big.num.abbr}{Vector that defines whether and which abbrevations are used for large numbers. It is a named numeric vector, where the name indicated the abbreviation, and the number the magnitude (in terms on numbers of zero). Numbers are only abbrevation when they are large enough. Set it to \code{NA} to disable abbrevations.  The default is \code{c("mln" = 6, "bln" = 9)}. For layers where \code{style} is set to \code{log10} or \code{log10_pretty}, the default is \code{NA}.}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.
#' \item{text.to.columns}{Logical that determines whether the text is aligned to three columns (from, text.separator, to). By default \code{FALSE}.}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.
#' \item{text.to.columns}{Logical that determines whether the text is aligned to three columns (from, text.separator, to). By default \code{FALSE}.}
#' \item{html.escape}{Logical that determins whther HTML code is escaped in the popups in view mode. By default \code{TRUE}. If set to \code{FALSE} HTML code can be added, e.g. to added white space via \code{&nbsp;}.}
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.hist.bg.color Background color of the histogram
#' @param legend.hist.bg.alpha Transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{legend.hist.bg.color} is used (normally 1).
#' @param title.snap.to.legend Logical that determines whether the title is part of the legend. By default \code{FALSE}, unless the legend is drawn outside the map (see \code{legend.outside}).
#' @param title.position Position of the title. Vector of two values, specifying the x and y coordinates. Either this vector contains "left", "LEFT", "center", "right", or "RIGHT" for the first value and "top", "TOP", "center", "bottom", or "BOTTOM" for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y coordinates of the tile. The uppercase values correspond to the position without margins (so tighter to the frame). 
#' By default the title is placed on top of the legend (determined by \code{legend.position}).
#' @param title.color color of the title
#' @param title.fontface font face for the title. By default, set to the global parameter \code{fontface}.
#' @param title.fontfamily font family for the title. By default, set to the global parameter \code{fontfamily}.
#' @param legend.frame either a logical that determines whether the legend is placed inside a frame, or a color that directly specifies the frame border color.
#' @param legend.frame.lwd line width of the legend frame (applicable if \code{legend.frame} is \code{TRUE} or a color)
