#' Layout of cartographic maps
#' 
#' This element specifies the map layout. The main function \code{tm_layout} controls title, margins, aspect ratio, colors, frame, legend, among many other things. The function \code{tm_legend} is a shortcut to access all \code{legend.} arguments without this prefix. The other functions are wrappers for two purposes: \code{tm_format} specifies position related layout settings such as margins, and \code{tm_style} specifies general styling related layout settings such as colors and font. Typically, the former functions are shape dependent, and the latter functions are shape independent. See details for predefined styles and formats. With \code{tmap.style}, a default style can be specified. Multiple \code{tm_layout} elements (or wrapper functions) can be stacked: called arguments will be overwritten.
#' 
#' Predefined styles:
#' \tabular{ll}{
#' \code{"white"}\tab White background, commonly used colors (default) \cr
#' \code{"gray"}/\code{"grey"}\tab Grey background, useful to highlight sequential palettes (e.g. in choropleths) \cr
#' \code{"natural"}\tab Emulation of natural view: blue waters and green land \cr
#' \code{"bw"}\tab Greyscale, obviously useful for greyscale printing \cr
#' \code{"classic"}\tab Classic styled maps (recommended) \cr
#' \code{"cobalt"}\tab Inspired by latex beamer style cobalt \cr
#' \code{"albatross"}\tab Inspired by latex beamer style albatross \cr
#' \code{"beaver"}\tab Inspired by latex beamer style beaver \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#'
#' Predefined formats
#' \tabular{ll}{
#' \code{"World"}\tab Format specified for world maps \cr
#' \code{"World_wide"}\tab Format specified for world maps with more space for the legend \cr
#' \code{"NLD"}\tab Format specified for maps of the Netherlands \cr
#' \code{"NLD_wide"}\tab Format specified for maps of the Netherlands with more space for the legend \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' @name tm_layout
#' @rdname tm_layout
#' @param title Global title of the map. For small multiples, multiple titles can be specified. The title is drawn inside the map. Alternatively, use \code{panel.labels} to print the map as a panel, with the title inside the panel header (especially useful for small multiples). Another alternative is the \code{main.title} which prints a title above the map. Titles for the legend items are specified at the layer functions (e.g. \code{\link{tm_fill}}).
#' @param scale numeric value that serves as the global scale parameter. All font sizes, symbol sizes, border widths, and line widths are controlled by this value. Each of these elements can be scaled independently with the \code{scale}, \code{lwd}, or \code{size} arguments provided by the \code{\link{tmap-element}s}.
#' @param title.size Relative size of the title
#' @param bg.color Background color. By default it is \code{"white"}. A recommended alternative for choropleths is light grey (e.g., \code{"grey85"}).
#' @param aes.color Default color values for the aesthetics layers. Should be a named vector with the names chosen from: \code{fill}, \code{borders}, \code{symbols}, \code{dots}, \code{lines}, \code{text}, \code{na}. Use \code{"#00000000"} for transparency.
#' @param aes.palette Default color palettes for the aesthetics. It takes a list of three items: \code{seq} for sequential palettes, \code{div} for diverging palettes, and \code{cat} for categorical palettes. By default, Color Brewer palettes (see (see \code{tmaptools::palette_explorer()})) are used. It is also possible provide a vector of colors for any of these items.
#' @param attr.color Default color value for map attributes
#' @param sepia.intensity Number between 0 and 1 that defines the amount of sepia effect, which gives the map a brown/yellowish flavour. By default this effect is disabled (\code{sepia.intensity=0}). All colored used in the map are adjusted with this effect.
#' @param saturation Number that determines how much saturation (also known as chroma) is used: \code{saturation=0} is greyscale and \code{saturation=1} is normal. A number larger than 1 results in very saturated maps. All colored used in the map are adjusted with this effect. Hacking tip: use a negative number.
#' @param frame Either a boolean that determines whether a frame is drawn, or a color value that specifies the color of the frame. 
#' @param frame.lwd width of the frame
#' @param frame.double.line draw a double frame line border?
#' @param asp Aspect ratio. The aspect ratio of the map (width/height). If \code{NA}, it is determined by the bounding box (see argument \code{bbox} of \code{\link{tm_shape}}), the \code{outer.margins}, and the \code{inner.margins}. If \code{0}, then the aspect ratio is adjusted to the aspect ratio of the device.
#' @param outer.margins Relative margins between device and frame. Vector of four values specifying the bottom, left, top, and right margin. Values are between 0 and 1. When facets are created, the outer margins are the margins between the outer panels and the device borders (see also \code{between.margin})
#' @param inner.margins Relative margins inside the frame. Vector of four values specifying the bottom, left, top, and right margin. Values are between 0 and 1. By default, 0 for each side if master shape is a raster, otherwise 0.02.
#' @param between.margin Margin between facets (small multiples) in number of text line heights. The height of a text line is automatically scaled down based on the number of facets. 
#' @param outer.bg.color Background color outside the frame.
#' @param fontface global font face for the text in the map. It can also be set locally per element (see e.g. \code{title.fontface}).
#' @param fontfamily global font family for the text in the map. It can also be set locally per  (see e.g. \code{title.fontfamily}).
#' @param compass.type type of compass, one of: \code{"arrow"}, \code{"4star"}, \code{"8star"}, \code{"radar"}, \code{"rose"}. Of course, only applicable if a compass is shown. The compass type can also be set within \code{\link{tm_compass}}.
#' @param earth.boundary Logical that determines whether the boundaries of the earth are shown or an object that specifies the boundaries. This object can be a vector of size four, a 2 by 2 matrix (bounding box), or an \code{\link[raster:extent]{extent}} object. By default, the boundaries are \code{c(-180, -90, 180, 90)}. Useful for projected world maps. Often, it is useful to crop both poles (e.g., with \code{c(-180, -88, 180, 88)}).
#' @param earth.boundary.color Color of the earth boundary.
#' @param earth.boundary.lwd Line width of the earth boundary.
#' @param earth.datum Geodetic datum to determine the earth boundary. By default \code{"WGS84"}, other frequently used datums are \code{"NAD83"} and \code{"NAD27"}. Any other \code{PROJ.4} character string can be used.
#' @param space.color Color of the space, i.e. the region inside the frame, and outside the earth boundary.
#' @param legend.show Logical that determines whether the legend is shown.
#' @param legend.only logical. Only draw the legend (without map)? Particularly useful for small multiples with a common legend.
#' @param legend.outside Logical that determines whether the legend is plot outside of the map/facets. Especially useful when using facets that have a common legend (i.e. with \code{free.scales=FALSE}).
#' @param legend.outside.position Character that determines the outside position of the legend. Only applicable when \code{legend.outside=TRUE}. One of: \code{"right"}, \code{"left"}, \code{"top"}, or \code{"bottom"}.
#' @param legend.outside.size Numeric value that determines the relative size of the legend, when \code{legend.outside=TRUE}. If the first value of \code{legend.outside.position} is \code{"top"} or \code{"bottom"}, then it is the width of the legend, else it is the height of the legend.
#' @param legend.position Position of the legend. Vector of two values, specifying the x and y coordinates. Either this vector contains \code{"left"}, \code{"LEFT"}, \code{"center"}, \code{"right"}, or \code{"RIGHT"} for the first value and \code{"top"}, \code{"TOP"}, \code{"center"}, \code{"bottom"}, or \code{"BOTTOM"} for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y coordinates of the left bottom corner of the legend. The uppercase values correspond to the position without margins (so tighter to the frame). By default, it is automatically placed in the corner with most space based on the (first) shape object. If \code{legend.outside=TRUE}, this argument specifies the legend position within the outside panel.
#' @param legend.stack Value that determines how the legend items are stacked: \code{"vertical"} or \code{"horizontal"}.
#' @param legend.just Justification of the legend relative to the point coordinates.  The first value specifies horizontal and the second value vertical justification. Possible values are: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}. Numeric values of 0 specify left/bottom alignment and 1 right/top alignment. This option is only used, if \code{legend.position} is specified by numeric coordinates.
#' @param legend.width width of the legend. If it is a negative number, it will be the exact legend width. If it is a positive number (by default), it will be the maximum legend width; the actual legend width will be decreased automatically based on the legend content and font sizes.
#' @param legend.height height of the legend. If it is a negative number, it will be the exact legend height. If it is a positive number (by default), it will be the maximum legend height; the actual legend height will be decreased automatically based on the legend content and font sizes.
#' @param legend.hist.height height of the histogram. This height is initial. If the total legend is downscaled to \code{legend.height}, the histogram is downscaled as well.
#' @param legend.hist.width width of the histogram. By default, it is equal to the \code{legend.width}.
#' @param legend.title.size Relative font size for the legend title
#' @param legend.title.fontface font face for the legend title. By default, set to the global parameter \code{fontface}.
#' @param legend.title.fontfamily font family for the legend title. By default, set to the global parameter \code{fontfamily}.
#' @param legend.text.size Relative font size for the legend text elements
#' @param legend.text.fontface font face for the legend text labels. By default, set to the global parameter \code{fontface}.
#' @param legend.text.fontfamily font family for the legend text labels. By default, set to the global parameter \code{fontfamily}.
#' @param legend.hist.size Relative font size for the choropleth histogram
#' @param legend.format list of formatting options for the legend numbers. Only applicable for layer functions (such as \code{\link{tm_fill}}) where \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientifically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientifically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.
#' \item{text.to.columns}{Logical that determines whether the text is aligned to three columns (from, text.separator, to). By default \code{FALSE}.}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.
#' \item{text.to.columns}{Logical that determines whether the text is aligned to three columns (from, text.separator, to). By default \code{FALSE}.}
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.text.color color of the legend text
#' @param legend.bg.color Background color of the legend. Use \code{TRUE} to match with the overall background color \code{bg.color}.
#' @param legend.bg.alpha Transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{legend.bg.color} is used (normally 1).
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
#' @param title.bg.color background color of the title. Use \code{TRUE} to match with the overall background color \code{bg.color}. By default, it is \code{TRUE} if \code{legend.frame} is \code{TRUE} or a color.
#' @param title.bg.alpha Transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{title.bg.color} is used (normally 1).
#' @param panel.show Logical that determines if the map(s) are shown as panels. If \code{TRUE}, the title will be placed in the panel header instead of inside the map. By default, it is \code{TRUE} when small multiples are created with the \code{by} variable. (See \code{\link{tm_facets}}) 
#' @param panel.labels Panel labels. Only applicable when \code{panel.show} is \code{TRUE}. For cross tables facets, it should be a list containing the row names in the first, and column names in the second item.
#' @param panel.label.size Relative font size of the panel labels
#' @param panel.label.fontface font face for the panel labels. By default, set to the global parameter \code{fontface}.
#' @param panel.label.fontfamily font family for the panel labels. By default, set to the global parameter \code{fontfamily}.
#' @param panel.label.color Font color of the panel labels
#' @param panel.label.bg.color Background color of the panel labels
#' @param panel.label.height Height of the labels in number of text line heights.
#' @param panel.label.rot Rotation angles of the panel labels. Vector of two values: the first is the rotation angle (in degrees) of the row panels, which are only used in cross-table facets (when \code{\link{tm_facets}}'s \code{by} is specified with two variables). The second is the rotation angle of the column panels.
#' @param main.title Title that is printed above the map (or small multiples). When multiple pages are generated (see \code{along} argument of \code{\link{tm_facets}}), a vector can be provided. By default, the main title is only printed when this \code{along} argument is specified.
#' @param main.title.size Size of the main title
#' @param main.title.fontface font face for the main title. By default, set to the global parameter \code{fontface}.
#' @param main.title.fontfamily font family for the main title. By default, set to the global parameter \code{fontfamily}.
#' @param main.title.color Color of the main title
#' @param main.title.position Position of the main title. Either a numeric value between 0 (left) and 1 (right), or a character value: \code{"left"}, \code{"center"}, or \code{"right"}.
#' @param attr.outside Logical that determines whether the attributes are plot outside of the map/facets.
#' @param attr.outside.position Character that determines the outside position of the attributes: \code{"top"} or \code{"bottom"}. Only applicable when \code{attr.outside=TRUE}. If the legend is also drawn outside (with \code{legend.outside=TRUE}) and on the same side of the map (e.g. also \code{"top"} or \code{"bottom"}), the attributes are placed between the map and the legend. This can be changed by setting \code{attr.outside.position} to \code{"TOP"} or \code{"BOTTOM"}: in this case, the attributes are placed above respectively below the legend.
#' @param attr.outside.size Numeric value that determines the relative height of the attribute viewport, when \code{attr.outside=TRUE}.
#' @param attr.position Position of the map attributes, which are \code{\link{tm_credits}}, \code{\link{tm_scale_bar}}, \code{\link{tm_compass}}, and \code{\link{tm_minimap}}. Vector of two values, specifying the x and y coordinates. The first value is \code{"left"}, \code{"LEFT"}, \code{"center"}, \code{"right"}, or \code{"RIGHT"}, and the second value \code{"top"}, \code{"TOP"}, \code{"center"}, \code{"bottom"}, or \code{"BOTTOM"}. The uppercase values correspond to the position without margins (so tighter to the frame). Positions can also be set separately in the map attribute functions. If \code{attr.outside=TRUE}, this argument specifies the position of the attributes within the outside panel.
#' @param attr.just Justification of the attributes relative to the point coordinates.  The first value specifies horizontal and the second value vertical justification. Possible values are: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}. Numeric values of 0 specify left/bottom alignment and 1 right/top alignment. This option is only used, if \code{attr.position} is specified by numeric coordinates. It can also be specified per attribute function.
#' @param design.mode Logical that enables the design mode. If \code{TRUE}, inner and outer margins, legend position, aspect ratio are explicitly shown. Also, feedback text in the console is given.
#' @param basemaps Migrated to \code{\link{tm_view}}. vector of one or more names of baselayer maps used in the interactive view mode. See \code{\link{tm_view}}.
#' @param basemaps.alpha Migrated to \code{\link{tm_view}}. alpha transparency (opacity) of the basemaps. See \code{\link{tm_view}}.
#' @param bg.overlay Not used anymore. See \code{\link{tm_view}}.
#' @param bg.overlay.alpha Not used anymore. See \code{\link{tm_view}}.
#' @param ... other arguments from \code{tm_layout}
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
#' @example ./examples/tm_layout.R
#' @export
tm_layout <- function(title,
					  scale,
					  title.size,
					  bg.color,
					  aes.color,
					  aes.palette,
					  attr.color,
  					  sepia.intensity, 
  					  saturation, 
					  frame,
  					  frame.lwd,
  					  frame.double.line,
					  asp,
					  outer.margins,
					  inner.margins,
					  between.margin,
					  outer.bg.color,
					  fontface, 
					  fontfamily,
 					  compass.type,
					  earth.boundary,
					  earth.boundary.color,
					  earth.boundary.lwd,
					  earth.datum,
					  space.color,
					  legend.show,
					  legend.only,
					  legend.outside,
					  legend.outside.position,
					  legend.outside.size,
					  legend.position,
					  legend.stack,
					  legend.just,
					  legend.width,
					  legend.height,
					  legend.hist.height,
					  legend.hist.width,
					  legend.title.size,
					  legend.title.fontface,
					  legend.title.fontfamily,
					  legend.text.size,
					  legend.text.fontface,
					  legend.text.fontfamily,
					  legend.hist.size,
					  legend.format,
					  legend.frame,
					  legend.frame.lwd,
					  legend.text.color,
					  legend.bg.color,
					  legend.bg.alpha,
					  legend.hist.bg.color,
					  legend.hist.bg.alpha,
					  title.snap.to.legend,
					  title.position,
					  title.color,
					  title.fontface,
					  title.fontfamily,
					  title.bg.color,
					  title.bg.alpha,
					  panel.show,
					  panel.labels,
					  panel.label.size,
					  panel.label.color,
					  panel.label.fontface,
					  panel.label.fontfamily,
					  panel.label.bg.color,
					  panel.label.height,
					  panel.label.rot,
					  main.title,
					  main.title.size,
					  main.title.color,
					  main.title.fontface,
					  main.title.fontfamily,
					  main.title.position,
					  attr.outside,
					  attr.outside.position,
					  attr.outside.size,
					  attr.position,
					  attr.just,
					  design.mode,
					  basemaps,
					  basemaps.alpha,
					  bg.overlay,
					  bg.overlay.alpha) {

	e1 <- parent.frame()
	args <- lapply(as.list(match.call()[-1]), eval, envir = e1)
	args$style <- NA
	
	if (any(c("basemaps", "basemaps.alpha") %in% names(args))) warning("As of version 2.0, basemaps and basemaps.alpha have to be called from tm_view")
	if (any(c("bg.overlay", "bg.overlay.alpha") %in% names(args))) warning("bg.overlay and bg.overlay.alpha are not used anymore")
			
	args[c("bg.overlay", "bg.overlay.alpha")] <- NULL

	g <- list(tm_layout=args)
	class(g) <- "tm"
	g
}

#' @rdname tm_layout
#' @export
tm_legend <- function(...) {
	x <- list(...)

	tl_names <- names(formals("tm_layout"))
	is_leg <- which(substr(tl_names, 1, 3)=="leg")
	
	legend_x <- which(names(x) %in% substr(tl_names[is_leg], 8, nchar(tl_names[is_leg])))
	names(x)[legend_x] <- paste("legend", names(x)[legend_x], sep=".")

	do.call("tm_layout", x)
}


#' @rdname tm_layout
#' @param style name of the style
#' @export
tm_style <- function(style, ...) {
	args <- list(...)
	
	.tmapOptions <- get(".tmapOptions", envir = .TMAP_CACHE)	
	check_style(style)
	
	args$style <- style
	structure(list(tm_layout=args), class = "tm")
}

#' @rdname tm_layout
#' @param format name of the format
#' @export
tm_format <- function(format, ...) {
	args <- list(...)
	
	.tmapFormats <- get(".tmapFormats", envir = .TMAP_CACHE)
	
	if (!(format %in% names(.tmapFormats))) stop("Unknown format. Please check tmap_format() for available formats")
	
	formatArgs <- .tmapFormats[[format]]
	if (length(args)) {
		formatArgs[names(args)] <- args	
	}
	formatArgs$style <- NA
	
	called <- names(args)
	if (is.null(called)) called <- character(0)
	
	attr(formatArgs, "format_args") <- called
	structure(list(tm_layout=formatArgs), class = "tm")
}

