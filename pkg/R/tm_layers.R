#' Add text labels
#' 
#' Creates a \code{\link{tmap-element}} that adds text labels.
#' 
#' @param text name of the variable in the shape object that contains the text labels
#' @param size relative size of the text labels (see note). Eiter one number, a name of a numeric variable in the shape data that is used to scale the sizes proportionally, or the value \code{"AREA"}, where the text size is proportional to the area size of the polygons.
#' @param root root number to which the font sizes are scaled. Only applicable if \code{size} is a variable name or \code{"AREA"}. If \code{root=2}, the square root is taken, if \code{root=3}, the cube root etc.
#' @param fontcolor color of the text labels
#' @param fontface font face of the text labels. By default, determined by the fontface argument of \code{\link{tm_layout}}.
#' @param fontfamily font family of the text labels. By default, determined by the fontfamily argument of \code{\link{tm_layout}}.
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{fontcolor} is used (normally 1).
#' @param case case of the font. Use "upper" to generate upper-case text, "lower" to generate lower-case text, and \code{NA} to leave the text as is.
#' @param shadow logical that determines whether a shadow is depicted behind the text. The color of the shadow is either white or yellow, depending of the \code{fontcolor}.
#' @param bg.color background color of the text labels. By default, \code{bg.color=NA}, so no background is drawn.
#' @param bg.alpha number between 0 and 1 that specifies the transparancy of the text background (0 is totally transparent, 1 is solid background).
#' @param size.lowerbound lowerbound for \code{size}. Only applicable when \code{size} is not a constant. If \code{print.tiny} is \code{TRUE}, then all text labels which relative text is smaller than \code{size.lowerbound} are depicted at relative size \code{size.lowerbound}. If \code{print.tiny} is \code{FALSE}, then text labels are only depicted if their relative sizes are at least \code{size.lowerbound} (in other words, tiny labels are omitted).
#' @param print.tiny boolean, see \code{size.lowerbound}
#' @param scale text size multiplier, useful in case \code{size} is variable or \code{"AREA"}.
#' @param auto.placement logical (or numeric) that determines whether the labels are placed automatically. If \code{TRUE}, the labels are placed next to the coordinate points with as little overlap as possible using the simulated annealing algorithm. Therefore, it is recommended for labeling spatial dots or bubbles. If a numeric value is provided, this value acts as a parameter that specifies the distance between the coordinate points and the text labels in terms of text line heights.
#' @param remove.overlap logical that determines whether the overlapping labels are removed
#' @param along.lines logical that determines whether labels are rotated along the spatial lines. Only applicabel if a spatial lines shape is used.
#' @param overwrite.lines logical that determines whether the part of the lines below the text labels is removed. Only applicabel if a spatial lines shape is used.
#' @param xmod horizontal position modification of the text (relatively): 0 means no modification, and 1 corresponds to the height of one line of text. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the text labels. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the text to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @note The absolute fontsize (in points) is determined by the (ROOT) viewport, which may depend on the graphics device.
#' @export
#' @example ../examples/tm_text.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}
tm_text <-  function(text, size=1, root=3, fontcolor=NA, fontface=NA, fontfamily=NA, alpha=NA, case=NA, shadow=FALSE, bg.color=NA, bg.alpha=NA, size.lowerbound=.4, print.tiny=FALSE, scale=1, auto.placement=FALSE, remove.overlap=FALSE, along.lines=FALSE, overwrite.lines=FALSE, xmod=0, ymod=0) {
	g <- list(tm_text=list(text=text, text.size=size, root=root, text.fontcolor=fontcolor, text.fontface=fontface, text.fontfamily=fontfamily, text.alpha=alpha, text.case=case, text.shadow=shadow, text.bg.color=bg.color, text.bg.alpha=bg.alpha,
							text.size.lowerbound=size.lowerbound, text.print.tiny=print.tiny, text.scale=scale, text.auto.placement=auto.placement, text.remove.overlap=remove.overlap, text.along.lines=along.lines, text.overwrite.lines=overwrite.lines, text.xmod=xmod, text.ymod=ymod))
	class(g) <- "tmap"
	g
}

#' Draw iso (contour) lines with labels
#' 
#' This function is a wrapper of \code{\link{tm_lines}} and \code{\link{tm_text}} aimed to draw isopleths, which can be created with \code{\link{smooth_map}}. 
#' 
#' @param col line color. See \code{\link{tm_lines}}.
#' @param text text to display. By default, it is the variable named \code{"level"} of the shape that is created with \code{\link{smooth_map}}
#' @param size text size (see \code{\link{tm_text}})
#' @param remove.overlap see \code{\link{tm_text}}
#' @param along.lines see \code{\link{tm_text}}
#' @param overwrite.lines see \code{\link{tm_text}}
#' @param ... arguments passed on to \code{\link{tm_lines}} or \code{\link{tm_text}}
#' @export
#' @seealso \code{\link{smooth_map}}
tm_iso <- function(col=NA, text="level", size=.5, 
				   remove.overlap=TRUE, along.lines=TRUE, overwrite.lines=TRUE, ...) {
	args <- list(...)
	argsL <- args[intersect(names(formals("tm_lines")), names(args))]
	argsT <- args[intersect(names(formals("tm_text")), names(args))]
	
	do.call("tm_lines", c(list(col=col), argsL)) +
		do.call("tm_text", c(list(text=text, size=size, auto.placement = auto.placement, 
								  remove.overlap=remove.overlap,
								  along.lines=along.lines,
								  overwrite.lines = overwrite.lines),
							 argsT))
}

#' Draw spatial lines
#' 
#' Creates a \code{\link{tmap-element}} that draw spatial lines.
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments. The aesthetic arguments of \code{tm_lines} are \code{col} and \code{lwd}. In the latter case, the arguments, except for the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' @param col color of the lines. Either a color value or a data variable name. If multiple values are specified, small multiples are drawn (see details).
#' @param lwd line width. If multiple values are specified, small multiples are drawn (see details).
#' @param lty line type.
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param scale line width multiplier number. 
#' @param lwd.legend vector of line widths that are shown in the legend. By default, this is determined automatically.
#' @param lwd.legend.labels vector of labels for that correspond to \code{lwd.legend}.
#' @param n preferred number of color scale classes. Only applicable when \code{lwd} is the name of a numeric variable.
#' @param style method to cut the color scale: e.g. "fixed", "equal", "pretty", "quantile", or "kmeans". See the details in \code{\link[classInt:classIntervals]{classIntervals}}. Only applicable when \code{lwd} is the name of a numeric variable.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param palette color palette (see \code{RColorBrewer::display.brewer.all}) for the lines. Only when \code{col} is set to a variable. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}.
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values. In this case of line widths, obviously only the positive side is used. 
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories}, then levels are combined.
#' @param colorNA color used for missing values
#' @param textNA text used for missing values. Use \code{NA} to omit text for missing values in the legend
#' @param title.col title of the legend element regarding the line colors
#' @param title.lwd title of the legend element regarding the line widths
#' @param legend.col.show logical that determines whether the legend for the line colors is shown
#' @param legend.lwd.show logical that determines whether the legend for the line widths is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{scientific}{Should the labels be formatted scientically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character string to use to translate "Less than" (which is the default).}
#' \item{text.or.more}{Character string to use to translate "or more" (which is the default). }
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.col.is.portrait logical that determines whether the legend element regarding the line colors is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.lwd.is.portrait logical that determines whether the legend element regarding the line widths is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.hist logical that determines whether a histogram is shown regarding the line colors
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend for line colors.
#' @param legend.col.z index value that determines the position of the legend element regarding the line colors with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.lwd.z index value that determines the position of the legend element regarding the line widths. (See \code{legend.col.z})
#' @param legend.hist.z index value that determines the position of the legend element regarding the histogram. (See \code{legend.col.z})
#' @param id name of the data variable that specifies the indices of the lines. Only used for SVG output (see \code{\link{itmap}}).
#' @export
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @example ../examples/tm_lines.R
#' @return \code{\link{tmap-element}}
tm_lines <- function(col=NA, lwd=1, lty="solid", alpha=NA,
					  scale=1,
					  lwd.legend = NULL,
					  lwd.legend.labels = NULL,
					  n = 5, style = ifelse(is.null(breaks), "pretty", "fixed"),
					  breaks = NULL,
					  palette = NULL,
					  labels = NULL,
					  auto.palette.mapping = TRUE,
					  contrast = 1,
					  max.categories = 12, 
					  colorNA = NA,
					  textNA = "Missing",
					 title.col=NA,
					 title.lwd=NA,
					 legend.col.show=TRUE,
					 legend.lwd.show=TRUE,
					 legend.format=list(),
					 legend.col.is.portrait=TRUE,
					 legend.lwd.is.portrait=FALSE,
					 legend.hist=FALSE,
					 legend.hist.title=NA,
					 legend.col.z=NA,
					 legend.lwd.z=NA,
					 legend.hist.z=NA,
					 id=NA) {
	g <- list(tm_lines=list(lines.col=col, lines.lwd=lwd, lines.lty=lty, lines.alpha=alpha, lines.scale=scale,
							lwd.legend=lwd.legend, lwd.legend.labels=lwd.legend.labels,
							 n=n, style=style, breaks=breaks, palette=palette, labels=labels,
							 auto.palette.mapping=auto.palette.mapping,
							 max.categories=max.categories,
							 contrast=contrast, colorNA=colorNA, textNA=textNA,
							title.col=title.col, title.lwd=title.lwd, 
							legend.col.show=legend.col.show,
							legend.lwd.show=legend.lwd.show,
							legend.format=legend.format,
							legend.col.is.portrait=legend.col.is.portrait, legend.lwd.is.portrait=legend.lwd.is.portrait, legend.hist=legend.hist, legend.hist.title=legend.hist.title, legend.col.z=legend.col.z, legend.lwd.z=legend.lwd.z, legend.hist.z=legend.hist.z, line.id=id))
	class(g) <- "tmap"
	g
}


# tm_iso <- function(col="black",
# 				   lwd=1,
# 				   lty="solid",
# 				   alpha=NA,
# 				   )


#' Draw polygons
#' 
#' Creates a \code{\link{tmap-element}} that draws the polygons. \code{tm_fill} fills the polygons. Either a fixed color is used, or a color palette is mapped to a data variable. By default, a divering color palette is used for numeric variables and a qualitative palette for categorical variables. \code{tm_borders} draws the borders of the polygons. \code{tm_polygons} fills the polygons and draws the polygon borders.
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments. The aesthetic argument of \code{tm_fill} (and \code{tm_polygons}) is \code{col}. In the latter case, the arguments, except for \code{thres.poly}, and the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' @name tm_fill
#' @rdname polygons
#' @param col For \code{tm_fill}, it is one of
#' \itemize{
#' \item a single color value
#' \item the name of a data variable that is contained in \code{shp}. Either the data variable contains color values, or values (numeric or categorical) that will be depicted by a color palette (see \code{palette}. In the latter case, a choropleth is drawn. #' \item \code{"MAP_COLORING"}. In this case polygons will be colored such that adjacent polygons do not get the same color. See the underlying function \code{\link{map_coloring}} for details.}
#' For \code{tm_borders}, it is a single color value that specifies the border line color. If multiple values are specified, small multiples are drawn (see details).
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param palette a palette name or a vector of colors. See \code{RColorBrewer::display.brewer.all()} for the named palette. Use a \code{"-"} as prefix to reverse the palette. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}.
#' @param convert2density boolean that determines whether \code{col} is converted to a density variable. Should be \code{TRUE} when \code{col} consists of absolute numbers. The area size is either approximated from the shape object, or given by the argument \code{area}.
#' @param area Name of the data variable that contains the area sizes in squared kilometer.
#' @param n preferred number of classes (in case \code{col} is a numeric variable).
#' @param style method to cut the color scale (in case \code{col} is a numeric variable): e.g. "fixed", "equal", "pretty", "quantile", or "kmeans". See the details in \code{\link[classInt:classIntervals]{classIntervals}}.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified.
#' @param labels labels of the classes.
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories}, then levels are combined.
#' @param colorNA color used for missing values
#' @param textNA text used for missing values. Use \code{NA} to omit text for missing values in the legend
#' @param thres.poly number that specifies the threshold at which polygons are taken into account. The number itself corresponds to the proportion of the area sizes of the polygons to the total polygon size. 
#' @param title title of the legend element
#' @param legend.show logical that determines whether the legend is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{scientific}{Should the labels be formatted scientically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character string to use to translate "Less than" (which is the default).}
#' \item{text.or.more}{Character string to use to translate "or more" (which is the default). }
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.is.portrait logical that determines whether the legend is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.hist logical that determines whether a histogram is shown
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend.
#' @param legend.z index value that determines the position of the legend element with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.hist.z index value that determines the position of the histogram legend element 
#' @param id name of the data variable that specifies the indices of the polygons. Only used for SVG output (see \code{\link{itmap}}).
#' @param ... for \code{tm_polygons}, these arguments passed to either \code{tm_fill} or \code{tm_borders}. For \code{tm_fill}, these arguments are passed on to \code{\link{map_coloring}}.
#' @keywords choropleth
#' @export
#' @example ../examples/tm_fill.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}	
tm_fill <- function(col=NA, 
					alpha=NA,
				    palette = NULL,
				    convert2density = FALSE,
			 		area = NULL,
				    n = 5,
				    style = ifelse(is.null(breaks), "pretty", "fixed"),
					breaks = NULL,
				    labels = NULL,
					auto.palette.mapping = TRUE,
					contrast = 1,
			 		max.categories = 12,
			 		colorNA = NA,
			 		textNA = "Missing",
					thres.poly = 1e-05,
					title=NA,
					legend.show=TRUE,
					legend.format=list(),
					legend.is.portrait=TRUE,
					legend.hist=FALSE,
					legend.hist.title=NA,
					legend.z=NA,
					legend.hist.z=NA,
					id=NA,
					...) {
	
	g <- list(tm_fill=c(as.list(environment()), list(map_coloring=list(...), call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tmap"
	g
}	


#' @name tm_borders
#' @rdname polygons
#' @param lwd border line width (see \code{\link[graphics:par]{par}})
#' @param lty border line type (see \code{\link[graphics:par]{par}})
#' @export
tm_borders <- function(col=NA, lwd=1, lty="solid", alpha=NA) {
	g <- list(tm_borders=as.list(environment()))
	class(g) <- "tmap"
	g
}

#' @name tm_polygons
#' @rdname polygons
#' @param border.col border line color
#' @param border.alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @export
tm_polygons <- function(col=NA, 
						alpha=NA,
						border.col=NA,
						border.alpha=NA,
						...) {
	args <- list(...)
	argsFill <- c(list(col=col, alpha=alpha), args[names(args)])
	argsBorders <- c(list(col=border.col, alpha=border.alpha), args[intersect(names(args), names(formals("tm_borders")))])
	g <- do.call("tm_fill", argsFill) + do.call("tm_borders", argsBorders)
	g$tm_fill$call <- names(match.call(expand.dots = TRUE)[-1])
	g
}


#' Draw a raster
#' 
#' Creates a \code{\link{tmap-element}} that draws a raster. Either a fixed color is used, or a color palette is mapped to a data variable. By default, a divering color palette is used for numeric variables and a qualitative palette for categorical variables.
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments. The aesthetic argument of \code{tm_raster} is \code{col}. In the latter case, the arguments, except for \code{thres.poly}, and the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' @param col either a single color value or the name of a data variable that is contained in \code{shp}. In the latter case, either the data variable contains color values, or values (numeric or categorical) that will be depicted by a color palette (see \code{palette}. If multiple values are specified, small multiples are drawn (see details). By default, it is the name of the first data variable.
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param palette palette name. See \code{RColorBrewer::display.brewer.all()} for options. Use a \code{"-"} as prefix to reverse the palette. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}.
#' @param n preferred number of classes (in case \code{col} is a numeric variable)
#' @param style method to cut the color scale (in case \code{col} is a numeric variable): e.g. "fixed", "equal", "pretty", "quantile", or "kmeans". See the details in \code{\link[classInt:classIntervals]{classIntervals}}.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories}, then levels are combined.
#' @param colorNA color used for missing values
#' @param saturation Number that determines how much saturation (also known as chroma) is used: \code{saturation=0} is greyscale and \code{saturation=1} is normal. This saturation value is multiplied by the overall saturation of the map (see \code{\link{tm_layout}}).
#' @param textNA text used for missing values. Use \code{NA} to omit text for missing values in the legend
#' @param title title of the legend element
#' @param legend.show logical that determines whether the legend is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{scientific}{Should the labels be formatted scientically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character string to use to translate "Less than" (which is the default).}
#' \item{text.or.more}{Character string to use to translate "or more" (which is the default). }
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.is.portrait logical that determines whether the legend is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.hist logical that determines whether a histogram is shown
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend.
#' @param legend.z index value that determines the position of the legend element with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.hist.z index value that determines the position of the histogram legend element 
#' @export
#' @example ../examples/tm_raster.r
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}	
tm_raster <- function(col=NA,
					  alpha = NA,
					  palette = NULL,
					  n = 5,
					  style = ifelse(is.null(breaks), "pretty", "fixed"),
					  breaks = NULL,
					  labels = NULL,
					  auto.palette.mapping = TRUE,
					  contrast = 1,
					  max.categories = 12,
					  colorNA = NA,
					  saturation = 1,
					  textNA = "Missing",
					  title=NA,
					  legend.show=TRUE,
					  legend.format=list(),
					  legend.is.portrait=TRUE,
					  legend.hist=FALSE,
					  legend.hist.title=NA,
					  legend.z=NA,
					  legend.hist.z=NA) {
	g <- list(tm_raster=as.list(environment()))
	class(g) <- "tmap"
	g
}

#' Draw bubbles or dots
#' 
#' Creates a \code{\link{tmap-element}} that draws bubbles or small dots. Both colors and sizes of the bubbles can be mapped to data variables. 
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments. The aesthetic arguments of \code{tm_bubbles} are \code{size} and \code{col}. In the latter case, the arguments, except for the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' @name tm_bubbles
#' @rdname tm_bubbles
#' @param size a single value or a \code{shp} data variable that determines the bubble sizes. The reference value \code{size=1} corresponds to the area of bubbles that have the same height as one line of text. If a data variable is provided, the bubble sizes are scaled proportionally (or perceptually, see \code{perceptual}) where the largest bubble will get \code{size=1}. If multiple values are specified, small multiples are drawn (see details).
#' @param col color(s) of the bubble. Either a color (vector), or categorical variable name(s). If multiple values are specified, small multiples are drawn (see details).
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param border.col color of the bubble borders.
#' @param border.lwd line width of the bubble borders. If \code{NA} (default), no bubble borders are drawn.
#' @param border.alpha transparency number, regarding the bubble borders, between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param scale bubble size multiplier number. 
#' @param perceptual logical that determines whether bubbles are scales with a perceptually (\code{TRUE}) or mathematically (\code{FALSE}, default value). The perceived area of larger bubbles is often underestimated. Flannery (1971) experimentally derived a method to compensate this, which is enabled by this argument.
#' @param size.lim vector of two limit values of the \code{size} variable. Only bubbles are drawn whose value is greater than or equal to the first value. Bubbles whose values exceed the second value are drawn at the size of the second value. Only applicable when \code{size} is the name of a numeric variable of \code{shp}
#' @param sizes.legend vector of bubble sizes that are shown in the legend. By default, this is determined automatically.
#' @param sizes.legend.labels vector of labels for that correspond to \code{sizes.legend}.
#' @param n preferred number of color scale classes. Only applicable when \code{col} is a numeric variable name.
#' @param style method to cut the color scale: e.g. "fixed", "equal", "pretty", "quantile", or "kmeans". See the details in \code{\link[classInt:classIntervals]{classIntervals}}. Only applicable when \code{col} is a numeric variable name.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param palette color palette (see \code{RColorBrewer::display.brewer.all}) for the bubbles. Only when \code{col} is set to a variable. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}.
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories}, then levels are combined.
#' @param colorNA colour for missing values
#' @param textNA text used for missing values. Use \code{NA} to omit text for missing values in the legend
#' @param xmod horizontal position modification of the bubbles, in terms of the height of one line of text. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the bubbles. See also \code{jitter} for random position modifications. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the bubbles to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @param jitter number that determines the amount of jittering, i.e. the random noise added to the position of the bubbles. 0 means no jittering is applied, any positive number means that the random noise has a standard deviation of \code{jitter} times the height of one line of text line.
#' @param title.size title of the legend element regarding the bubble sizes
#' @param title.col title of the legend element regarding the bubble colors
#' @param legend.size.show logical that determines whether the legend for the bubble sizes is shown
#' @param legend.col.show logical that determines whether the legend for the bubble colors is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{scientific}{Should the labels be formatted scientically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character string to use to translate "Less than" (which is the default).}
#' \item{text.or.more}{Character string to use to translate "or more" (which is the default). }
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.size.is.portrait logical that determines whether the legend element regarding the bubble sizes is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.hist logical that determines whether a histogram is shown regarding the bubble colors
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend for bubble colors.
#' @param legend.col.is.portrait logical that determines whether the legend element regarding the bubble colors is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.size.z index value that determines the position of the legend element regarding the bubble sizes with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.col.z index value that determines the position of the legend element regarding the bubble colors. (See \code{legend.size.z})
#' @param legend.hist.z index value that determines the position of the histogram legend element. (See \code{legend.size.z})
#' @param id name of the data variable that specifies the indices of the bubbles. Only used for SVG output (see \code{\link{itmap}}).
#' @param title shortcut for \code{title.col} for \code{tm_dots}
#' @param legend.is.portrait shortcut for \code{legend.col.is.portrait} for \code{tm_dots}
#' @param legend.z shortcut for \code{legend.col.z shortcut} for \code{tm_dots}
#' @keywords bubble map
#' @export
#' @example ../examples/tm_bubbles.R
#' @references Flannery J (1971). The Relative Effectiveness of Some Common Graduated Point Symbols in the Presentation of Quantitative Data. Canadian Cartographer, 8 (2), 96-109.
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}
tm_bubbles <- function(size=.2, col=NA,
						alpha=NA,
						border.col=NA,
						border.lwd=1,
						border.alpha=NA,
						scale=1,
						perceptual=FALSE,
						size.lim=NA,
						sizes.legend = NULL,
						sizes.legend.labels = NULL,
						n = 5, style = ifelse(is.null(breaks), "pretty", "fixed"),
						breaks = NULL,
						palette = NULL,
						labels = NULL,
						auto.palette.mapping = TRUE,
						contrast = 1,
						max.categories = 12,
						colorNA = "#FF1414",
						textNA = "Missing",
						jitter=0,
						xmod = 0,
						ymod = 0,
						title.size = NA,
						title.col = NA,
						legend.size.show=TRUE,
						legend.col.show=TRUE,
						legend.format=list(),
					   	legend.size.is.portrait=FALSE,
					    legend.col.is.portrait=TRUE,
					   	legend.hist=FALSE,
						legend.hist.title=NA,
						legend.size.z=NA,
						legend.col.z=NA,
						legend.hist.z=NA,
						id=NA) {
	g <- list(tm_bubbles=list(bubble.size=size, bubble.col=col, bubble.alpha=alpha, bubble.border.lwd=border.lwd,
							   bubble.border.col=border.col,
							   bubble.border.alpha=border.alpha,
								 bubble.scale=scale,
							     perceptual=perceptual,
								 size.lim=size.lim,
							     sizes.legend=sizes.legend,
							     sizes.legend.labels=sizes.legend.labels,
								 n=n, style=style, breaks=breaks, palette=palette, labels=labels,
								 auto.palette.mapping=auto.palette.mapping,
								 max.categories=max.categories,
								 contrast=contrast,
								 colorNA=colorNA,
								 textNA=textNA,
							  bubble.jitter=jitter,
								 bubble.xmod=xmod,
								 bubble.ymod=ymod,
							  legend.format=legend.format,
							  title.size=title.size,
							  title.col=title.col,
							  legend.size.show=legend.size.show,
							  legend.col.show=legend.col.show,
							  legend.size.is.portrait=legend.size.is.portrait, 
							  legend.col.is.portrait=legend.col.is.portrait, 
							  legend.hist=legend.hist,
							  legend.hist.title=legend.hist.title,
							  legend.size.z=legend.size.z, 
							  legend.col.z=legend.col.z,
							  legend.hist.z=legend.hist.z,
							  bubble.id=id,
							  are.dots=FALSE))
	class(g) <- "tmap"
	g
}


#' @rdname tm_bubbles
#' @param ... arguments passed on to \code{tm_bubbles}
#' @export
tm_dots <- function(col=NA, size=.02, title = NA, legend.is.portrait=TRUE, legend.z=NA, ...) {
	g <- do.call("tm_bubbles", c(list(size=size, col=col, title.col=title, 
								 legend.col.is.portrait=legend.is.portrait,
								 legend.size.z=legend.z), list(...)))
	g$tm_bubbles$are.dots <- TRUE
	g
}

