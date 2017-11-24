#' Add text labels
#' 
#' Creates a \code{\link{tmap-element}} that adds text labels.
#' 
#' @param text name of the variable in the shape object that contains the text labels
#' @param size relative size of the text labels (see note). Eiter one number, a name of a numeric variable in the shape data that is used to scale the sizes proportionally, or the value \code{"AREA"}, where the text size is proportional to the area size of the polygons.
#' @param col color of the text labels. Either a color value or a data variable name. If multiple values are specified, small multiples are drawn (see details).
#' @param root root number to which the font sizes are scaled. Only applicable if \code{size} is a variable name or \code{"AREA"}. If \code{root=2}, the square root is taken, if \code{root=3}, the cube root etc.
#' @param size.lim vector of two limit values of the \code{size} variable. Only text labels are drawn whose value is greater than or equal to the first value. Text labels whose values exceed the second value are drawn at the size of the second value. Only applicable when \code{size} is the name of a numeric variable of \code{shp}. See also \code{size.lowerbound} which is a threshold of the relative font size.
#' @param sizes.legend vector of text sizes that are shown in the legend. By default, this is determined automatically.
#' @param sizes.legend.labels vector of labels for that correspond to \code{sizes.legend}.
#' @param sizes.legend.text vector of example text to show in the legend next to sizes.legend.labels. By default "Abc". When \code{NA}, examples from the data variable whose sizes are close to the sizes.legend are taken and \code{"NA"} for classes where no match is found.
#' @param n preferred number of color scale classes. Only applicable when \code{col} is a numeric variable name.
#' @param style method to process the color scale when \code{col} is a numeric variable. Discrete options are \code{"cat"}, \code{"fixed"}, \code{"sd"}, \code{"equal"}, \code{"pretty"}, \code{"quantile"}, \code{"kmeans"}, \code{"hclust"}, \code{"bclust"}, \code{"fisher"}, and \code{"jenks"}. A numeric variable is processed as a categorial variable when using \code{"cat"}, i.e. each unique value will correspond to a distinct category. For the other discrete options, see the details in \code{\link[classInt:classIntervals]{classIntervals}}. Continuous options are \code{"cont"} and \code{"order"}. The former maps the values of \code{col} to a smooth gradient, whereas the latter maps the order of values of \code{col} to a smooth gradient. They are the continuous variants of respectively the discrete methods "equal" and quantile".
#' @param breaks in case \code{style=="fixed"}, breaks should be specified. The \code{breaks} argument can also be used when \code{style="cont"}. In that case, the breaks are mapped evenly to the sequential or divering color palette.
#' @param interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{col} is a numerc variable.
#' @param palette a palette name or a vector of colors. See \code{tmaptools::palette_explorer()} for the named palettes. Use a \code{"-"} as prefix to reverse the palette. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}, which typically depends on the style. The type of palette from \code{aes.palette} is automatically determined, but can be overwritten: use \code{"seq"} for sequential, \code{"div"} for diverging, and \code{"cat"} for categorical.
#' @param labels labels of the color classes, applicable if \code{col} is a data variable name
#' @param labels.text Example text to show in the legend next to the \code{labels}. When \code{NA} (default), examples from the data variable are taken and \code{"NA"} for classes where they don't exist.
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values. When categorical color palettes are used, this method stretches the palette if there are more levels than colors.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories} and \code{auto.palette.mapping} is \code{FALSE}, then levels are combined.
#' @param colorNA colour for missing values. Use \code{NULL} for transparency.
#' @param textNA text used for missing values. 
#' @param showNA logical that determines whether missing values are named in the legend. By default (\code{NA}), this depends on the presence of missing values.
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
#' @param auto.placement logical (or numeric) that determines whether the labels are placed automatically. If \code{TRUE}, the labels are placed next to the coordinate points with as little overlap as possible using the simulated annealing algorithm. Therefore, it is recommended for labeling spatial dots or symbols. If a numeric value is provided, this value acts as a parameter that specifies the distance between the coordinate points and the text labels in terms of text line heights.
#' @param remove.overlap logical that determines whether the overlapping labels are removed
#' @param along.lines logical that determines whether labels are rotated along the spatial lines. Only applicabel if a spatial lines shape is used.
#' @param overwrite.lines logical that determines whether the part of the lines below the text labels is removed. Only applicabel if a spatial lines shape is used.
#' @param just justification of the text relative to the point coordinates.  The first value specifies horizontal and the second value vertical justification. Possible values are: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}. Numeric values of 0 specify left alignment and 1 right alignment.
#' @param xmod horizontal position modification of the text (relatively): 0 means no modification, and 1 corresponds to the height of one line of text. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the text labels. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the text to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @param title.size title of the legend element regarding the text sizes
#' @param title.col title of the legend element regarding the text colors
#' @param legend.size.show logical that determines whether the legend for the text sizes is shown
#' @param legend.col.show logical that determines whether the legend for the text colors is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.
#' \item{text.to.columns}{Logical that determines whether the text is aligned to three columns (from, text.separator, to). By default \code{FALSE}.}
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.size.is.portrait logical that determines whether the legend element regarding the text sizes is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.size.reverse logical that determines whether the items of the legend regarding the text sizes are shown in reverse order, i.e. from bottom to top when \code{legend.size.is.portrait = TRUE} and from right to left when \code{legend.size.is.portrait = FALSE}
#' @param legend.hist logical that determines whether a histogram is shown regarding the text colors
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend for text colors.
#' @param legend.col.is.portrait logical that determines whether the legend element regarding the text colors is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.col.reverse logical that determines whether the items of the legend regarding the text colors are shown in reverse order, i.e. from bottom to top when \code{legend.col.is.portrait = TRUE} and from right to left when \code{legend.col.is.portrait = FALSE}
#' @param legend.size.z index value that determines the position of the legend element regarding the text sizes with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.col.z index value that determines the position of the legend element regarding the text colors. (See \code{legend.size.z})
#' @param legend.hist.z index value that determines the position of the histogram legend element. (See \code{legend.size.z})
#' @note The absolute fontsize (in points) is determined by the (ROOT) viewport, which may depend on the graphics device.
#' @export
#' @example ./examples/tm_text.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}
tm_text <-  function(text, size=1, col=NA, root=3, 
					 size.lim=NA,
					 sizes.legend = NULL,
					 sizes.legend.labels = NULL,
					 sizes.legend.text = "Abc",
					 n = 5, style = ifelse(is.null(breaks), "pretty", "fixed"),
					 breaks = NULL,
					 interval.closure = "left",
					 palette = NULL,
					 labels = NULL,
					 labels.text = NA,
					 auto.palette.mapping = TRUE,
					 contrast = NA,
					 max.categories = 12,
					 colorNA = NA,
					 textNA = "Missing",
					 showNA = NA,
					 fontface=NA, 
					 fontfamily=NA, alpha=NA, case=NA, shadow=FALSE, bg.color=NA, bg.alpha=NA, size.lowerbound=.4, print.tiny=FALSE, scale=1, auto.placement=FALSE, remove.overlap=FALSE, along.lines=FALSE, overwrite.lines=FALSE, just=c("center","center"), xmod=0, ymod=0,
					 title.size = NA,
					 title.col = NA,
					 legend.size.show=TRUE,
					 legend.col.show=TRUE,
					 legend.format=list(),
					 legend.size.is.portrait=FALSE,
					 legend.col.is.portrait=TRUE,
					 legend.size.reverse=FALSE,
					 legend.col.reverse=FALSE,
					 legend.hist=FALSE,
					 legend.hist.title=NA,
					 legend.size.z=NA,
					 legend.col.z=NA,
					 legend.hist.z=NA) {
	g <- list(tm_text=c(as.list(environment()), list(call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tmap"
	g
}
	
#' Draw iso (contour) lines with labels
#' 
#' This function is a wrapper of \code{\link{tm_lines}} and \code{\link{tm_text}} aimed to draw isopleths, which can be created with \code{\link[tmaptools:smooth_map]{smooth_map}}. 
#' 
#' @param col line color. See \code{\link{tm_lines}}.
#' @param text text to display. By default, it is the variable named \code{"level"} of the shape that is created with \code{\link[tmaptools:smooth_map]{smooth_map}}
#' @param size text size (see \code{\link{tm_text}})
#' @param remove.overlap see \code{\link{tm_text}}
#' @param along.lines see \code{\link{tm_text}}
#' @param overwrite.lines see \code{\link{tm_text}}
#' @param ... arguments passed on to \code{\link{tm_lines}} or \code{\link{tm_text}}
#' @export
#' @seealso \code{\link[tmaptools:smooth_map]{smooth_map}}
tm_iso <- function(col=NA, text="level", size=.5, 
				   remove.overlap=TRUE, along.lines=TRUE, overwrite.lines=TRUE, ...) {
	args <- list(...)
	argsL <- args[intersect(names(formals("tm_lines")), names(args))]
	argsT <- args[intersect(names(formals("tm_text")), names(args))]
	
	do.call("tm_lines", c(list(col=col), argsL)) +
		do.call("tm_text", c(list(text=text, size=size,
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
#' @param lwd line width. Either a numeric value or a data variable. In the latter case, the class of the highest values (see \code{style}) will get the line width defined by \code{scale}. If multiple values are specified, small multiples are drawn (see details).
#' @param lty line type.
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param scale line width multiplier number. 
#' @param lwd.legend vector of line widths that are shown in the legend. By default, this is determined automatically.
#' @param lwd.legend.labels vector of labels for that correspond to \code{lwd.legend}.
#' @param n preferred number of color scale classes. Only applicable when \code{lwd} is the name of a numeric variable.
#' @param style method to process the color scale when \code{col} is a numeric variable. Discrete options are \code{"cat"}, \code{"fixed"}, \code{"sd"}, \code{"equal"}, \code{"pretty"}, \code{"quantile"}, \code{"kmeans"}, \code{"hclust"}, \code{"bclust"}, \code{"fisher"}, and \code{"jenks"}. A numeric variable is processed as a categorial variable when using \code{"cat"}, i.e. each unique value will correspond to a distinct category. For the other discrete options, see the details in \code{\link[classInt:classIntervals]{classIntervals}}. Continuous options are \code{"cont"} and \code{"order"}. The former maps the values of \code{col} to a smooth gradient, whereas the latter maps the order of values of \code{col} to a smooth gradient. They are the continuous variants of respectively the discrete methods "equal" and quantile".
#' @param breaks in case \code{style=="fixed"}, breaks should be specified. The \code{breaks} argument can also be used when \code{style="cont"}. In that case, the breaks are mapped evenly to the sequential or divering color palette.
#' @param interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{col} is a numerc variable.
#' @param palette a palette name or a vector of colors. See \code{tmaptools::palette_explorer()} for the named palettes. Use a \code{"-"} as prefix to reverse the palette. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}, which typically depends on the style. The type of palette from \code{aes.palette} is automatically determined, but can be overwritten: use \code{"seq"} for sequential, \code{"div"} for diverging, and \code{"cat"} for categorical.
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values. In this case of line widths, obviously only the positive side is used. When categorical color palettes are used, this method stretches the palette if there are more levels than colors.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories} and \code{auto.palette.mapping} is \code{FALSE}, then levels are combined.
#' @param colorNA color used for missing values. Use \code{NULL} for transparency.
#' @param textNA text used for missing values.
#' @param showNA logical that determines whether missing values are named in the legend. By default (\code{NA}), this depends on the presence of missing values.
#' @param title.col title of the legend element regarding the line colors
#' @param title.lwd title of the legend element regarding the line widths
#' @param legend.col.show logical that determines whether the legend for the line colors is shown
#' @param legend.lwd.show logical that determines whether the legend for the line widths is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.
#' \item{text.to.columns}{Logical that determines whether the text is aligned to three columns (from, text.separator, to). By default \code{FALSE}.}
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.col.is.portrait logical that determines whether the legend element regarding the line colors is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.lwd.is.portrait logical that determines whether the legend element regarding the line widths is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.col.reverse logical that determines whether the items of the legend regarding the line colors sizes are shown in reverse order, i.e. from bottom to top when \code{legend.col.is.portrait = TRUE} and from right to left when \code{legend.col.is.portrait = FALSE}
#' @param legend.lwd.reverse logical that determines whether the items of the legend regarding the line widths are shown in reverse order, i.e. from bottom to top when \code{legend.lwd.is.portrait = TRUE} and from right to left when \code{legend.lwd.is.portrait = FALSE}
#' @param legend.hist logical that determines whether a histogram is shown regarding the line colors
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend for line colors.
#' @param legend.col.z index value that determines the position of the legend element regarding the line colors with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.lwd.z index value that determines the position of the legend element regarding the line widths. (See \code{legend.col.z})
#' @param legend.hist.z index value that determines the position of the legend element regarding the histogram. (See \code{legend.col.z})
#' @param id name of the data variable that specifies the indices of the lines. Only used for \code{"view"} mode (see \code{\link{tmap_mode}}).
#' @param popup.vars names of data variables that are shown in the popups in \code{"view"} mode. If \code{NA} (default), only aesthetic variables (i.e. specified by \code{col} and \code{lwd}) are shown). If they are not specified, all variables are shown. Set popup.vars to \code{FALSE} to disable popups. When a vector of variable names is provided, the names (if specified) are printed in the popups.
#' @param popup.format list of formatting options for the popup values. See the argument \code{legend.format} for options. Only applicable for numeric data variables. If one list of formatting options is provided, it is applied to all numeric variables of \code{popup.vars}. Also, a (named) list of lists can be provided. In that case, each list of formatting options is applied to the named variable.
#' @export
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @example ./examples/tm_lines.R
#' @return \code{\link{tmap-element}}
tm_lines <- function(col=NA, lwd=1, lty="solid", alpha=NA,
					 scale=1,
					 lwd.legend = NULL,
					 lwd.legend.labels = NULL,
					 n = 5, style = ifelse(is.null(breaks), "pretty", "fixed"),
					 breaks = NULL,
					 interval.closure = "left",
					 palette = NULL,
					 labels = NULL,
					 auto.palette.mapping = TRUE,
					 contrast = NA,
					 max.categories = 12, 
					 colorNA = NA,
					 textNA = "Missing",
					 showNA = NA,
					 title.col=NA,
					 title.lwd=NA,
					 legend.col.show=TRUE,
					 legend.lwd.show=TRUE,
					 legend.format=list(),
					 legend.col.is.portrait=TRUE,
					 legend.lwd.is.portrait=FALSE,
					 legend.col.reverse=FALSE,
					 legend.lwd.reverse=FALSE,
					 legend.hist=FALSE,
					 legend.hist.title=NA,
					 legend.col.z=NA,
					 legend.lwd.z=NA,
					 legend.hist.z=NA,
					 id=NA,
					 popup.vars=NA,
					 popup.format=list()) {
	g <- list(tm_lines=c(as.list(environment()), list(call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tmap"
	g
}




#' Draw polygons
#' 
#' Creates a \code{\link{tmap-element}} that draws the polygons. \code{tm_fill} fills the polygons. Either a fixed color is used, or a color palette is mapped to a data variable. \code{tm_borders} draws the borders of the polygons. \code{tm_polygons} fills the polygons and draws the polygon borders.
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments. The aesthetic argument of \code{tm_fill} (and \code{tm_polygons}) is \code{col}. In the latter case, the arguments, except for \code{thres.poly}, and the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' @name tm_fill
#' @rdname tm_polygons
#' @param col For \code{tm_fill}, it is one of
#' \itemize{
#' \item a single color value
#' \item the name of a data variable that is contained in \code{shp}. Either the data variable contains color values, or values (numeric or categorical) that will be depicted by a color palette (see \code{palette}. In the latter case, a choropleth is drawn. 
#' \item \code{"MAP_COLORS"}. In this case polygons will be colored such that adjacent polygons do not get the same color. See the underlying function \code{\link[tmaptools:map_coloring]{map_coloring}} for details.}
#' For \code{tm_borders}, it is a single color value that specifies the border line color. If multiple values are specified, small multiples are drawn (see details).
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param palette a palette name or a vector of colors. See \code{tmaptools::palette_explorer()} for the named palettes. Use a \code{"-"} as prefix to reverse the palette. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}, which typically depends on the style. The type of palette from \code{aes.palette} is automatically determined, but can be overwritten: use \code{"seq"} for sequential, \code{"div"} for diverging, and \code{"cat"} for categorical.
#' @param convert2density boolean that determines whether \code{col} is converted to a density variable. Should be \code{TRUE} when \code{col} consists of absolute numbers. The area size is either approximated from the shape object, or given by the argument \code{area}.
#' @param area Name of the data variable that contains the area sizes in squared kilometer.
#' @param n preferred number of classes (in case \code{col} is a numeric variable).
#' @param style method to process the color scale when \code{col} is a numeric variable. Discrete options are \code{"cat"}, \code{"fixed"}, \code{"sd"}, \code{"equal"}, \code{"pretty"}, \code{"quantile"}, \code{"kmeans"}, \code{"hclust"}, \code{"bclust"}, \code{"fisher"}, and \code{"jenks"}. A numeric variable is processed as a categorial variable when using \code{"cat"}, i.e. each unique value will correspond to a distinct category. For the other discrete options, see the details in \code{\link[classInt:classIntervals]{classIntervals}}. Continuous options are \code{"cont"} and \code{"order"}. The former maps the values of \code{col} to a smooth gradient, whereas the latter maps the order of values of \code{col} to a smooth gradient. They are the continuous variants of respectively the discrete methods "equal" and quantile".
#' @param breaks in case \code{style=="fixed"}, breaks should be specified. The \code{breaks} argument can also be used when \code{style="cont"}. In that case, the breaks are mapped evenly to the sequential or divering color palette.
#' @param interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{col} is a numerc variable.
#' @param labels labels of the classes.
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values. When categorical color palettes are used, this method stretches the palette if there are more levels than colors.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories} and \code{auto.palette.mapping} is \code{FALSE}, then levels are combined.
#' @param colorNA color used for missing values. Use \code{NULL} for transparency.
#' @param textNA text used for missing values.
#' @param showNA logical that determines whether missing values are named in the legend. By default (\code{NA}), this depends on the presence of missing values.
#' @param thres.poly number that specifies the threshold at which polygons are taken into account. The number itself corresponds to the proportion of the area sizes of the polygons to the total polygon size. By default, all polygons are drawn. To ignore polygons that are not visible in a normal plot, a value like \code{1e-05} is recommended.
#' @param title title of the legend element
#' @param legend.show logical that determines whether the legend is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.
#' \item{text.to.columns}{Logical that determines whether the text is aligned to three columns (from, text.separator, to). By default \code{FALSE}.}
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.is.portrait logical that determines whether the legend is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.reverse logical that determines whether the items are shown in reverse order, i.e. from bottom to top when \code{legend.is.portrait = TRUE} and from right to left when \code{legend.is.portrait = FALSE}
#' @param legend.hist logical that determines whether a histogram is shown
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend.
#' @param legend.z index value that determines the position of the legend element with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.hist.z index value that determines the position of the histogram legend element 
#' @param id name of the data variable that specifies the indices of the polygons. Only used for \code{"view"} mode (see \code{\link{tmap_mode}}).
#' @param popup.vars names of data variables that are shown in the popups in \code{"view"} mode. If \code{convert2density=TRUE}, the derived density variable name is suffixed with \code{_density}. If \code{NA} (default), only aesthetic variables (i.e. specified by \code{col} and \code{lwd}) are shown). If they are not specified, all variables are shown. Set popup.vars to \code{FALSE} to disable popups. When a vector of variable names is provided, the names (if specified) are printed in the popups.
#' @param popup.format list of formatting options for the popup values. See the argument \code{legend.format} for options. Only applicable for numeric data variables. If one list of formatting options is provided, it is applied to all numeric variables of \code{popup.vars}. Also, a (named) list of lists can be provided. In that case, each list of formatting options is applied to the named variable.
#' @param ... for \code{tm_polygons}, these arguments passed to either \code{tm_fill} or \code{tm_borders}. For \code{tm_fill}, these arguments are passed on to \code{\link[tmaptools:map_coloring]{map_coloring}}.
#' @keywords choropleth
#' @export
#' @example ./examples/tm_fill.R
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
					interval.closure = "left",
				    labels = NULL,
					auto.palette.mapping = TRUE,
					contrast = NA,
			 		max.categories = 12,
			 		colorNA = NA,
			 		textNA = "Missing",
					showNA = NA,
					thres.poly = 0,
					title=NA,
					legend.show=TRUE,
					legend.format=list(),
					legend.is.portrait=TRUE,
					legend.reverse=FALSE,
					legend.hist=FALSE,
					legend.hist.title=NA,
					legend.z=NA,
					legend.hist.z=NA,
					id=NA,
					popup.vars=NA,
					popup.format=list(),
					...) {
	
	g <- list(tm_fill=c(as.list(environment()), list(map_coloring=list(...), call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tmap"
	g
}	


#' @name tm_borders
#' @rdname tm_polygons
#' @param lwd border line width (see \code{\link[graphics:par]{par}})
#' @param lty border line type (see \code{\link[graphics:par]{par}})
#' @export
tm_borders <- function(col=NA, lwd=1, lty="solid", alpha=NA) {
	g <- list(tm_borders=as.list(environment()))
	class(g) <- "tmap"
	g
}

#' @name tm_polygons
#' @rdname tm_polygons
#' @param border.col border line color
#' @param border.alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @export
tm_polygons <- function(col=NA, 
						alpha=NA,
						border.col=NA,
						border.alpha=NA,
						...) {
	args <- list(...)
	argsFill <- c(list(col=col, alpha=alpha), args[setdiff(names(args), c("lwd", "lty"))])
	argsBorders <- c(list(col=border.col, alpha=border.alpha), args[intersect(names(args), names(formals("tm_borders")))])
	g <- do.call("tm_fill", argsFill) + do.call("tm_borders", argsBorders)
	g$tm_fill$call <- names(match.call(expand.dots = TRUE)[-1])
	g
}


#' Draw a raster
#' 
#' Creates a \code{\link{tmap-element}} that draws a raster. For coloring, there are three options: 1) a fixed color is used, 2) a color palette is mapped to a data variable, 3) RGB values are used. The function \code{tm_raster} is designed for option 2, while \code{tm_rgb} is used for option 3.
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments. The aesthetic argument of \code{tm_raster} is \code{col}. In the latter case, the arguments, except for the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' @param col three options: a single color value, the name of a data variable that is contained in \code{shp}, or the name of a variable in \code{shp} that contain color values. In the second case the values (numeric or categorical) that will be depicted by a color palette (see \code{palette}. If omitted, and if \code{shp} contains three numeric layers that range between 0 and 255, these are interpreted as RGB values, else, the first data variable is selected.
#' If multiple values are specified, small multiples are drawn (see details). By default, it is the name of the first data variable.
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param palette a palette name or a vector of colors. See \code{tmaptools::palette_explorer()} for the named palettes. Use a \code{"-"} as prefix to reverse the palette. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}, which typically depends on the style. The type of palette from \code{aes.palette} is automatically determined, but can be overwritten: use \code{"seq"} for sequential, \code{"div"} for diverging, and \code{"cat"} for categorical.
#' @param n preferred number of classes (in case \code{col} is a numeric variable)
#' @param style method to process the color scale when \code{col} is a numeric variable. Discrete options are \code{"cat"}, \code{"fixed"}, \code{"sd"}, \code{"equal"}, \code{"pretty"}, \code{"quantile"}, \code{"kmeans"}, \code{"hclust"}, \code{"bclust"}, \code{"fisher"}, and \code{"jenks"}. A numeric variable is processed as a categorial variable when using \code{"cat"}, i.e. each unique value will correspond to a distinct category. For the other discrete options, see the details in \code{\link[classInt:classIntervals]{classIntervals}}. Continuous options are \code{"cont"} and \code{"order"}. The former maps the values of \code{col} to a smooth gradient, whereas the latter maps the order of values of \code{col} to a smooth gradient. They are the continuous variants of respectively the discrete methods "equal" and quantile".
#' @param breaks in case \code{style=="fixed"}, breaks should be specified. The \code{breaks} argument can also be used when \code{style="cont"}. In that case, the breaks are mapped evenly to the sequential or divering color palette.
#' @param interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{col} is a numerc variable.
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values. When categorical color palettes are used, this method stretches the palette if there are more levels than colors.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories} and \code{auto.palette.mapping} is \code{FALSE}, then levels are combined.
#' @param colorNA color used for missing values. Use \code{NULL} for transparency.
#' @param saturation Number that determines how much saturation (also known as chroma) is used: \code{saturation=0} is greyscale and \code{saturation=1} is normal. This saturation value is multiplied by the overall saturation of the map (see \code{\link{tm_layout}}).
#' @param interpolate Should the raster image be interpolated? By default \code{FALSE} for \code{tm_raster} and \code{TRUE} for \code{tm_rgb}.
#' @param textNA text used for missing values.
#' @param showNA logical that determines whether missing values are named in the legend. By default (\code{NA}), this depends on the presence of missing values.
#' @param title title of the legend element
#' @param legend.show logical that determines whether the legend is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.
#' \item{text.to.columns}{Logical that determines whether the text is aligned to three columns (from, text.separator, to). By default \code{FALSE}.}
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.is.portrait logical that determines whether the legend is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.reverse logical that determines whether the items of the legend regarding the text sizes are shown in reverse order, i.e. from bottom to top when \code{legend.is.portrait = TRUE} and from right to left when \code{legend.is.portrait = FALSE}
#' @param legend.hist logical that determines whether a histogram is shown
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend.
#' @param legend.z index value that determines the position of the legend element with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.hist.z index value that determines the position of the histogram legend element 
#' @param ... arguments passed on from \code{tm_raster} to \code{tm_rgb}
#' @name tm_raster
#' @rdname tm_raster
#' @export
#' @example ./examples/tm_raster.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}	
tm_raster <- function(col=NA,
					  alpha = NA,
					  palette = NULL,
					  n = 5,
					  style = ifelse(is.null(breaks), "pretty", "fixed"),
					  breaks = NULL,
					  interval.closure = "left",
					  labels = NULL,
					  auto.palette.mapping = TRUE,
					  contrast = NA,
					  max.categories = 12,
					  colorNA = NULL,
					  saturation = 1,
					  interpolate = FALSE,
					  textNA = "Missing",
					  showNA = NA,
					  title=NA,
					  legend.show=TRUE,
					  legend.format=list(),
					  legend.is.portrait=TRUE,
					  legend.reverse=FALSE,
					  legend.hist=FALSE,
					  legend.hist.title=NA,
					  legend.z=NA,
					  legend.hist.z=NA) {
	g <- list(tm_raster=as.list(environment()))
	g$tm_raster$is.RGB <- FALSE
	class(g) <- "tmap"
	g
}

#' @name tm_rgb
#' @rdname tm_raster
#' @export
tm_rgb <- function(alpha = NA, saturation = 1, interpolate=TRUE, ...) {
	g <- do.call("tm_raster", c(list(alpha=alpha, saturation=saturation, interpolate=interpolate), list(...)))
	g$tm_raster$is.RGB <- TRUE
	class(g) <- "tmap"
	g
}



#' Draw symbols
#' 
#' Creates a \code{\link{tmap-element}} that draws symbols, including symbols and dots. The color, size, and shape of the symbols can be mapped to data variables. 
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments, which are \code{size}, \code{col}, and \code{shape}. In the latter case, the arguments, except for the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' A  shape specification is one of the following three options. To specify multiple shapes (needed for the \code{shapes} argument), a vector or list of these shape specification is required. The shape specification options can also be mixed. For the \code{shapes} argument, it is possible to use a named vector or list, where the names correspond to the value of the variable speficied by the \code{shape} argument.
#' \enumerate{
#'  \item{A numeric value that specifies the plotting character of the symbol. See parameter \code{pch} of \code{\link[graphics:points]{points}} and the last example to create a plot with all options.}
#'  \item{A \code{\link[grid:grid.grob]{grob}} object, which can be a ggplot2 plot object created with \code{\link[ggplot2:ggplotGrob]{ggplotGrob}}. To specify multiple shapes, a list of grob objects is required. See example of a proportional symbol map with ggplot2 plots}.
#'  \item{An icon specification, which can be created with \code{\link{tmap_icons}}.}
#'  }
#'  For small multiples, a list of these shape specification(s) should be provided.
#' 
#' @name tm_symbols
#' @rdname tm_symbols
#' @param size a single value or a \code{shp} data variable that determines the symbol sizes. The reference value \code{size=1} corresponds to the area of symbols that have the same height as one line of text. If a data variable is provided, the symbol sizes are scaled proportionally (or perceptually, see \code{perceptual}) where by default the symbol with the largest data value will get \code{size=1} (see also \code{size.max}). If multiple values are specified, small multiples are drawn (see details).
#' @param col color(s) of the symbol. Either a color (vector), or categorical variable name(s). If multiple values are specified, small multiples are drawn (see details).
#' @param shape shape(s) of the symbol. Either direct shape specification(s) or a data variable name(s) that is mapped to the symbols specified by the \code{shapes} argument. See details for the shape specification.
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param border.col color of the symbol borders.
#' @param border.lwd line width of the symbol borders. If \code{NA}, no symbol borders are drawn.
#' @param border.alpha transparency number, regarding the symbol borders, between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param scale symbol size multiplier number. 
#' @param perceptual logical that determines whether symbols are scales with a perceptually (\code{TRUE}) or mathematically (\code{FALSE}, default value). The perceived area of larger symbols is often underestimated. Flannery (1971) experimentally derived a method to compensate this for symbols, which is enabled by this argument.
#' @param clustering value that determines whether the symbols are clustered in \code{"view"} mode. It does not work proportional bubbles (i.e. \code{tm_bubbles}). One of: \code{TRUE}, \code{FALSE}, or the output of \code{\link[leaflet:markerClusterOptions]{markerClusterOptions}}.
#' @param size.max value that is mapped to \code{size=1}. By default (\code{NA}), the maximum data value is chosen. Only applicable when \code{size} is the name of a numeric variable of \code{shp}
#' @param size.lim vector of two limit values of the \code{size} variable. Only symbols are drawn whose value is greater than or equal to the first value. Symbols whose values exceed the second value are drawn at the size of the second value. Only applicable when \code{size} is the name of a numeric variable of \code{shp}
#' @param sizes.legend vector of symbol sizes that are shown in the legend. By default, this is determined automatically.
#' @param sizes.legend.labels vector of labels for that correspond to \code{sizes.legend}.
#' @param n preferred number of color scale classes. Only applicable when \code{col} is a numeric variable name.
#' @param style method to process the color scale when \code{col} is a numeric variable. Discrete options are \code{"cat"}, \code{"fixed"}, \code{"sd"}, \code{"equal"}, \code{"pretty"}, \code{"quantile"}, \code{"kmeans"}, \code{"hclust"}, \code{"bclust"}, \code{"fisher"}, and \code{"jenks"}. A numeric variable is processed as a categorial variable when using \code{"cat"}, i.e. each unique value will correspond to a distinct category. For the other discrete options, see the details in \code{\link[classInt:classIntervals]{classIntervals}}. Continuous options are \code{"cont"} and \code{"order"}. The former maps the values of \code{col} to a smooth gradient, whereas the latter maps the order of values of \code{col} to a smooth gradient. They are the continuous variants of respectively the discrete methods "equal" and quantile".
#' @param breaks in case \code{style=="fixed"}, breaks should be specified. The \code{breaks} argument can also be used when \code{style="cont"}. In that case, the breaks are mapped evenly to the sequential or divering color palette.
#' @param interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{col} is a numerc variable.
#' @param palette a palette name or a vector of colors. See \code{tmaptools::palette_explorer()} for the named palettes. Use a \code{"-"} as prefix to reverse the palette. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}, which typically depends on the style. The type of palette from \code{aes.palette} is automatically determined, but can be overwritten: use \code{"seq"} for sequential, \code{"div"} for diverging, and \code{"cat"} for categorical.
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values. When categorical color palettes are used, this method stretches the palette if there are more levels than colors.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories} and \code{auto.palette.mapping} is \code{FALSE}, then levels are combined.
#' @param colorNA colour for missing values. Use \code{NULL} for transparency.
#' @param textNA text used for missing values of the color variable.
#' @param showNA logical that determines whether missing values are named in the legend. By default (\code{NA}), this depends on the presence of missing values.
#' @param shapes palette of symbol shapes. Only applicable if \code{shape} is a (vector of) categorical variable(s). See details for the shape specification. By default, the filled symbols 21 to 25 are taken.
#' @param shapes.legend symbol shapes that are used in the legend (instead of the symbols specified with \code{shape}. Especially useful when \code{shapes} consist of grobs that have to be represented by neutrally colored shapes (see also \code{shapes.legend.fill}.
#' @param shapes.legend.fill Fill color of legend shapes (see \code{shapes.legend})
#' @param shapes.labels Legend labels for the symbol shapes
#' @param shapeNA the shape (a number or grob) for missing values. By default a cross (number 4).
#' @param shape.textNA text used for missing values of the shape variable.
#' @param shapes.n preferred number of shape classes. Only applicable when \code{shape} is a numeric variable name.
#' @param shapes.style method to process the shape scale when \code{shape} is a numeric variable. See \code{style} argument for options
#' @param shapes.breaks in case \code{shapes.style=="fixed"}, breaks should be specified
#' @param shapes.interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{shape} is a numerc variable.
#' @param legend.max.symbol.size Maximum size of the symbols that are drawn in the legend. For circles and bubbles, a value larger than one is recommended (and used for \code{tm_bubbles})
#' @param just justification of the symbols relative to the point coordinates.  The first value specifies horizontal and the second value vertical justification. Possible values are: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}. Numeric values of 0 specify left alignment and 1 right alignment. The default value is \code{c("center", "center")}. For icons, this value may already be speficied (see \code{\link{tmap_icons}}). The \code{just}, if specified, will overrides this.
#' @param jitter number that determines the amount of jittering, i.e. the random noise added to the position of the symbols. 0 means no jittering is applied, any positive number means that the random noise has a standard deviation of \code{jitter} times the height of one line of text line.
#' @param xmod horizontal position modification of the symbols, in terms of the height of one line of text. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the symbols. See also \code{jitter} for random position modifications. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the symbols to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @param icon.scale scaling number that determines how large the icons (or grobs) are in plot mode in comparison to proportional symbols (such as bubbles). In view mode, the size is determined by the icon specification (see \code{\link{tmap_icons}}) or, if grobs are specified by \code{grob.width} and \code{grob.heigth}
#' @param grob.dim vector of four values that determine how grob objects (see details) are shown in view mode. The first and second value are the width and height of the displayed icon. The third and fourth value are the width and height of the rendered png image that is used for the icon. Generally, the third and fourth value should be large enough to render a ggplot2 graphic succesfully. Only needed for the view mode.
#' @param title.size title of the legend element regarding the symbol sizes
#' @param title.col title of the legend element regarding the symbol colors
#' @param title.shape title of the legend element regarding the symbol shapes
#' @param legend.size.show logical that determines whether the legend for the symbol sizes is shown
#' @param legend.col.show logical that determines whether the legend for the symbol colors is shown
#' @param legend.shape.show logical that determines whether the legend for the symbol shapes is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.
#' \item{text.to.columns}{Logical that determines whether the text is aligned to three columns (from, text.separator, to). By default \code{FALSE}.}
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.size.is.portrait logical that determines whether the legend element regarding the symbol sizes is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.col.is.portrait logical that determines whether the legend element regarding the symbol colors is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.shape.is.portrait logical that determines whether the legend element regarding the symbol shapes is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.size.reverse logical that determines whether the items of the legend regarding the symbol sizes are shown in reverse order, i.e. from bottom to top when \code{legend.size.is.portrait = TRUE} and from right to left when \code{legend.size.is.portrait = FALSE}
#' @param legend.col.reverse logical that determines whether the items of the legend regarding the symbol colors are shown in reverse order, i.e. from bottom to top when \code{legend.col.is.portrait = TRUE} and from right to left when \code{legend.col.is.portrait = FALSE}
#' @param legend.shape.reverse logical that determines whether the items of the legend regarding the symbol shapes are shown in reverse order, i.e. from bottom to top when \code{legend.shape.is.portrait = TRUE} and from right to left when \code{legend.shape.is.portrait = FALSE}
#' @param legend.hist logical that determines whether a histogram is shown regarding the symbol colors
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend for symbol colors.
#' @param legend.size.z index value that determines the position of the legend element regarding the symbol sizes with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.col.z index value that determines the position of the legend element regarding the symbol colors. (See \code{legend.size.z})
#' @param legend.shape.z index value that determines the position of the legend element regarding the symbol shapes. (See \code{legend.size.z})
#' @param legend.hist.z index value that determines the position of the histogram legend element. (See \code{legend.size.z})
#' @param id name of the data variable that specifies the indices of the symbols. Only used for \code{"view"} mode (see \code{\link{tmap_mode}}).
#' @param popup.vars names of data variables that are shown in the popups in \code{"view"} mode. If \code{NA} (default), only aesthetic variables (i.e. specified by \code{col} and \code{lwd}) are shown). If they are not specified, all variables are shown. Set popup.vars to \code{FALSE} to disable popups. When a vector of variable names is provided, the names (if specified) are printed in the popups.
#' @param popup.format list of formatting options for the popup values. See the argument \code{legend.format} for options. Only applicable for numeric data variables. If one list of formatting options is provided, it is applied to all numeric variables of \code{popup.vars}. Also, a (named) list of lists can be provided. In that case, each list of formatting options is applied to the named variable.
#' @param title shortcut for \code{title.col} for \code{tm_dots}
#' @param legend.show shortcut for \code{legend.col.show} for \code{tm_dots}
#' @param legend.is.portrait shortcut for \code{legend.col.is.portrait} for \code{tm_dots}
#' @param legend.z shortcut for \code{legend.col.z shortcut} for \code{tm_dots}
#' @keywords symbol map
#' @export
#' @example ./examples/tm_symbols.R
#' @references Flannery J (1971). The Relative Effectiveness of Some Common Graduated Point Symbols in the Presentation of Quantitative Data. Canadian Cartographer, 8 (2), 96-109.
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}
tm_symbols <- function(size=1, col=NA,
					   shape=21,
						alpha=NA,
						border.col=NA,
						border.lwd=1,
						border.alpha=NA,
						scale=1,
						perceptual=FALSE,
						clustering=FALSE,
						size.max=NA,
						size.lim=NA,
						sizes.legend = NULL,
						sizes.legend.labels = NULL,
						n = 5, style = ifelse(is.null(breaks), "pretty", "fixed"),
						breaks = NULL,
						interval.closure = "left",
						palette = NULL,
						labels = NULL,
						auto.palette.mapping = TRUE,
						contrast = NA,
						max.categories = 12,
						colorNA = NA,
						textNA = "Missing",
						showNA = NA,
						shapes = 21:25,
						shapes.legend = NULL,
						shapes.legend.fill = NA,
						shapes.labels = NULL,
						shapeNA = 4,
						shape.textNA = "Missing",
						shapes.n = 5, shapes.style = ifelse(is.null(shapes.breaks), "pretty", "fixed"),
						shapes.breaks = NULL,
						shapes.interval.closure = "left",
						legend.max.symbol.size = .8,
						just=NA,
						jitter=0,
						xmod = 0,
						ymod = 0,
						icon.scale = 3,
						grob.dim = c(width=48, height=48, render.width=256, render.height=256),
						title.size = NA,
						title.col = NA,
						title.shape=NA,
						legend.size.show=TRUE,
						legend.col.show=TRUE,
						legend.shape.show=TRUE,
						legend.format=list(),
					   	legend.size.is.portrait=FALSE,
					    legend.col.is.portrait=TRUE,
						legend.shape.is.portrait=TRUE,
						legend.size.reverse=FALSE,
						legend.col.reverse=FALSE,
						legend.shape.reverse=FALSE,
						legend.hist=FALSE,
						legend.hist.title=NA,
						legend.size.z=NA,
						legend.col.z=NA,
						legend.shape.z=NA,
						legend.hist.z=NA,
						id=NA,
						popup.vars=NA,
						popup.format=list()) {
	g <- list(tm_symbols=c(as.list(environment()), list(are.dots=FALSE, are.markers=FALSE, call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tmap"
	g
	
}

#' @rdname tm_symbols
#' @export
tm_squares <- function(size=1, 
					   col=NA,
					   shape=22,
					   scale=4/3,
					   ...) {
	g <- do.call("tm_symbols", c(list(size=size, col=col, shape=shape, scale=scale), list(...)))
	g
}

#' @rdname tm_symbols
#' @export
tm_bubbles <- function(size=1,
					   col=NA,
					   shape=21,
					   scale=4/3,
					   legend.max.symbol.size=1,
					   ...) {
	g <- do.call("tm_symbols", c(list(size=size, col=col, shape=shape, scale=scale, legend.max.symbol.size=legend.max.symbol.size), list(...)))
	g
}



#' @rdname tm_symbols
#' @export
tm_dots <- function(col=NA, 
					size=.02, 
					shape=16,
					title = NA, 
					legend.show=TRUE, 
					legend.is.portrait=TRUE, 
					legend.z=NA, ...) {
	g <- do.call("tm_symbols", c(list(size=size, col=col, shape=shape,
									  title.col=title, 
									  legend.col.show=legend.show,
									  legend.col.is.portrait=legend.is.portrait,
									  legend.col.z=legend.z), list(...)))
	g$tm_symbols$are.dots <- TRUE
	g
}


#' @rdname tm_symbols
#' @param text text of the markers. Shown in plot mode, and as popup text in view mode.
#' @param text.just justification of marker text (see \code{just} argument of \code{\link{tm_text}}). Only applicable in plot mode.
#' @param markers.on.top.of.text For \code{tm_markers}, should the markers be drawn on top of the text labels? 
#' @param ... arguments passed on to \code{tm_symbols}. For \code{tm_markers}, arguments can also be passed on to \code{tm_text}. In that case, they have to be prefixed with \code{text.}, e.g. the \code{col} argument should be names \code{text.col}
#' @export
tm_markers <- function(shape=marker_icon(),
					   col=NA,
					   border.col=NULL,
					   clustering=TRUE,
					   text=NULL,
					   text.just=c("center", "top"),
					   markers.on.top.of.text=TRUE,
					   ...) {
	args <- list(...)
	argsS <- args[intersect(names(formals("tm_symbols")), names(args))]

	# all text label items are preceeded with "text."
	argsT <- args[intersect(paste("text", names(formals("tm_text")), sep="."), names(args))]
	argsT[c("text.text", "text.text.just")] <- NULL # already explicit arguments
	argsT_names <- names(argsT)
	
	names(argsT) <- substr(argsT_names, 6, nchar(argsT_names))
	
	if (is.null(text)) {
		tmT <- NULL
	} else {
		tmT <- do.call("tm_text", c(list(text=text, just=text.just), argsT))
	}
	
	tmS <- do.call("tm_symbols", c(list(shape=shape, col=col, border.col=border.col), argsS))
	
	g <- if (markers.on.top.of.text) {
		tmS + tmT
	} else {
		tmT + tmS
	}
	g$tm_symbols$are.markers <- TRUE
	g
}


