check_deprecated_layer_fun_args <- function(auto.palette.mapping, max.categories, midpoint) {
	tmapOptions = get("tmapOptions", envir = .TMAP_CACHE)
	show.warnings <- tmapOptions$show.warnings
	
	
	if (!is.null(auto.palette.mapping) && show.warnings) {
		warning("The argument auto.palette.mapping is deprecated. Please use midpoint for numeric data and stretch.palette for categorical data to control the palette mapping.", call. = FALSE)
		if (auto.palette.mapping && is.null(midpoint)) midpoint <- 0 # for backwards compatability
	}
	if (!is.null(max.categories) && show.warnings) warning("The argument max.categories is deprecated. It can be specified with tmap_options.", call. = FALSE)
	midpoint
}

#' Add text labels
#' 
#' Creates a \code{\link{tmap-element}} that adds text labels.
#' 
#' @param text name of the variable in the shape object that contains the text labels
#' @param size relative size of the text labels (see note). Either one number, a name of a numeric variable in the shape data that is used to scale the sizes proportionally, or the value \code{"AREA"}, where the text size is proportional to the area size of the polygons.
#' @param col color of the text labels. Either a color value or a data variable name. If multiple values are specified, small multiples are drawn (see details).
#' @param root root number to which the font sizes are scaled. Only applicable if \code{size} is a variable name or \code{"AREA"}. If \code{root=2}, the square root is taken, if \code{root=3}, the cube root etc.
#' @param clustering value that determines whether the text labels are clustered in \code{"view"} mode. One of: \code{TRUE}, \code{FALSE}, or the output of \code{\link[leaflet:markerClusterOptions]{markerClusterOptions}}.
#' @param size.lim vector of two limit values of the \code{size} variable. Only text labels are drawn whose value is greater than or equal to the first value. Text labels whose values exceed the second value are drawn at the size of the second value. Only applicable when \code{size} is the name of a numeric variable of \code{shp}. See also \code{size.lowerbound} which is a threshold of the relative font size.
#' @param sizes.legend vector of text sizes that are shown in the legend. By default, this is determined automatically.
#' @param sizes.legend.labels vector of labels for that correspond to \code{sizes.legend}.
#' @param sizes.legend.text vector of example text to show in the legend next to sizes.legend.labels. By default "Abc". When \code{NA}, examples from the data variable whose sizes are close to the sizes.legend are taken and \code{"NA"} for classes where no match is found.
#' @param n preferred number of color scale classes. Only applicable when \code{col} is a numeric variable name.
#' @param style method to process the color scale when \code{col} is a numeric variable. Discrete gradient options are \code{"cat"}, \code{"fixed"}, \code{"sd"}, \code{"equal"}, \code{"pretty"}, \code{"quantile"}, \code{"kmeans"}, \code{"hclust"}, \code{"bclust"}, \code{"fisher"}, \code{"jenks"}, \code{"dpih"}, \code{"headtails"}, and \code{"log10_pretty"}. A numeric variable is processed as a categorical variable when using \code{"cat"}, i.e. each unique value will correspond to a distinct category. For the other discrete gradient options (except \code{"log10_pretty"}), see the details in \code{\link[classInt:classIntervals]{classIntervals}} (extra arguments can be passed on via \code{style.args}). Continuous gradient options are \code{"cont"}, \code{"order"}, and \code{"log10"}. The first maps the values of \code{col} to a smooth gradient, the second maps the order of values of \code{col} to a smooth gradient, and the third uses a logarithmic transformation. The numeric variable can be either regarded as a continuous variable or a count (integer) variable. See \code{as.count}.
#' @param style.args arguments passed on to \code{\link[classInt:classIntervals]{classIntervals}}, the function that determine color classes (see also \code{style}).
#' @param as.count when \code{col} is a numeric variable, should it be processed as a count variable? For instance, if \code{style = "pretty"}, \code{n = 2}, and the value range of the variable is 0 to 10, then the column classes for \code{as.count = TRUE} are 0; 1 to 5; 6 to 10 (note that 0 is regarded as an own category) whereas for \code{as.count = FALSE} they are 0 to 5; 5 to 10. Only applicable if \code{style} is \code{"pretty"}, \code{"fixed"}, or \code{"log10_pretty"}. By default, \code{TRUE} if \code{style} is one of these, and the variable is an integer. 
#' @param breaks in case \code{style=="fixed"}, breaks should be specified. The \code{breaks} argument can also be used when \code{style="cont"}. In that case, the breaks are mapped evenly to the sequential or diverging color palette.
#' @param interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{col} is a numeric variable. If \code{as.count = TRUE}, \code{inverval.closure} is always set to \code{"left"}.
#' @param palette a palette name or a vector of colors. See \code{tmaptools::palette_explorer()} for the named palettes. Use a \code{"-"} as prefix to reverse the palette. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}, which typically depends on the style. The type of palette from \code{aes.palette} is automatically determined, but can be overwritten: use \code{"seq"} for sequential, \code{"div"} for diverging, and \code{"cat"} for categorical.
#' @param labels labels of the color classes, applicable if \code{col} is a data variable name
#' @param drop.levels should unused color classes be omitted? \code{FALSE} by default.
#' @param labels.text Example text to show in the legend next to the \code{labels}. When \code{NA} (default), examples from the data variable are taken and \code{"NA"} for classes where they don't exist.
#' @param midpoint The value mapped to the middle color of a diverging palette. By default it is set to 0 if negative and positive values are present. In that case, the two sides of the color palette are assigned to negative respectively positive values. If all values are positive or all values are negative, then the midpoint is set to \code{NA}, which means that the value that corresponds to the middle color class (see \code{style}) is mapped to the middle color. Only applies when \code{col} is a numeric variable. If it is specified for sequential color palettes (e.g. \code{"Blues"}), then this color palette will be treated as a diverging color palette.
#' @param stretch.palette Logical that determines whether the categorical color palette should be stretched if there are more categories than colors. If \code{TRUE} (default), interpolated colors are used (like a rainbow). If \code{FALSE}, the palette is repeated.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param colorNA colour for missing values. Use \code{NULL} for transparency.
#' @param textNA text used for missing values. 
#' @param showNA logical that determines whether missing values are named in the legend. By default (\code{NA}), this depends on the presence of missing values.
#' @param colorNULL colour for polygons that are shown on the map that are out of scope 
#' @param fontface font face of the text labels. By default, determined by the fontface argument of \code{\link{tm_layout}}.
#' @param fontfamily font family of the text labels. By default, determined by the fontfamily argument of \code{\link{tm_layout}}.
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{fontcolor} is used (normally 1).
#' @param case case of the font. Use "upper" to generate upper-case text, "lower" to generate lower-case text, and \code{NA} to leave the text as is.
#' @param shadow logical that determines whether a shadow is depicted behind the text. The color of the shadow is either white or yellow, depending of the \code{fontcolor}.
#' @param bg.color background color of the text labels. By default, \code{bg.color=NA}, so no background is drawn.
#' @param bg.alpha number between 0 and 1 that specifies the transparency of the text background (0 is totally transparent, 1 is solid background).
#' @param size.lowerbound lowerbound for \code{size}. Only applicable when \code{size} is not a constant. If \code{print.tiny} is \code{TRUE}, then all text labels which relative text is smaller than \code{size.lowerbound} are depicted at relative size \code{size.lowerbound}. If \code{print.tiny} is \code{FALSE}, then text labels are only depicted if their relative sizes are at least \code{size.lowerbound} (in other words, tiny labels are omitted).
#' @param print.tiny boolean, see \code{size.lowerbound}
#' @param scale text size multiplier, useful in case \code{size} is variable or \code{"AREA"}.
#' @param auto.placement logical (or numeric) that determines whether the labels are placed automatically. If \code{TRUE}, the labels are placed next to the coordinate points with as little overlap as possible using the simulated annealing algorithm. Therefore, it is recommended for labeling spatial dots or symbols. If a numeric value is provided, this value acts as a parameter that specifies the distance between the coordinate points and the text labels in terms of text line heights.
#' @param remove.overlap logical that determines whether the overlapping labels are removed
#' @param along.lines logical that determines whether labels are rotated along the spatial lines. Only applicable if a spatial lines shape is used.
#' @param overwrite.lines logical that determines whether the part of the lines below the text labels is removed. Only applicable if a spatial lines shape is used.
#' @param just justification of the text relative to the point coordinates. Either one of the following values: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}, or a vector of two values where first value specifies horizontal and the second value vertical justification. Besides the mentioned values, also numeric values between 0 and 1 can be used. 0 means left justification for the first value and bottom justification for the second value. Note that in view mode, only one value is used.
#' @param xmod horizontal position modification of the text (relatively): 0 means no modification, and 1 corresponds to the height of one line of text. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the text labels. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the text to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @param title.size title of the legend element regarding the text sizes
#' @param title.col title of the legend element regarding the text colors
#' @param legend.size.show logical that determines whether the legend for the text sizes is shown
#' @param legend.col.show logical that determines whether the legend for the text colors is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientifically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientifically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{big.num.abbr}{Vector that defines whether and which abbrevations are used for large numbers. It is a named numeric vector, where the name indicated the abbreviation, and the number the magnitude (in terms on numbers of zero). Numbers are only abbrevation when they are large enough. Set it to \code{NA} to disable abbrevations.  The default is \code{c("mln" = 6, "bln" = 9)}. For layers where \code{style} is set to \code{log10} or \code{log10_pretty}, the default is \code{NA}.}
#' \item{prefix}{Prefix of each number}
#' \item{suffix}{Suffix of each number}
#' \item{prefix}{Prefix of each number}
#' \item{suffix}{Suffix of each number}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}. By default \code{"left"} for legends in portrait format (\code{legend.is.portrait = TRUE}), and \code{"center"} otherwise.}
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
#' @param id name of the data variable that specifies the indices of the text labels. Only used for \code{"view"} mode (see \code{\link{tmap_mode}}).
#' @param zindex zindex of the pane in view mode. By default, it is set to the layer number plus 400. By default, the tmap layers will therefore be placed in the custom panes \code{"tmap401"}, \code{"tmap402"}, etc., except for the base tile layers, which are placed in the standard \code{"tile"}. This parameter determines both the name of the pane and the z-index, which determines the pane order from bottom to top. For instance, if \code{zindex} is set to 500, the pane will be named \code{"tmap500"}.
#' @param group name of the group to which this layer belongs in view mode. Each group can be selected or deselected in the layer control item. Set \code{group = NULL} to hide the layer in the layer control item. By default, it will be set to the name of the shape (specified in \code{\link{tm_shape}}).
#' @param auto.palette.mapping deprecated. It has been replaced by \code{midpoint} for numeric variables and \code{stretch.palette} for categorical variables.
#' @param max.categories deprecated. It has moved to \code{\link{tmap_options}}.
#' @note The absolute fontsize (in points) is determined by the (ROOT) viewport, which may depend on the graphics device.
#' @export
#' @example ./examples/tm_text.R
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
#' @return \code{\link{tmap-element}}
tm_text <-  function(text, size=1, col=NA, root=3, 
					 clustering=FALSE,
					 size.lim=NA,
					 sizes.legend = NULL,
					 sizes.legend.labels = NULL,
					 sizes.legend.text = "Abc",
					 n = 5, style = ifelse(is.null(breaks), "pretty", "fixed"),
					 style.args = list(),
					 as.count = NA,
					 breaks = NULL,
					 interval.closure = "left",
					 palette = NULL,
					 labels = NULL,
					 drop.levels = FALSE,
					 labels.text = NA,
					 midpoint = NULL,
					 stretch.palette = TRUE,
					 contrast = NA,
					 colorNA = NA,
					 textNA = "Missing",
					 showNA = NA,
					 colorNULL = NA,
					 fontface=NA, 
					 fontfamily=NA, alpha=NA, case=NA, shadow=FALSE, bg.color=NA, bg.alpha=NA, size.lowerbound=.4, print.tiny=FALSE, scale=1, auto.placement=FALSE, remove.overlap=FALSE, along.lines=FALSE, overwrite.lines=FALSE, just="center", xmod=0, ymod=0,
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
					 legend.hist.z=NA,
					 id=NA,
					 zindex = NA,
					 group = NA,
					 auto.palette.mapping = NULL,
					 max.categories = NULL) {
	midpoint <- check_deprecated_layer_fun_args(auto.palette.mapping, max.categories, midpoint)

	g <- list(tm_text=c(as.list(environment()), list(call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tmap"
	g
}
	
#' Draw iso (contour) lines with labels
#' 
#' This function is a wrapper of \code{\link{tm_lines}} and \code{\link{tm_text}} aimed to draw isopleths. 
#' 
#' @param col line color. See \code{\link{tm_lines}}.
#' @param text text to display.
#' @param size text size (see \code{\link{tm_text}})
#' @param remove.overlap see \code{\link{tm_text}}
#' @param along.lines see \code{\link{tm_text}}
#' @param overwrite.lines see \code{\link{tm_text}}
#' @param bg.color background color of the labels. Note: in tmap <= 3.2, the iso lines were cut to make space for labels. In tmap >= 3.3, this is changed: the iso lines remain unchanged, but the labels are printed with a background color by default.
#' @param group name of the group to which this layer belongs in view mode. Each group can be selected or deselected in the layer control item. Set \code{group = NULL} to hide the layer in the layer control item. By default, it will be set to the name of the shape (specified in \code{\link{tm_shape}}).
#' @param ... arguments passed on to \code{\link{tm_lines}} or \code{\link{tm_text}}
#' @export
tm_iso <- function(col=NA, text="level", size=.5, 
				   remove.overlap=TRUE, along.lines=TRUE, overwrite.lines=TRUE,
				   bg.color = tmap_options()$bg.color,
				   group = NA, ...) {
	args <- list(...)
	argsL <- args[intersect(names(formals("tm_lines")), names(args))]
	argsT <- args[intersect(names(formals("tm_text")), names(args))]
	
	do.call("tm_lines", c(list(col=col), argsL)) +
		do.call("tm_text", c(list(text=text, size=size,
								  remove.overlap=remove.overlap,
								  along.lines=along.lines,
								  overwrite.lines = overwrite.lines,
								  bg.color = bg.color),
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
#' @param lwd.legeld.col color of lines that are shown in the legend for the lwd aesthetic. By default, the middle color of the \code{palette} is taken.
#' @param n preferred number of color scale classes. Only applicable when \code{lwd} is the name of a numeric variable.
#' @param style method to process the color scale when \code{col} is a numeric variable. Discrete gradient options are \code{"cat"}, \code{"fixed"}, \code{"sd"}, \code{"equal"}, \code{"pretty"}, \code{"quantile"}, \code{"kmeans"}, \code{"hclust"}, \code{"bclust"}, \code{"fisher"}, \code{"jenks"}, \code{"dpih"}, \code{"headtails"}, and \code{"log10_pretty"}. A numeric variable is processed as a categorical variable when using \code{"cat"}, i.e. each unique value will correspond to a distinct category. For the other discrete gradient options (except \code{"log10_pretty"}), see the details in \code{\link[classInt:classIntervals]{classIntervals}} (extra arguments can be passed on via \code{style.args}). Continuous gradient options are \code{"cont"}, \code{"order"}, and \code{"log10"}. The first maps the values of \code{col} to a smooth gradient, the second maps the order of values of \code{col} to a smooth gradient, and the third uses a logarithmic transformation. The numeric variable can be either regarded as a continuous variable or a count (integer) variable. See \code{as.count}.
#' @param style.args arguments passed on to \code{\link[classInt:classIntervals]{classIntervals}}, the function that determine color classes (see also \code{style}).
#' @param as.count when \code{col} is a numeric variable, should it be processed as a count variable? For instance, if \code{style = "pretty"}, \code{n = 2}, and the value range of the variable is 0 to 10, then the column classes for \code{as.count = TRUE} are 0; 1 to 5; 6 to 10 (note that 0 is regarded as an own category) whereas for \code{as.count = FALSE} they are 0 to 5; 5 to 10. Only applicable if \code{style} is \code{"pretty"}, \code{"fixed"}, or \code{"log10_pretty"}. By default, \code{TRUE} if \code{style} is one of these, and the variable is an integer.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified. The \code{breaks} argument can also be used when \code{style="cont"}. In that case, the breaks are mapped evenly to the sequential or diverging color palette.
#' @param interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{col} is a numeric variable. If \code{as.count = TRUE}, \code{inverval.closure} is always set to \code{"left"}.
#' @param palette a palette name or a vector of colors. See \code{tmaptools::palette_explorer()} for the named palettes. Use a \code{"-"} as prefix to reverse the palette. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}, which typically depends on the style. The type of palette from \code{aes.palette} is automatically determined, but can be overwritten: use \code{"seq"} for sequential, \code{"div"} for diverging, and \code{"cat"} for categorical.
#' @param labels labels of the classes
#' @param drop.levels should unused classes be omitted? \code{FALSE} by default.
#' @param midpoint The value mapped to the middle color of a diverging palette. By default it is set to 0 if negative and positive values are present. In that case, the two sides of the color palette are assigned to negative respectively positive values. If all values are positive or all values are negative, then the midpoint is set to \code{NA}, which means that the value that corresponds to the middle color class (see \code{style}) is mapped to the middle color. Only applies when \code{col} is a numeric variable. If it is specified for sequential color palettes (e.g. \code{"Blues"}), then this color palette will be treated as a diverging color palette.
#' @param stretch.palette Logical that determines whether the categorical color palette should be stretched if there are more categories than colors. If \code{TRUE} (default), interpolated colors are used (like a rainbow). If \code{FALSE}, the palette is repeated.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param colorNA color used for missing values. Use \code{NULL} for transparency.
#' @param textNA text used for missing values.
#' @param showNA logical that determines whether missing values are named in the legend. By default (\code{NA}), this depends on the presence of missing values.
#' @param colorNULL colour for polygons that are shown on the map that are out of scope 
#' @param title.col title of the legend element regarding the line colors
#' @param title.lwd title of the legend element regarding the line widths
#' @param legend.col.show logical that determines whether the legend for the line colors is shown
#' @param legend.lwd.show logical that determines whether the legend for the line widths is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientifically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientifically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{big.num.abbr}{Vector that defines whether and which abbrevations are used for large numbers. It is a named numeric vector, where the name indicated the abbreviation, and the number the magnitude (in terms on numbers of zero). Numbers are only abbrevation when they are large enough. Set it to \code{NA} to disable abbrevations.  The default is \code{c("mln" = 6, "bln" = 9)}. For layers where \code{style} is set to \code{log10} or \code{log10_pretty}, the default is \code{NA}.}
#' \item{prefix}{Prefix of each number}
#' \item{suffix}{Suffix of each number}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.}
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
#' @param interactive logical that determines whether this layer is interactive in view mode (e.g. hover text, popup, and click event in shiny apps)
#' @param popup.vars names of data variables that are shown in the popups in \code{"view"} mode. If \code{NA} (default), only aesthetic variables (i.e. specified by \code{col} and \code{lwd}) are shown). If they are not specified, all variables are shown. Set popup.vars to \code{FALSE} to disable popups. When a vector of variable names is provided, the names (if specified) are printed in the popups.
#' @param popup.format list of formatting options for the popup values. See the argument \code{legend.format} for options. Only applicable for numeric data variables. If one list of formatting options is provided, it is applied to all numeric variables of \code{popup.vars}. Also, a (named) list of lists can be provided. In that case, each list of formatting options is applied to the named variable.
#' @param zindex zindex of the pane in view mode. By default, it is set to the layer number plus 400. By default, the tmap layers will therefore be placed in the custom panes \code{"tmap401"}, \code{"tmap402"}, etc., except for the base tile layers, which are placed in the standard \code{"tile"}. This parameter determines both the name of the pane and the z-index, which determines the pane order from bottom to top. For instance, if \code{zindex} is set to 500, the pane will be named \code{"tmap500"}.
#' @param group name of the group to which this layer belongs in view mode. Each group can be selected or deselected in the layer control item. Set \code{group = NULL} to hide the layer in the layer control item. By default, it will be set to the name of the shape (specified in \code{\link{tm_shape}}).
#' @param auto.palette.mapping deprecated. It has been replaced by \code{midpoint} for numeric variables and \code{stretch.palette} for categorical variables.
#' @param max.categories deprecated. It has moved to \code{\link{tmap_options}}.
#' @param ... these arguments are passed on to \code{\link[classInt:classIntervals]{classIntervals}}, the function that determine color classes (see also \code{style}).
#' @export
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
#' @example ./examples/tm_lines.R
#' @return \code{\link{tmap-element}}
tm_lines <- function(col=NA, lwd=1, lty="solid", alpha=NA,
					 scale=1,
					 lwd.legend = NULL,
					 lwd.legend.labels = NULL,
					 lwd.legeld.col = NA,
					 n = 5, style = ifelse(is.null(breaks), "pretty", "fixed"),
					 style.args = list(),
					 as.count = NA,
					 breaks = NULL,
					 interval.closure = "left",
					 palette = NULL,
					 labels = NULL,
					 drop.levels = FALSE,
					 midpoint = NULL,
					 stretch.palette = TRUE,
					 contrast = NA,
					 colorNA = NA,
					 textNA = "Missing",
					 showNA = NA,
					 colorNULL = NA,
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
					 interactive=TRUE,
					 popup.vars=NA,
					 popup.format=list(),
					 zindex=NA,
					 group = NA,
					 auto.palette.mapping = NULL,
					 max.categories = NULL,
					 ...) {
	midpoint <- check_deprecated_layer_fun_args(auto.palette.mapping, max.categories, midpoint)
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
#' @param style method to process the color scale when \code{col} is a numeric variable. Discrete gradient options are \code{"cat"}, \code{"fixed"}, \code{"sd"}, \code{"equal"}, \code{"pretty"}, \code{"quantile"}, \code{"kmeans"}, \code{"hclust"}, \code{"bclust"}, \code{"fisher"}, \code{"jenks"}, \code{"dpih"}, \code{"headtails"}, and \code{"log10_pretty"}. A numeric variable is processed as a categorical variable when using \code{"cat"}, i.e. each unique value will correspond to a distinct category. For the other discrete gradient options (except \code{"log10_pretty"}), see the details in \code{\link[classInt:classIntervals]{classIntervals}} (extra arguments can be passed on via \code{style.args}). Continuous gradient options are \code{"cont"}, \code{"order"}, and \code{"log10"}. The first maps the values of \code{col} to a smooth gradient, the second maps the order of values of \code{col} to a smooth gradient, and the third uses a logarithmic transformation. The numeric variable can be either regarded as a continuous variable or a count (integer) variable. See \code{as.count}.
#' @param style.args arguments passed on to \code{\link[classInt:classIntervals]{classIntervals}}, the function that determine color classes (see also \code{style}).
#' @param as.count when \code{col} is a numeric variable, should it be processed as a count variable? For instance, if \code{style = "pretty"}, \code{n = 2}, and the value range of the variable is 0 to 10, then the column classes for \code{as.count = TRUE} are 0; 1 to 5; 6 to 10 (note that 0 is regarded as an own category) whereas for \code{as.count = FALSE} they are 0 to 5; 5 to 10. Only applicable if \code{style} is \code{"pretty"}, \code{"fixed"}, or \code{"log10_pretty"}. By default, \code{TRUE} if \code{style} is one of these, and the variable is an integer.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified. The \code{breaks} argument can also be used when \code{style="cont"}. In that case, the breaks are mapped evenly to the sequential or diverging color palette.
#' @param interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{col} is a numeric variable. If \code{as.count = TRUE}, \code{inverval.closure} is always set to \code{"left"}.
#' @param labels labels of the classes.
#' @param drop.levels should unused classes be omitted? \code{FALSE} by default.
#' @param midpoint The value mapped to the middle color of a diverging palette. By default it is set to 0 if negative and positive values are present. In that case, the two sides of the color palette are assigned to negative respectively positive values. If all values are positive or all values are negative, then the midpoint is set to \code{NA}, which means that the value that corresponds to the middle color class (see \code{style}) is mapped to the middle color. Only applies when \code{col} is a numeric variable. If it is specified for sequential color palettes (e.g. \code{"Blues"}), then this color palette will be treated as a diverging color palette.
#' @param stretch.palette Logical that determines whether the categorical color palette should be stretched if there are more categories than colors. If \code{TRUE} (default), interpolated colors are used (like a rainbow). If \code{FALSE}, the palette is repeated.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param colorNA color used for missing values. Use \code{NULL} for transparency.
#' @param textNA text used for missing values.
#' @param showNA logical that determines whether missing values are named in the legend. By default (\code{NA}), this depends on the presence of missing values.
#' @param colorNULL colour for polygons that are shown on the map that are out of scope 
#' @param thres.poly number that specifies the threshold at which polygons are taken into account. The number itself corresponds to the proportion of the area sizes of the polygons to the total polygon size. By default, all polygons are drawn. To ignore polygons that are not visible in a normal plot, a value like \code{1e-05} is recommended.
#' @param title title of the legend element
#' @param legend.show logical that determines whether the legend is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientifically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientifically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{big.num.abbr}{Vector that defines whether and which abbrevations are used for large numbers. It is a named numeric vector, where the name indicated the abbreviation, and the number the magnitude (in terms on numbers of zero). Numbers are only abbrevation when they are large enough. Set it to \code{NA} to disable abbrevations.  The default is \code{c("mln" = 6, "bln" = 9)}. For layers where \code{style} is set to \code{log10} or \code{log10_pretty}, the default is \code{NA}.}
#' \item{prefix}{Prefix of each number}
#' \item{suffix}{Suffix of each number}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.}
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
#' @param interactive logical that determines whether this layer is interactive in view mode (e.g. hover text, popup, and click event in shiny apps)
#' @param popup.vars names of data variables that are shown in the popups in \code{"view"} mode. If \code{convert2density=TRUE}, the derived density variable name is suffixed with \code{_density}. If \code{NA} (default), only aesthetic variables (i.e. specified by \code{col} and \code{lwd}) are shown). If they are not specified, all variables are shown. Set popup.vars to \code{FALSE} to disable popups. When a vector of variable names is provided, the names (if specified) are printed in the popups.
#' @param popup.format list of formatting options for the popup values. See the argument \code{legend.format} for options. Only applicable for numeric data variables. If one list of formatting options is provided, it is applied to all numeric variables of \code{popup.vars}. Also, a (named) list of lists can be provided. In that case, each list of formatting options is applied to the named variable.
#' @param zindex zindex of the pane in view mode. By default, it is set to the layer number plus 400. By default, the tmap layers will therefore be placed in the custom panes \code{"tmap401"}, \code{"tmap402"}, etc., except for the base tile layers, which are placed in the standard \code{"tile"}. This parameter determines both the name of the pane and the z-index, which determines the pane order from bottom to top. For instance, if \code{zindex} is set to 500, the pane will be named \code{"tmap500"}.
#' @param group name of the group to which this layer belongs in view mode. Each group can be selected or deselected in the layer control item. Set \code{group = NULL} to hide the layer in the layer control item. By default, it will be set to the name of the shape (specified in \code{\link{tm_shape}}).
#' @param auto.palette.mapping deprecated. It has been replaced by \code{midpoint} for numeric variables and \code{stretch.palette} for categorical variables.
#' @param max.categories deprecated. It has moved to \code{\link{tmap_options}}.
#' @param ... for \code{tm_polygons}, these arguments passed to either \code{tm_fill} or \code{tm_borders}. For \code{tm_fill}, these arguments are passed on to \code{\link[tmaptools:map_coloring]{map_coloring}}.
#' @concept choropleth
#' @export
#' @example ./examples/tm_fill.R
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
#' @return \code{\link{tmap-element}}	
tm_fill <- function(col=NA, 
					alpha=NA,
				    palette = NULL,
				    convert2density = FALSE,
			 		area = NULL,
				    n = 5,
				    style = ifelse(is.null(breaks), "pretty", "fixed"),
					style.args = list(),
					as.count = NA,
					breaks = NULL,
					interval.closure = "left",
				    labels = NULL,
					drop.levels = FALSE,
					midpoint = NULL,
					stretch.palette = TRUE,
					contrast = NA,
			 		colorNA = NA,
			 		textNA = "Missing",
					showNA = NA,
					colorNULL = NA,
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
					interactive = TRUE,
					popup.vars=NA,
					popup.format=list(),
					zindex=NA,
					group = NA,
					auto.palette.mapping = NULL,
					max.categories = NULL,
					...) {
	midpoint <- check_deprecated_layer_fun_args(auto.palette.mapping, max.categories, midpoint)
	g <- list(tm_fill=c(as.list(environment()), list(extra_args=list(...), call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tmap"
	g
}	


#' @name tm_borders
#' @rdname tm_polygons
#' @param lwd border line width (see \code{\link[graphics:par]{par}})
#' @param lty border line type (see \code{\link[graphics:par]{par}})
#' @export
tm_borders <- function(col=NA, lwd=1, lty="solid", alpha=NA, zindex = NA, group = NA) {
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
						zindex = NA,
						group = NA,
						...) {
	args <- list(...)
	argsFill <- c(list(col=col, alpha=alpha, zindex = zindex, group=group), args[setdiff(names(args), c("lwd", "lty"))])
	argsBorders <- c(list(col=border.col, alpha=border.alpha), args[intersect(names(args), names(formals("tm_borders")))])
	g <- do.call("tm_fill", argsFill) + do.call("tm_borders", argsBorders)
	g$tm_fill$call <- names(match.call(expand.dots = TRUE)[-1])
	g
}


#' Draw a raster
#' 
#' Creates a \code{\link{tmap-element}} that draws a raster. For coloring, there are three options: 1) a fixed color is used, 2) a color palette is mapped to a data variable, 3) RGB values are used. The function \code{tm_raster} is designed for options 1 and 2, while \code{tm_rgb} is used for option 3.
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments. The aesthetic argument of \code{tm_raster} is \code{col}. In the latter case, the arguments, except for the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' @param col three options: the name of a data variable that is contained in \code{shp}, the name of a variable in \code{shp} that contain color values, a single color value. In the first case the values (numeric or categorical) that will be depicted by a color palette (see \code{palette}. If multiple values are specified, small multiples are drawn (see details). By default, it is a vector of the names of all data variables unless the \code{by} argument of \code{\link{tm_facets}} is defined (in that case, the default color of dots is taken from the tmap option \code{aes.color}). If the shape (stars object) contains a third dimension, small multiples are created per 3rd dimension value). Note that the number of small multiples is limited by \code{tmap_options("limits")}).
#' @param r raster band for the red channel. It should be an integer between 1 and the number of raster layers.
#' @param g raster band for the green channel. It should be an integer between 1 and the number of raster layers.
#' @param b raster band for the blue channel. It should be an integer between 1 and the number of raster layers.
#' @param a raster band for the alpha channel. It should be an integer between 1 and the number of raster layers.
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param palette a palette name or a vector of colors. See \code{tmaptools::palette_explorer()} for the named palettes. Use a \code{"-"} as prefix to reverse the palette. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}, which typically depends on the style. The type of palette from \code{aes.palette} is automatically determined, but can be overwritten: use \code{"seq"} for sequential, \code{"div"} for diverging, and \code{"cat"} for categorical.
#' @param n preferred number of classes (in case \code{col} is a numeric variable)
#' @param style method to process the color scale when \code{col} is a numeric variable. Discrete gradient options are \code{"cat"}, \code{"fixed"}, \code{"sd"}, \code{"equal"}, \code{"pretty"}, \code{"quantile"}, \code{"kmeans"}, \code{"hclust"}, \code{"bclust"}, \code{"fisher"}, \code{"jenks"}, \code{"dpih"}, \code{"headtails"}, and \code{"log10_pretty"}. A numeric variable is processed as a categorical variable when using \code{"cat"}, i.e. each unique value will correspond to a distinct category. For the other discrete gradient options (except \code{"log10_pretty"}), see the details in \code{\link[classInt:classIntervals]{classIntervals}} (extra arguments can be passed on via \code{style.args}). Continuous gradient options are \code{"cont"}, \code{"order"}, and \code{"log10"}. The first maps the values of \code{col} to a smooth gradient, the second maps the order of values of \code{col} to a smooth gradient, and the third uses a logarithmic transformation. The numeric variable can be either regarded as a continuous variable or a count (integer) variable. See \code{as.count}.
#' @param style.args arguments passed on to \code{\link[classInt:classIntervals]{classIntervals}}, the function that determine color classes (see also \code{style}).
#' @param as.count when \code{col} is a numeric variable, should it be processed as a count variable? For instance, if \code{style = "pretty"}, \code{n = 2}, and the value range of the variable is 0 to 10, then the column classes for \code{as.count = TRUE} are 0; 1 to 5; 6 to 10 (note that 0 is regarded as an own category) whereas for \code{as.count = FALSE} they are 0 to 5; 5 to 10. Only applicable if \code{style} is \code{"pretty"}, \code{"fixed"}, or \code{"log10_pretty"}. By default, \code{TRUE} if \code{style} is one of these, and the variable is an integer.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified. The \code{breaks} argument can also be used when \code{style="cont"}. In that case, the breaks are mapped evenly to the sequential or diverging color palette.
#' @param interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{col} is a numeric variable. If \code{as.count = TRUE}, \code{inverval.closure} is always set to \code{"left"}.
#' @param labels labels of the classes
#' @param drop.levels should unused classes be omitted? \code{FALSE} by default.
#' @param midpoint The value mapped to the middle color of a diverging palette. By default it is set to 0 if negative and positive values are present. In that case, the two sides of the color palette are assigned to negative respectively positive values. If all values are positive or all values are negative, then the midpoint is set to \code{NA}, which means that the value that corresponds to the middle color class (see \code{style}) is mapped to the middle color. Only applies when \code{col} is a numeric variable. If it is specified for sequential color palettes (e.g. \code{"Blues"}), then this color palette will be treated as a diverging color palette.
#' @param stretch.palette Logical that determines whether the categorical color palette should be stretched if there are more categories than colors. If \code{TRUE} (default), interpolated colors are used (like a rainbow). If \code{FALSE}, the palette is repeated.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param saturation Number that determines how much saturation (also known as chroma) is used: \code{saturation=0} is greyscale and \code{saturation=1} is normal. This saturation value is multiplied by the overall saturation of the map (see \code{\link{tm_layout}}).
#' @param interpolate Should the raster image be interpolated? By default \code{FALSE} for \code{tm_raster} and \code{TRUE} for \code{tm_rgb}.
#' @param colorNA color used for missing values. Use \code{NULL} for transparency.
#' @param textNA text used for missing values.
#' @param showNA logical that determines whether missing values are named in the legend. By default (\code{NA}), this depends on the presence of missing values.
#' @param colorNULL colour for polygons that are shown on the map that are out of scope 
#' @param title title of the legend element
#' @param legend.show logical that determines whether the legend is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientifically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientifically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{big.num.abbr}{Vector that defines whether and which abbrevations are used for large numbers. It is a named numeric vector, where the name indicated the abbreviation, and the number the magnitude (in terms on numbers of zero). Numbers are only abbrevation when they are large enough. Set it to \code{NA} to disable abbrevations.  The default is \code{c("mln" = 6, "bln" = 9)}. For layers where \code{style} is set to \code{log10} or \code{log10_pretty}, the default is \code{NA}.}
#' \item{prefix}{Prefix of each number}
#' \item{suffix}{Suffix of each number}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.}
#' \item{text.to.columns}{Logical that determines whether the text is aligned to three columns (from, text.separator, to). By default \code{FALSE}.}
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param legend.is.portrait logical that determines whether the legend is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.reverse logical that determines whether the items of the legend regarding the text sizes are shown in reverse order, i.e. from bottom to top when \code{legend.is.portrait = TRUE} and from right to left when \code{legend.is.portrait = FALSE}
#' @param legend.hist logical that determines whether a histogram is shown
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend.
#' @param legend.z index value that determines the position of the legend element with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.hist.z index value that determines the position of the histogram legend element 
#' @param zindex zindex of the pane in view mode. By default, it is set to the layer number plus 400. By default, the tmap layers will therefore be placed in the custom panes \code{"tmap401"}, \code{"tmap402"}, etc., except for the base tile layers, which are placed in the standard \code{"tile"}. This parameter determines both the name of the pane and the z-index, which determines the pane order from bottom to top. For instance, if \code{zindex} is set to 500, the pane will be named \code{"tmap500"}.
#' @param group name of the group to which this layer belongs in view mode. Each group can be selected or deselected in the layer control item. Set \code{group = NULL} to hide the layer in the layer control item. By default, it will be set to the name of the shape (specified in \code{\link{tm_shape}}).
#' @param auto.palette.mapping deprecated. It has been replaced by \code{midpoint} for numeric variables and \code{stretch.palette} for categorical variables.
#' @param max.categories deprecated. It has moved to \code{\link{tmap_options}}.
#' @param max.value for \code{tm_rgb}, what is the maximum value per layer? By default 255.
#' @param ... arguments passed on from \code{tm_rgb} and \code{tm_rgba} to \code{tm_raster}.
#' @name tm_raster
#' @rdname tm_raster
#' @export
#' @example ./examples/tm_raster.R
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
#' @return \code{\link{tmap-element}}	
tm_raster <- function(col=NA,
					  alpha = NA,
					  palette = NULL,
					  n = 5,
					  style = ifelse(is.null(breaks), "pretty", "fixed"),
					  style.args = list(),
					  as.count = NA,
					  breaks = NULL,
					  interval.closure = "left",
					  labels = NULL,
					  drop.levels = FALSE,
					  midpoint = NULL,
					  stretch.palette = TRUE,
					  contrast = NA,
					  saturation = 1,
					  interpolate = NA,
					  colorNA = NULL,
					  textNA = "Missing",
					  showNA = NA,
					  colorNULL = NULL,
					  title=NA,
					  legend.show=TRUE,
					  legend.format=list(),
					  legend.is.portrait=TRUE,
					  legend.reverse=FALSE,
					  legend.hist=FALSE,
					  legend.hist.title=NA,
					  legend.z=NA,
					  legend.hist.z=NA,
					  zindex = NA,
					  group = NA,
					  auto.palette.mapping = NULL,
					  max.categories = NULL,
					  max.value = 255) {
	midpoint <- check_deprecated_layer_fun_args(auto.palette.mapping, max.categories, midpoint)
	g <- list(tm_raster=as.list(environment()))
	g$tm_raster$is.RGB <- FALSE
	g$tm_raster$rbg.vars <- NULL
	class(g) <- "tmap"
	g
}

#' @name tm_rgb
#' @rdname tm_raster
#' @export
tm_rgb <- function(r = 1, g = 2, b = 3, alpha = NA, saturation = 1, interpolate=TRUE, max.value = 255, ...) {
	h <- do.call("tm_raster", c(list(alpha=alpha, saturation=saturation, interpolate=interpolate, max.value=max.value), list(...)))
	h$tm_raster$is.RGB <- TRUE
	h$tm_raster$rgb.vars <- c(r, g, b)
	class(h) <- "tmap"
	h
}

#' @name tm_rgba
#' @rdname tm_raster
#' @export
tm_rgba <- function(r = 1, g = 2, b = 3, a = 4, alpha = NA, saturation = 1, interpolate=TRUE, max.value = 255, ...) {
	h <- do.call("tm_raster", c(list(alpha=alpha, saturation=saturation, interpolate=interpolate, max.value=max.value), list(...)))
	h$tm_raster$is.RGB <- TRUE
	h$tm_raster$rgb.vars <- c(r, g, b, a)
	class(h) <- "tmap"
	h
}


#' Draw symbols
#' 
#' Creates a \code{\link{tmap-element}} that draws symbols, including symbols and dots. The color, size, and shape of the symbols can be mapped to data variables. 
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments, which are \code{size}, \code{col}, and \code{shape}. In the latter case, the arguments, except for the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' A  shape specification is one of the following three options.
#' \enumerate{
#'  \item{A numeric value that specifies the plotting character of the symbol. See parameter \code{pch} of \code{\link[graphics:points]{points}} and the last example to create a plot with all options. Note that this is not supported for the \code{"view" mode.}}
#'  \item{A \code{\link[grid:grid.grob]{grob}} object, which can be a ggplot2 plot object created with \code{\link[ggplot2:ggplotGrob]{ggplotGrob}}. To specify multiple shapes, a list of grob objects is required. See example of a proportional symbol map with ggplot2 plots}.
#'  \item{An icon specification, which can be created with \code{\link{tmap_icons}}.}
#'  }
#'  To specify multiple shapes (needed for the \code{shapes} argument), a vector or list of these shape specification is required. The shape specification options can also be mixed. For the \code{shapes} argument, it is possible to use a named vector or list, where the names correspond to the value of the variable specified by the \code{shape} argument.
#'  For small multiples, a list of these shape specification(s) should be provided.
#' 
#' @name tm_symbols
#' @rdname tm_symbols
#' @param size a single value or a \code{shp} data variable that determines the symbol sizes. The reference value \code{size=1} corresponds to the area of symbols that have the same height as one line of text. If a data variable (which should be numeric) is provided, the symbol area sizes are scaled proportionally (or perceptually, see \code{perceptual}) where by default the symbol with the largest data value will get \code{size=1} (see also \code{size.max}). If multiple values are specified, small multiples are drawn (see details).
#' @param col color(s) of the symbol. Either a color (vector), or categorical variable name(s). If multiple values are specified, small multiples are drawn (see details).
#' @param shape shape(s) of the symbol. Either direct shape specification(s) or a data variable name(s) that is mapped to the symbols specified by the \code{shapes} argument. Note that the default shapes (specified by \code{shapes}) is not supported in \code{"view"} mode. See details for the shape specification.
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param border.col color of the symbol borders.
#' @param border.lwd line width of the symbol borders. If \code{NA}, no symbol borders are drawn.
#' @param border.alpha transparency number, regarding the symbol borders, between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param scale symbol size multiplier number. 
#' @param perceptual by default (with \code{perceptual = FALSE}), the symbol area sizes are scaled proportionally to the data variables. This is done by taking the square root of the (normalized) data variable, since the plotting system (\code{grid} package) expects size in radius rather than area. However, the perceived area of larger symbols is often underestimated. Flannery (1971) experimentally derived a method to compensate this for symbols, which is enabled by this argument; if \code{perceptual = TRUE}, not the suqare root (power exponent 0.5) is taken, but power exponent 0.5716.
#' @param clustering value that determines whether the symbols are clustered in \code{"view"} mode. It does not work proportional bubbles (i.e. \code{tm_bubbles}). One of: \code{TRUE}, \code{FALSE}, or the output of \code{\link[leaflet:markerClusterOptions]{markerClusterOptions}}.
#' @param size.max value that is mapped to \code{size=1}. By default (\code{NA}), the maximum data value is chosen. Only applicable when \code{size} is the name of a numeric variable of \code{shp}
#' @param size.lim vector of two limit values of the \code{size} variable. Only symbols are drawn whose value is greater than or equal to the first value. Symbols whose values exceed the second value are drawn at the size of the second value. Only applicable when \code{size} is the name of a numeric variable of \code{shp}
#' @param sizes.legend vector of symbol sizes that are shown in the legend. By default, this is determined automatically.
#' @param sizes.legend.labels vector of labels for that correspond to \code{sizes.legend}.
#' @param n preferred number of color scale classes. Only applicable when \code{col} is a numeric variable name.
#' @param style method to process the color scale when \code{col} is a numeric variable. Discrete gradient options are \code{"cat"}, \code{"fixed"}, \code{"sd"}, \code{"equal"}, \code{"pretty"}, \code{"quantile"}, \code{"kmeans"}, \code{"hclust"}, \code{"bclust"}, \code{"fisher"}, \code{"jenks"}, \code{"dpih"}, \code{"headtails"}, and \code{"log10_pretty"}. A numeric variable is processed as a categorical variable when using \code{"cat"}, i.e. each unique value will correspond to a distinct category. For the other discrete gradient options (except \code{"log10_pretty"}), see the details in \code{\link[classInt:classIntervals]{classIntervals}} (extra arguments can be passed on via \code{style.args}). Continuous gradient options are \code{"cont"}, \code{"order"}, and \code{"log10"}. The first maps the values of \code{col} to a smooth gradient, the second maps the order of values of \code{col} to a smooth gradient, and the third uses a logarithmic transformation. The numeric variable can be either regarded as a continuous variable or a count (integer) variable. See \code{as.count}.
#' @param style.args arguments passed on to \code{\link[classInt:classIntervals]{classIntervals}}, the function that determine color classes (see also \code{style}).
#' @param as.count when \code{col} is a numeric variable, should it be processed as a count variable? For instance, if \code{style = "pretty"}, \code{n = 2}, and the value range of the variable is 0 to 10, then the column classes for \code{as.count = TRUE} are 0; 1 to 5; 6 to 10 (note that 0 is regarded as an own category) whereas for \code{as.count = FALSE} they are 0 to 5; 5 to 10. Only applicable if \code{style} is \code{"pretty"}, \code{"fixed"}, or \code{"log10_pretty"}. By default, \code{TRUE} if \code{style} is one of these, and the variable is an integer.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified. The \code{breaks} argument can also be used when \code{style="cont"}. In that case, the breaks are mapped evenly to the sequential or diverging color palette.
#' @param interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{col} is a numeric variable. If \code{as.count = TRUE}, \code{inverval.closure} is always set to \code{"left"}.
#' @param palette a palette name or a vector of colors. See \code{tmaptools::palette_explorer()} for the named palettes. Use a \code{"-"} as prefix to reverse the palette. The default palette is taken from \code{\link{tm_layout}}'s argument \code{aes.palette}, which typically depends on the style. The type of palette from \code{aes.palette} is automatically determined, but can be overwritten: use \code{"seq"} for sequential, \code{"div"} for diverging, and \code{"cat"} for categorical.
#' @param labels labels of the classes
#' @param drop.levels should unused classes be omitted? \code{FALSE} by default.
#' @param midpoint The value mapped to the middle color of a diverging palette. By default it is set to 0 if negative and positive values are present. In that case, the two sides of the color palette are assigned to negative respectively positive values. If all values are positive or all values are negative, then the midpoint is set to \code{NA}, which means that the value that corresponds to the middle color class (see \code{style}) is mapped to the middle color. Only applies when \code{col} is a numeric variable. If it is specified for sequential color palettes (e.g. \code{"Blues"}), then this color palette will be treated as a diverging color palette.
#' @param stretch.palette Logical that determines whether the categorical color palette should be stretched if there are more categories than colors. If \code{TRUE} (default), interpolated colors are used (like a rainbow). If \code{FALSE}, the palette is repeated.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param colorNA colour for missing values. Use \code{NULL} for transparency.
#' @param textNA text used for missing values of the color variable.
#' @param showNA logical that determines whether missing values are named in the legend. By default (\code{NA}), this depends on the presence of missing values.
#' @param colorNULL colour for polygons that are shown on the map that are out of scope 
#' @param shapes palette of symbol shapes. Only applicable if \code{shape} is a (vector of) categorical variable(s). See details for the shape specification. By default, the filled symbols 21 to 25 are taken.
#' @param shapes.legend symbol shapes that are used in the legend (instead of the symbols specified with \code{shape}). These shapes will be used in the legends regarding the \code{size} and \code{col} of the symbols. Especially useful when \code{shapes} consist of grobs that have to be represented by neutrally colored shapes. See also \code{shapes.legend.fill}.
#' @param shapes.legend.fill Fill color of legend shapes. These colors will be used in the legends regarding the \code{size} and \code{shape} of the symbols. See also \code{shapes.legend}.
#' @param shapes.labels Legend labels for the symbol shapes
#' @param shapes.drop.levels should unused symbol classes be omitted? \code{FALSE} by default.
#' @param shapeNA the shape (a number or grob) for missing values. By default a cross (number 4). Set to \code{NA} to hide symbols for missing values.
#' @param shape.textNA text used for missing values of the shape variable.
#' @param shape.showNA logical that determines whether missing values are named in the legend. By default (\code{NA}), this depends on the presence of missing values.
#' @param shapes.n preferred number of shape classes. Only applicable when \code{shape} is a numeric variable name.
#' @param shapes.style method to process the shape scale when \code{shape} is a numeric variable. See \code{style} argument for options.
#' @param shapes.style.args arguments passed on to \code{\link[classInt:classIntervals]{classIntervals}} (see also \code{shapes.tyle}).
#' @param shapes.as.count when \code{shape} is a numeric variable, should it be processed as a count variable? See \code{as.count} argument for options.
#' @param shapes.breaks in case \code{shapes.style=="fixed"}, breaks should be specified
#' @param shapes.interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. Only applicable if \code{shape} is a numeric variable.
#' @param legend.max.symbol.size Maximum size of the symbols that are drawn in the legend. For circles and bubbles, a value larger than one is recommended (and used for \code{tm_bubbles})
#' @param just justification of the symbols relative to the point coordinates.  The first value specifies horizontal and the second value vertical justification. Possible values are: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}. Numeric values of 0 specify left alignment and 1 right alignment. The default value is \code{c("center", "center")}. For icons, this value may already be speficied (see \code{\link{tmap_icons}}). The \code{just}, if specified, will overrides this.
#' @param jitter number that determines the amount of jittering, i.e. the random noise added to the position of the symbols. 0 means no jittering is applied, any positive number means that the random noise has a standard deviation of \code{jitter} times the height of one line of text line.
#' @param xmod horizontal position modification of the symbols, in terms of the height of one line of text. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the symbols. See also \code{jitter} for random position modifications. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the symbols to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @param icon.scale scaling number that determines how large the icons (or grobs) are in plot mode in comparison to proportional symbols (such as bubbles). In view mode, the size is determined by the icon specification (see \code{\link{tmap_icons}}) or, if grobs are specified by \code{grob.width} and \code{grob.heigth}
#' @param grob.dim vector of four values that determine how grob objects (see details) are shown in view mode. The first and second value are the width and height of the displayed icon. The third and fourth value are the width and height of the rendered png image that is used for the icon. Generally, the third and fourth value should be large enough to render a ggplot2 graphic successfully. Only needed for the view mode.
#' @param title.size title of the legend element regarding the symbol sizes
#' @param title.col title of the legend element regarding the symbol colors
#' @param title.shape title of the legend element regarding the symbol shapes
#' @param legend.size.show logical that determines whether the legend for the symbol sizes is shown
#' @param legend.col.show logical that determines whether the legend for the symbol colors is shown
#' @param legend.shape.show logical that determines whether the legend for the symbol shapes is shown
#' @param legend.format list of formatting options for the legend numbers. Only applicable if \code{labels} is undefined. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientifically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientifically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{big.num.abbr}{Vector that defines whether and which abbrevations are used for large numbers. It is a named numeric vector, where the name indicated the abbreviation, and the number the magnitude (in terms on numbers of zero). Numbers are only abbrevation when they are large enough. Set it to \code{NA} to disable abbrevations.  The default is \code{c("mln" = 6, "bln" = 9)}. For layers where \code{style} is set to \code{log10} or \code{log10_pretty}, the default is \code{NA}.}
#' \item{prefix}{Prefix of each number}
#' \item{suffix}{Suffix of each number}
#' \item{text.separator}{Character string to use to separate numbers in the legend (default: "to").}
#' \item{text.less.than}{Character value(s) to use to translate "Less than". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.or.more}{Character value(s) to use to translate "or more". When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}}
#' \item{text.align}{Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}. By default \code{"left"} for legends in portrait format (\code{legend.is.protrait = TRUE}), and \code{"center"} otherwise.}
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
#' @param interactive logical that determines whether this layer is interactive in view mode (e.g. hover text, popup, and click event in shiny apps)
#' @param popup.vars names of data variables that are shown in the popups in \code{"view"} mode. If \code{NA} (default), only aesthetic variables (i.e. specified by \code{col} and \code{lwd}) are shown). If they are not specified, all variables are shown. Set popup.vars to \code{FALSE} to disable popups. When a vector of variable names is provided, the names (if specified) are printed in the popups.
#' @param popup.format list of formatting options for the popup values. See the argument \code{legend.format} for options. Only applicable for numeric data variables. If one list of formatting options is provided, it is applied to all numeric variables of \code{popup.vars}. Also, a (named) list of lists can be provided. In that case, each list of formatting options is applied to the named variable.
#' @param title shortcut for \code{title.col} for \code{tm_dots}
#' @param legend.show shortcut for \code{legend.col.show} for \code{tm_dots}
#' @param legend.is.portrait shortcut for \code{legend.col.is.portrait} for \code{tm_dots}
#' @param legend.z shortcut for \code{legend.col.z shortcut} for \code{tm_dots}
#' @param zindex zindex of the pane in view mode. By default, it is set to the layer number plus 400. By default, the tmap layers will therefore be placed in the custom panes \code{"tmap401"}, \code{"tmap402"}, etc., except for the base tile layers, which are placed in the standard \code{"tile"}. This parameter determines both the name of the pane and the z-index, which determines the pane order from bottom to top. For instance, if \code{zindex} is set to 500, the pane will be named \code{"tmap500"}.
#' @param group name of the group to which this layer belongs in view mode. Each group can be selected or deselected in the layer control item. Set \code{group = NULL} to hide the layer in the layer control item. By default, it will be set to the name of the shape (specified in \code{\link{tm_shape}}).
#' @param auto.palette.mapping deprecated. It has been replaced by \code{midpoint} for numeric variables and \code{stretch.palette} for categorical variables.
#' @param max.categories deprecated. It has moved to \code{\link{tmap_options}}.
#' @param ... arguments passed on to \code{tm_symbols}. For \code{tm_markers}, arguments can also be passed on to \code{tm_text}. In that case, they have to be prefixed with \code{text.}, e.g. the \code{col} argument should be names \code{text.col}.
#' @concept symbol map
#' @export
#' @example ./examples/tm_symbols.R
#' @references Flannery J (1971). The Relative Effectiveness of Some Common Graduated Point Symbols in the Presentation of Quantitative Data. Canadian Cartographer, 8(2), 96-109.
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}
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
						style.args = list(),
						as.count = NA,
						breaks = NULL,
						interval.closure = "left",
						palette = NULL,
						labels = NULL,
						drop.levels = FALSE,
						midpoint = NULL,
						stretch.palette = TRUE,
						contrast = NA,
						colorNA = NA,
						textNA = "Missing",
						showNA = NA,
						colorNULL = NA,
						shapes = 21:25,
						shapes.legend = NULL,
						shapes.legend.fill = NA,
						shapes.labels = NULL,
						shapes.drop.levels = FALSE,
						shapeNA = 4,
						shape.textNA = "Missing",
						shape.showNA = NA,
						shapes.n = 5, shapes.style = ifelse(is.null(shapes.breaks), "pretty", "fixed"),
						shapes.style.args = list(),
						shapes.as.count = NA,
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
						interactive=TRUE,
						popup.vars=NA,
						popup.format=list(),
						zindex=NA,
						group = NA,
						auto.palette.mapping = NULL,
						max.categories = NULL) {
	midpoint <- check_deprecated_layer_fun_args(auto.palette.mapping, max.categories, midpoint)
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
					shape=19,
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
#' @export
tm_markers <- function(shape=marker_icon(),
					   col=NA,
					   border.col=NULL,
					   clustering=TRUE,
					   text=NULL,
					   text.just="top",
					   markers.on.top.of.text=TRUE,
					   group = NA,
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
		tmT <- do.call("tm_text", c(list(text=text, just=text.just, clustering = clustering), argsT))
	}
	
	tmS <- do.call("tm_symbols", c(list(shape=shape, col=col, border.col=border.col, clustering = clustering), argsS))
	
	g <- if (markers.on.top.of.text) {
		tmS + tmT
	} else {
		tmT + tmS
	}
	g$tm_symbols$are.markers <- TRUE
	g
}

#' Draw simple features
#' 
#' Creates a \code{\link{tmap-element}} that draws simple features. Basically, it is a stack of \code{\link{tm_polygons}}, \code{\link{tm_lines}} and \code{\link{tm_dots}}. In other words, polygons are plotted as polygons, lines as lines and points as dots.

#' @param col color of the simple features. See the \code{col} argument of \code{\link{tm_polygons}}, \code{\link{tm_lines}} and \code{\link{tm_symbols}}.
#' @param size size of the dots. See the \code{size} argument \code{\link{tm_symbols}}. By default, the size is similar to dot size (see \code{\link{tm_dots}})
#' @param shape shape of the dots. See the \code{shape} argument \code{\link{tm_symbols}}. By default, dots are shown.
#' @param lwd width of the lines. See the \code{lwd} argument of \code{\link{tm_lines}}
#' @param lty type of the lines. See the \code{lty} argument of \code{\link{tm_lines}}
#' @param alpha transparency number. See \code{alpha} argument of \code{\link{tm_polygons}}, \code{\link{tm_lines}} and \code{\link{tm_symbols}}
#' @param palette palette. See \code{palette} argument of \code{\link{tm_polygons}}, \code{\link{tm_lines}} and \code{\link{tm_symbols}}
#' @param border.col color of the borders. See \code{border.col} argument of \code{\link{tm_polygons}} and \code{\link{tm_symbols}}. 
#' @param border.lwd line width of the borders. See \code{border.lwd} argument of \code{\link{tm_polygons}} and \code{\link{tm_symbols}}.
#' @param border.lty line type of the borders. See \code{border.lwd} argument of \code{\link{tm_polygons}} and \code{\link{tm_symbols}}.
#' @param border.alpha transparency of the borders. See \code{border.alpha} argument of \code{\link{tm_polygons}} and \code{\link{tm_symbols}}.
#' @param group name of the group to which this layer belongs in view mode. Each group can be selected or deselected in the layer control item. Set \code{group = NULL} to hide the layer in the layer control item. By default, it will be set to the name of the shape (specified in \code{\link{tm_shape}}).
#' @param ... other arguments passed on to \code{\link{tm_polygons}}, \code{\link{tm_lines}} and \code{\link{tm_symbols}}
#' @concept simple features
#' @export
#' @example ./examples/tm_sf.R
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}
#' @return \code{\link{tmap-element}}
tm_sf <- function(col=NA, size=.02, shape = 19, lwd=1, lty = "solid", alpha=NA, palette=NULL, border.col=NA, border.lwd=1, border.lty = "solid", border.alpha=NA, group = NA, ...) {
	args <- list(...)
	
	argsFill <- c(list(col = col, alpha = alpha, palette = palette), args[intersect(names(args), names(formals("tm_fill")))])
	argsBorders <- c(list(col = border.col, alpha = border.alpha, lty = border.lty))
	argsLines <- c(list(col = col, lwd = lwd, lty = lty, alpha = alpha, palette = palette), args[intersect(names(args), names(formals("tm_lines")))])
	argsSymbols <- c(list(col = col, size = size, shape = shape, alpha = alpha, palette = palette, border.col = border.col, border.lwd = border.lwd, border.alpha = border.alpha), args[intersect(names(args), names(formals("tm_symbols")))])
	
	g <- do.call("tm_fill", argsFill) + do.call("tm_borders", argsBorders) + do.call("tm_lines", argsLines) + do.call("tm_symbols", argsSymbols)
	
	called_names <- names(match.call(expand.dots = TRUE)[-1])
	
	
	
	g$tm_fill$call <- called_names
	g$tm_fill$from_tm_sf <- TRUE
	
	g$tm_lines$call <- called_names
	g$tm_symbols$call <- called_names

	g	
}

#' @rdname tm_tiles
#' @export
tm_basemap <- function(server=NA, group = NA, alpha = NA, tms = FALSE) {
	g <- list(tm_basemap=c(as.list(environment()), list(gtype = "base", zindex = NA)))
	class(g) <- "tmap"
	g
}

#' Draw a tile layer
#' 
#' Creates a \code{\link{tmap-element}} that draws a tile layer. This feature is only available in view mode. For plot mode, a tile image can be retrieved by \code{\link[tmaptools:read_osm]{read_osm}}. The function \code{tm_basemap} draws the tile layer as basemap (i.e. as bottom layer), whereas \code{tm_tiles} draws the tile layer as overlay layer (where the stacking order corresponds to the order in which this layer is called). Note that basemaps are shown by default (see details).
#' 
#' When \code{tm_basemap} is not specified, the default basemaps are shown, which can be configured by the \code{basemaps} arugument in \code{\link{tmap_options}}. By default (for style \code{"white"}) three basemaps are drawn: \code{c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldTopoMap")}. To disable basemaps, add \code{tm_basemap(NULL)} to the plot, or set \code{tmap_options(basemaps = NULL)}. Similarly, when \code{tm_tiles} is not specified, the overlay maps specified by the \code{overlays} argument in in \code{\link{tmap_options}} are shown as front layer. By default, this argument is set to \code{NULL}, so no overlay maps are shown by default. See examples.
#' 
#' @param server name of the provider or an URL. The list of available providers can be obtained with \code{providers} (tip: in RStudio, type \code{providers$} to see the options). See \url{https://leaflet-extras.github.io/leaflet-providers/preview/} for a preview of those. When a URL is provided, it should be in template format, e.g. \code{"https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"}. Use \code{NULL} in \code{tm_basemap} to disable the basemaps.
#' @param group name of the group to which this layer belongs in view mode. Each group can be selected or deselected in the layer control item. Set \code{group = NULL} to hide the layer in the layer control item. By default, it will be set to the name of the shape (specified in \code{\link{tm_shape}}). Tile layers generated with \code{tm_basemap} will be base groups whereas tile layers generated with \code{tm_tiles} will be overlay groups.
#' @param alpha alpha
#' @param zindex zindex of the pane in view mode. By default, it is set to the layer number plus 400. By default, the tmap layers will therefore be placed in the custom panes \code{"tmap401"}, \code{"tmap402"}, etc., except for the base tile layers, which are placed in the standard \code{"tile"}. This parameter determines both the name of the pane and the z-index, which determines the pane order from bottom to top. For instance, if \code{zindex} is set to 500, the pane will be named \code{"tmap500"}.
#' @param tms is the provided tile server defined according to the TMS protocol? By default \code{FALSE}.
#' @export
#' @rdname tm_tiles
#' @name tm_tiles
#' @example ./examples/tm_tiles.R
tm_tiles <- function(server, group = NA, alpha = 1, zindex = NA, tms = FALSE) {
	if (missing(server)) stop("Please specify server (name or url)")
	g <- list(tm_tiles=c(as.list(environment()), list(gtype = "overlay")))
	class(g) <- "tmap"
	g
}
