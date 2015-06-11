#' Add text labels
#' 
#' Creates a \code{\link{tmap-element}} that adds text labels.
#' 
#' @param text name of the variable in the shape object that contains the text labels
#' @param size relative size of the text labels (see note). Eiter one number, a name of a numeric variable in the shape data that is used to scale the sizes proportionally, or the value \code{"AREA"}, where the text size is proportional to the area size of the polygons.
#' @param root root number to which the font sizes are scaled. Only applicable if \code{size} is a variable name or \code{"AREA"}. If \code{root=2}, the square root is taken, if \code{root=3}, the cube root etc.
#' @param fontcolor color of the text labels
#' @param fontface font face of the text labels
#' @param fontfamily font family of the text labels
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{fontcolor} is used (normally 1).
#' @param case case of the font. Use "upper" to generate upper-case text, "lower" to generate lower-case text, and \code{NA} to leave the text as is.
#' @param shadow logical that determines whether a shadow is depicted behind the text. The color of the shadow is either white or yellow, depending of the \code{fontcolor}.
#' @param bg.color background color of the text labels. By default, \code{bg.color=NA}, so no background is drawn.
#' @param bg.alpha number between 0 and 1 that specifies the transparancy of the text background (0 is totally transparent, 1 is solid background).
#' @param size.lowerbound lowerbound for \code{size}. Only useful when \code{size} is not a constant. If \code{print.tiny} is \code{TRUE}, then all text labels which relative text is smaller than \code{size.lowerbound} are depicted at relative size \code{size.lowerbound}. If \code{print.tiny} is \code{FALSE}, then text labels are only depicted if their relative sizes are at least \code{size.lowerbound} (in other words, tiny labels are omitted).
#' @param print.tiny boolean, see \code{size.lowerbound}
#' @param scale text size multiplier, useful in case \code{size} is variable or \code{"AREA"}.
#' @param xmod horizontal position modification of the text (relatively): 0 means no modification, and 1 means the total width of the frame. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the text labels. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the text to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @note The absolute fontsize (in points) is determined by the (ROOT) viewport, which may depend on the graphics device.
#' @export
#' @example ../examples/tm_text.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}
tm_text <-  function(text, size=1, root=3, fontcolor=NA, fontface="plain", fontfamily="sans", alpha=NA, case=NA, shadow=FALSE, bg.color=NA, bg.alpha=NA, size.lowerbound=.4, print.tiny=FALSE, scale=1, xmod=0, ymod=0) {
	g <- list(tm_text=list(text=text, text.size=size, root=root, text.fontcolor=fontcolor, text.fontface=fontface, text.fontfamily=fontfamily, text.alpha=alpha, text.case=case, text.shadow=shadow, text.bg.color=bg.color, text.bg.alpha=bg.alpha,
							text.size.lowerbound=size.lowerbound, text.print.tiny=print.tiny, text.scale=scale, text.xmod=xmod, text.ymod=ymod))
	class(g) <- "tmap"
	g
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
#' @param n preferred number of color scale classes. Only applicable when \code{lwd} is the name of a numeric variable.
#' @param style method to cut the color scale: e.g. "fixed", "equal", "pretty", "quantile", or "kmeans". See the details in \code{\link[classInt:classIntervals]{classIntervals}}. Only applicable when \code{lwd} is the name of a numeric variable.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param palette color palette (see \code{RColorBrewer::display.brewer.all}) for the lines. Only when \code{col} is set to a variable.
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values. In this case of line widths, obviously only the positive side is used. 
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories}, then levels are combined.
#' @param colorNA color used for missing values
#' @param textNA text used for missing values. Use \code{NA} to omit text for missing values in the legend
#' @param text_separator Character string to use to separate numbers in the legend (default: "to").
#' @param text_less_than Character string to use to translate "Less than" (which is the default).
#' @param text_or_more Character string to use to translate "or more" (which is the default). 
#' @param title.col title of the legend element regarding the line colors
#' @param title.lwd title of the legend element regarding the line widths
#' @param legend.col.is.portrait logical that determines whether the legend element regarding the line colors is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.lwd.is.portrait logical that determines whether the legend element regarding the line widths is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.hist logical that determines whether a histogram is shown regarding the line colors
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend for line colors.
#' @param legend.col.z index value that determines the position of the legend element regarding the line colors with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.lwd.z index value that determines the position of the legend element regarding the line widths. (See \code{legend.col.z})
#' @param legend.hist.z index value that determines the position of the legend element regarding the histogram. (See \code{legend.col.z})
#' @export
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @example ../examples/tm_lines.R
#' @return \code{\link{tmap-element}}
tm_lines <- function(col="red", lwd=1, lty="solid", alpha=NA,
					  scale=1,
					  n = 5, style = "pretty",
					  breaks = NULL,
					  palette = NULL,
					  labels = NULL,
					  auto.palette.mapping = TRUE,
					  contrast = 1,
					  max.categories = 12, 
					  colorNA = "grey65",
					  textNA = "Missing",
					  text_separator = "to",
					  text_less_than = "Less than",
					  text_or_more = "or more",
					 title.col=NA,
					 title.lwd=NA,
					 legend.col.is.portrait=TRUE,
					 legend.lwd.is.portrait=FALSE,
					 legend.hist=FALSE,
					 legend.hist.title=NA,
					 legend.col.z=NA,
					 legend.lwd.z=NA,
					 legend.hist.z=NA) {
	g <- list(tm_lines=list(lines.col=col, lines.lwd=lwd, lines.lty=lty, lines.alpha=alpha, lines.scale=scale,
							 n=n, style=style, breaks=breaks, palette=palette, labels=labels,
							 auto.palette.mapping=auto.palette.mapping,
							 max.categories=max.categories,
							 contrast=contrast, colorNA=colorNA, textNA=textNA, text_separator=text_separator,
							text_less_than=text_less_than, text_or_more=text_or_more, title.col=title.col, title.lwd=title.lwd, legend.col.is.portrait=legend.col.is.portrait, legend.lwd.is.portrait=legend.lwd.is.portrait, legend.hist=legend.hist, legend.hist.title=legend.hist.title, legend.col.z=legend.col.z, legend.lwd.z=legend.lwd.z, legend.hist.z=legend.hist.z))
	class(g) <- "tmap"
	g
}


#' Draw polygons
#' 
#' Creates a \code{\link{tmap-element}} that draws the polygons. \code{tm_fill} fills the polygons. Either a fixed color is used, or a color palette is mapped to a data variable. By default, a divering color palette is used for numeric variables and a qualitative palette for categorical variables. \code{tm_borders} draws the borders of the polygons. \code{tm_polygons} fills the polygons and draws the polygon borders.
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments. The aesthetic argument of \code{tm_fill} (and \code{tm_polygons}) is \code{col}. In the latter case, the arguments, except for \code{thres.poly}, and the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' @name tm_fill
#' @rdname polygons
#' @param col either a single color value or the name of a data variable that is contained in \code{shp}. In the latter case, either the data variable contains color values, or values (numeric or categorical) that will be depicted by a color palette (see \code{palette}. In the latter case, a choropleth is drawn. If multiple values are specified, small multiples are drawn (see details). For \code{tm_borders}, it is a single color value that specifies the border line color.
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param palette a palette name or a vector of colors. See \code{RColorBrewer::display.brewer.all()} for the named palette. Use a \code{"-"} as prefix to reverse the palette. By default, \code{"RdYlGn"} is taken for numeric variables and \code{"Dark2"} for categorical variables.
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
#' @param text_separator Character string to use to separate numbers in the legend (default: "to").
#' @param text_less_than Character string to use to translate "Less than" (which is the default).
#' @param text_or_more Character string to use to translate "or more" (which is the default). 
#' @param thres.poly number that specifies the threshold at which polygons are taken into account. The number itself corresponds to the proportion of the area sizes of the polygons to the total polygon size. 
#' @param title title of the legend element
#' @param legend.is.portrait logical that determines whether the legend is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.hist logical that determines whether a histogram is shown
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend.
#' @param legend.z index value that determines the position of the legend element with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.hist.z index value that determines the position of the histogram legend element 
#' @keywords choropleth
#' @export
#' @example ../examples/tm_fill.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}	
tm_fill <- function(col="grey85", 
					alpha=NA,
				    palette = NULL,
				    convert2density = FALSE,
			 		area = NULL,
				    n = 5,
				    style = "pretty",
					breaks = NULL,
				    labels = NULL,
					auto.palette.mapping = TRUE,
					contrast = 1,
			 		max.categories = 12,
			 		colorNA = "grey60",
			 		textNA = "Missing",
					text_separator = "to",
					text_less_than = "Less than",
					text_or_more = "or more",
					thres.poly = 1e-05,
					title=NA,
					legend.is.portrait=TRUE,
					legend.hist=FALSE,
					legend.hist.title=NA,
					legend.z=NA,
					legend.hist.z=NA) {
	
	g <- list(tm_fill=as.list(environment()))
	class(g) <- "tmap"
	g
}	


#' @name tm_borders
#' @rdname polygons
#' @param lwd border line width (see \code{\link[graphics:par]{par}})
#' @param lty border line type (see \code{\link[graphics:par]{par}})
#' @export
tm_borders <- function(col="grey40", lwd=1, lty="solid", alpha=NA) {
	g <- list(tm_borders=as.list(environment()))
	class(g) <- "tmap"
	g
}

#' @name tm_polygons
#' @rdname polygons
#' @param border.col border line color
#' @param border.alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param ... arguments passed to either \code{tm_fill} or \code{tm_borders}
#' @export
tm_polygons <- function(col="grey85", 
						alpha=NA,
						border.col="grey40",
						border.alpha=NA,
						...) {
	args <- list(...)
	argsFill <- c(list(col=col, alpha=alpha), args[intersect(names(args), names(formals("tm_fill")))])
	argsBorders <- c(list(col=border.col, alpha=border.alpha), args[intersect(names(args), names(formals("tm_borders")))])
	
	do.call("tm_fill", argsFill) + do.call("tm_borders", argsBorders)
}


#' Draw a raster
#' 
#' Creates a \code{\link{tmap-element}} that draws a raster. Either a fixed color is used, or a color palette is mapped to a data variable. By default, a divering color palette is used for numeric variables and a qualitative palette for categorical variables.
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments. The aesthetic argument of \code{tm_raster} is \code{col}. In the latter case, the arguments, except for \code{thres.poly}, and the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' @param col either a single color value or the name of a data variable that is contained in \code{shp}. In the latter case, either the data variable contains color values, or values (numeric or categorical) that will be depicted by a color palette (see \code{palette}. If multiple values are specified, small multiples are drawn (see details).
#' @param alpha transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{col} is used (normally 1).
#' @param palette palette name. See \code{RColorBrewer::display.brewer.all()} for options. Use a \code{"-"} as prefix to reverse the palette. By default, \code{"RdYlGn"} is taken for numeric variables and \code{"Dark2"} for categorical variables.
#' @param n preferred number of classes (in case \code{col} is a numeric variable)
#' @param style method to cut the color scale (in case \code{col} is a numeric variable): e.g. "fixed", "equal", "pretty", "quantile", or "kmeans". See the details in \code{\link[classInt:classIntervals]{classIntervals}}.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories}, then levels are combined.
#' @param colorNA color used for missing values
#' @param textNA text used for missing values. Use \code{NA} to omit text for missing values in the legend
#' @param text_separator Character string to use to separate numbers in the legend (default: "to").
#' @param text_less_than Character string to use to translate "Less than" (which is the default).
#' @param text_or_more Character string to use to translate "or more" (which is the default). 
#' @param title title of the legend element
#' @param legend.is.portrait logical that determines whether the legend is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.hist logical that determines whether a histogram is shown
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend.
#' @param legend.z index value that determines the position of the legend element with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.hist.z index value that determines the position of the histogram legend element 
#' @export
#' @example ../examples/tm_raster.r
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}	
tm_raster <- function(col="grey70",
					  alpha = NA,
					  palette = NULL,
					  n = 5,
					  style = "pretty",
					  breaks = NULL,
					  labels = NULL,
					  auto.palette.mapping = TRUE,
					  contrast = 1,
					  max.categories = 12,
					  colorNA = NA,
					  textNA = "Missing",
					  text_separator = "to",
					  text_less_than = "Less than",
					  text_or_more = "or more",
					  title=NA,
					  legend.is.portrait=TRUE,
					  legend.hist=FALSE,
					  legend.hist.title=NA,
					  legend.z=NA,
					  legend.hist.z=NA) {
	g <- list(tm_raster=as.list(environment()))
	class(g) <- "tmap"
	g
}

#' Draw bubbles
#' 
#' Creates a \code{\link{tmap-element}} that draws bubbles. Both colors and sizes of the bubbles can be mapped to data variables. 
#' 
#' Small multiples can be drawn in two ways: either by specifying the \code{by} argument in \code{\link{tm_facets}}, or by defining multiple variables in the aesthetic arguments. The aesthetic arguments of \code{tm_bubbles} are \code{size} and \code{col}. In the latter case, the arguments, except for the ones starting with \code{legend.}, can be specified for small multiples as follows. If the argument normally only takes a single value, such as \code{n}, then a vector of those values can be specified, one for each small multiple. If the argument normally can take a vector, such as \code{palette}, then a list of those vectors (or values) can be specified, one for each small multiple.
#' 
#' @param size \code{shp} data variable that determines the bubble sizes. If multiple values are specified, small multiples are drawn (see details).
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
#' @param palette color palette (see \code{RColorBrewer::display.brewer.all}) for the bubbles. Only when \code{col} is set to a variable.
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast vector of two numbers that determine the range that is used for sequential and diverging palettes (applicable when \code{auto.palette.mapping=TRUE}). Both numbers should be between 0 and 1. The first number determines where the palette begins, and the second number where it ends. For sequential palettes, 0 means the brightest color, and 1 the darkest color. For diverging palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories}, then levels are combined.
#' @param colorNA colour for missing values
#' @param textNA text used for missing values. Use \code{NA} to omit text for missing values in the legend
#' @param text_separator Character string to use to separate numbers in the legend (default: "to").
#' @param text_less_than Character string to use to translate "Less than" (which is the default).
#' @param text_or_more Character string to use to translate "or more" (which is the default). 
#' @param xmod horizontal position modification of the bubbles, relatively where 0 means no modification, and 1 means the total width of the frame. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the bubbles. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the bubbles to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @param title.size title of the legend element regarding the bubble sizes
#' @param title.col title of the legend element regarding the bubble colors
#' @param legend.size.is.portrait logical that determines whether the legend element regarding the bubble sizes is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.hist logical that determines whether a histogram is shown regarding the bubble colors
#' @param legend.hist.title title for the histogram. By default, one title is used for both the histogram and the normal legend for bubble colors.
#' @param legend.col.is.portrait logical that determines whether the legend element regarding the bubble colors is in portrait mode (\code{TRUE}) or landscape (\code{FALSE})
#' @param legend.size.z index value that determines the position of the legend element regarding the bubble sizes with respect to other legend elements. The legend elements are stacked according to their z values. The legend element with the lowest z value is placed on top.
#' @param legend.col.z index value that determines the position of the legend element regarding the bubble colors. (See \code{legend.size.z})
#' @param legend.hist.z index value that determines the position of the histogram legend element. (See \code{legend.size.z})
#' @keywords bubble map
#' @export
#' @example ../examples/tm_bubbles.R
#' @references Flannery J (1971). The Relative Effectiveness of Some Common Graduated Point Symbols in the Presentation of Quantitative Data. Canadian Cartographer, 8 (2), 96-109.
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}
tm_bubbles <- function(size=1, col="blueviolet",
						alpha=NA,
						border.col=NA,
						border.lwd=1,
						border.alpha=NA,
						scale=1,
						perceptual=FALSE,
						size.lim=NA,
						sizes.legend = NULL,
						sizes.legend.labels = NULL,
						n = 5, style = "pretty",
						breaks = NULL,
						palette = NULL,
						labels = NULL,
						auto.palette.mapping = TRUE,
						contrast = 1,
						max.categories = 12,
						colorNA = "#FF1414",
						textNA = "Missing",
						text_separator = "to",
						text_less_than = "Less than",
						text_or_more = "or more",
						xmod = 0,
						ymod = 0,
						title.size = NA,
						title.col = NA,
					   	legend.size.is.portrait=FALSE,
					    legend.col.is.portrait=TRUE,
					   	legend.hist=FALSE,
						legend.hist.title=NA,
						legend.size.z=NA,
						legend.col.z=NA,
						legend.hist.z=NA) {
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
							  text_separator=text_separator,
							  text_less_than=text_less_than,
							  text_or_more=text_or_more,
								 bubble.xmod=xmod,
								 bubble.ymod=ymod,
							  title.size=title.size,
							  title.col=title.col,
							  legend.size.is.portrait=legend.size.is.portrait, 
							  legend.col.is.portrait=legend.col.is.portrait, 
							  legend.hist=legend.hist,
							  legend.hist.title=legend.hist.title,
							  legend.size.z=legend.size.z, 
							  legend.col.z=legend.col.z,
							  legend.hist.z=legend.hist.z))
	class(g) <- "tmap"
	g
}



