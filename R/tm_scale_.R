#' tmap function to define a constant visual value
#' 
#' tmap function to define a constant visual value
#' 
#' @export
tm_const = function() {
	tmapOption("value.const")
}

#' tmap function to specify all variables in the shape object
#' 
#' tmap function to specify all variables in the shape object
#' 
#' @export
tm_shape_vars = function() {
	structure(list(), class = c("tm_shape_vars", "list"))
}

#' Scales: automatic scale
#' 
#' Scales in tmap are configured by the family of functions with prefix \code{tm_scale}. Such function should be used for the input of the \code{.scale} arguments in the layer functions (e.g. \code{fill.scale} in \code{\link{tm_polygons}}). The function \code{tm_scale} is an scale that is set automatically given by the data type (factor, numeric, and integer) and the visual variable. The tmap option \code{scales.var} contains information which scale is applied when.
#' 
#' @param ... arguments passed on to the applied scale function \code{tm_scale_}
#' @export
tm_scale = function(...) {
	structure(c(list(FUN = "tmapScaleAuto"), list(...)), class = c("tm_scale_auto", "tm_scale", "list"))
}

tm_scale_asis = function(...) {
	structure(c(list(FUN = "tmapScaleAsIs"), list(...)), class = c("tm_scale_asis", "tm_scale", "list"))
}

#' @export
#' @name tm_scale_ordinal
#' @rdname tm_scale_categorical
tm_scale_ordinal = function(n.max = 30,
							values = NA, # e.g. palette, shape numbers, set of sizes (if two values are specified they are interpret as range)
							values.repeat = FALSE,
							values.range  = 1,
							values.scale = 1,
							value.na = NA, # value for NA
							value.null = NA, # value for NULL (needed?)
							value.neutral = NA, # value for neutral (used in other legends)
							levels = NULL, # levels to show (other values are treated as missing)
							levels.drop = FALSE,
							labels = NULL, # labels for the categories (by default the factor levels)
							label.na = NA, # label for missing values
							label.null = NA,
							label.format = NA) {
	structure(c(list(FUN = "tmapScaleCategorical"), as.list(environment())), class = c("tm_scale_ordinal", "tm_scale", "list"))
}

#' Scales: categorical and ordinal scale
#' 
#' Scales in tmap are configured by the family of functions with prefix \code{tm_scale}. Such function should be used for the input of the \code{.scale} arguments in the layer functions (e.g. \code{fill.scale} in \code{\link{tm_polygons}}). The functions \code{tm_scale_categorical} and \code{tm_scale_ordinal} are used for categorical data. The only difference between these functions is that the former assumes unordered categories whereas the latter assumes ordered categories. For colors (the visual variable \code{fill} or \code{col}), different default color palettes are used (see the tmap option \code{values.var}).
#' 
#' @param n.max Maximum number of categories (factor levels). In case there are more, they are grouped into \code{n.max} groups.
#' @param values (generic scale argument) The visual values. For colors (e.g. \code{fill} or \code{col} for \code{tm_polygons}) this is a palette name from the `cols4all` package (see \code{\link[cols4all:c4a]{c4a}}) or vector of colors, for size (e.g. \code{size} for \code{tm_symbols}) these are a set of sizes (if two values are specified they are interpret as range), for symbol shapes (e.g. \code{shape} for \code{\link{tm_symbols}}) these are a set of symbols, etc. The tmap option \code{values.var} contains the default values per visual variable and in some cases also per data type.
#' @param values.repeat (generic scale argument) Should the values be repeated in case there are more categories?
#' @param values.range (generic scale argument) Range of the values. Vector of two numbers (both between 0 and 1) where the first determines the minimum and the second the maximum. Full range, which means that all values are used, is encoded as \code{c(0, 1)}. For instance, when a grey scale is used for color (from black to white), \code{c(0,1)} means that all colors are used, \code{0.25, 0.75} means that only colors from dark grey to light grey are used (more precisely \code{"grey25"} to \code{"grey75"}), and \code{0, 0.5} means that only colors are used from black to middle grey (\code{"grey50"}). When only one number is specified, this is interpreted as the second number (where the first is set to 0). Default values can be set via the tmap option \code{values.range}.
#' @param values.scale (generic scale argument) Scaling of the values. Only useful for size-related visual variables, such as \code{size} of \code{\link{tm_symbols}} and \code{lwd} of \code{\link{tm_lines}}.
#' @param value.na (generic scale argument) Value used for missing values. See tmap option \code{"value.na"} for defaults per visual variable.
#' @param value.null (generic scale argument) Value used for NULL values. See tmap option \code{"value.null"} for defaults per visual variable. Null data values occur when out-of-scope features are shown (e.g. for a map of Europe showing a data variable per country, the null values are applied to countries outside Europe).
#' @param value.neutral (generic scale argument) Value that can be considered neutral. This is used for legends of other visual variables of the same map layer. E.g. when both \code{fill} and \code{size} are used for \code{\link{tm_symbols}} (using filled circles), the size legend items are filled with the \code{value.neutral} color from the \code{fill.scale} scale, and fill legend items are bubbles of size \code{value.neutral} from the \code{size.scale} scale.
#' @param levels Levels to show. Other values are treated as missing.
#' @param levels.drop Should unused levels be dropped (and therefore are not assigned to a visual value and shown in the legend)?
#' @param labels (generic scale argument) Labels
#' @param label.na (generic scale argument) Label for missing values
#' @param label.null (generic scale argument) Label for null (out-of-scope) values
#' @param label.format (generic scale argument) Label formatting (similar to legend.format in tmap3)
#' @export
#' @name tm_scale_categorical
#' @rdname tm_scale_categorical
tm_scale_categorical = function(n.max = 30,
								values = NA, # e.g. palette, shape numbers, set of sizes (if two values are specified they are interpret as range)
								values.repeat = TRUE,
								values.range  = NA,
								values.scale = 1,
								value.na = NA, # value for NA
								value.null = NA, # value for NULL (needed?)
								value.neutral = NA, # value for neutral (used in other legends)
								levels = NULL, # levels to show (other values are treated as missing)
								levels.drop = FALSE,
								labels = NULL, # labels for the categories (by default the factor levels)
								label.na = NA, # label for missing values
								label.null = NA,
								label.format = NA) { # label for null values (needed?)
	structure(c(list(FUN = "tmapScaleCategorical"), as.list(environment())), class = c("tm_scale_categorical", "tm_scale", "list"))
}

#' Scales: interval scale
#' 
#' Scales in tmap are configured by the family of functions with prefix \code{tm_scale}. Such function should be used for the input of the \code{.scale} arguments in the layer functions (e.g. \code{fill.scale} in \code{\link{tm_polygons}}). The function \code{tm_scale_interval} is used for numerical data.
#' 
#' @param n Number of intervals. For some styles (see argument \code{style} below) it is the preferred number rather than the exact number.
#' @param style Method to create intervals. Options are \code{"cat"}, \code{"fixed"}, \code{"sd"}, \code{"equal"}, \code{"pretty"}, \code{"quantile"}, \code{"kmeans"}, \code{"hclust"}, \code{"bclust"}, \code{"fisher"}, \code{"jenks"}, \code{"dpih"}, \code{"headtails"}, and \code{"log10_pretty"}. See the details in \code{\link[classInt:classIntervals]{classIntervals}} (extra arguments can be passed on via \code{style.args}).
#' @param style.args List of extra arguments passed on to \code{\link[classInt:classIntervals]{classIntervals}}.
#' @param breaks Interval breaks (only used and required when \code{style=="fixed"})
#' @param interval.closure value that determines whether where the intervals are closed: \code{"left"} or \code{"right"}. If \code{as.count = TRUE}, \code{inverval.closure} is always set to \code{"left"}.
#' @param midpoint The data value that is interpreted as the midpoint. By default it is set to 0 if negative and positive values are present. Useful when values are diverging colors. In that case, the two sides of the color palette are assigned to negative respectively positive values. If all values are positive or all values are negative, then the midpoint is set to \code{NA}, which means that the value that corresponds to the middle color class (see \code{style}) is mapped to the middle color. If it is specified for sequential color palettes (e.g. \code{"Blues"}), then this color palette will be treated as a diverging color palette.
#' @param as.count Should the data variable be processed as a count variable? For instance, if \code{style = "pretty"}, \code{n = 2}, and the value range of the variable is 0 to 10, then the column classes for \code{as.count = TRUE} are 0; 1 to 5; 6 to 10 (note that 0 is regarded as an own category) whereas for \code{as.count = FALSE} they are 0 to 5; 5 to 10. Only applicable if \code{style} is \code{"pretty"}, \code{"fixed"}, or \code{"log10_pretty"}. By default, \code{TRUE} if \code{style} is one of these, and the variable is an integer. 
#' @param values (generic scale argument) The visual values. For colors (e.g. \code{fill} or \code{col} for \code{\link{tm_polygons}}) this is a palette name from the `cols4all` package (see \code{\link[cols4all:c4a]{c4a}}) or vector of colors, for size (e.g. \code{size} for \code{tm_symbols}) these are a set of sizes (if two values are specified they are interpret as range), for symbol shapes (e.g. \code{shape} for \code{tm_symbols}) these are a set of symbols, etc. The tmap option \code{values.var} contains the default values per visual variable and in some cases also per data type.
#' @param values.repeat (generic scale argument) Should the values be repeated in case there are more categories?
#' @param values.range (generic scale argument) Range of the values. Vector of two numbers (both between 0 and 1) where the first determines the minimum and the second the maximum. Full range, which means that all values are used, is encoded as \code{c(0, 1)}. For instance, when a grey scale is used for color (from black to white), \code{c(0,1)} means that all colors are used, \code{0.25, 0.75} means that only colors from dark grey to light grey are used (more precisely \code{"grey25"} to \code{"grey75"}), and \code{0, 0.5} means that only colors are used from black to middle grey (\code{"grey50"}). When only one number is specified, this is interpreted as the second number (where the first is set to 0). Default values can be set via the tmap option \code{values.range}.
#' @param values.scale (generic scale argument) Scaling of the values. Only useful for size-related visual variables, such as \code{size} of \code{\link{tm_symbols}} and \code{lwd} of \code{\link{tm_lines}}.
#' @param value.na (generic scale argument) Value used for missing values. See tmap option \code{"value.na"} for defaults per visual variable.
#' @param value.null (generic scale argument) Value used for NULL values. See tmap option \code{"value.null"} for defaults per visual variable. Null data values occur when out-of-scope features are shown (e.g. for a map of Europe showing a data variable per country, the null values are applied to countries outside Europe).
#' @param value.neutral (generic scale argument) Value that can be considered neutral. This is used for legends of other visual variables of the same map layer. E.g. when both \code{fill} and \code{size} are used for \code{\link{tm_symbols}} (using filled circles), the size legend items are filled with the \code{value.neutral} color from the \code{fill.scale} scale, and fill legend items are bubbles of size \code{value.neutral} from the \code{size.scale} scale.
#' @param levels Levels to show. Other values are treated as missing.
#' @param levels.drop Should unused levels be dropped (and therefore are not assigned to a visual value and shown in the legend)?
#' @param labels (generic scale argument) Labels
#' @param label.na (generic scale argument) Label for missing values
#' @param label.null (generic scale argument) Label for null (out-of-scope) values
#' @param label.format (generic scale argument) Label formatting (similar to legend.format in tmap3)
#' @export
tm_scale_intervals = function(n = 5, 
							  style = ifelse(is.null(breaks), "pretty", "fixed"),
							  style.args = list(),
							  breaks = NULL,
							  interval.closure = "left",
							  midpoint = NULL,
							  as.count = NA,
							  values = NA,
							  values.repeat = FALSE,
							  values.range  = NA,
							  values.scale = 1,
							  value.na = NA,
							  value.null = NA,
							  value.neutral = NA,
							  labels = NULL,
							  label.na = NA,
							  label.null = NA,
							  label.format = NA) {
	structure(c(list(FUN = "tmapScaleIntervals"), as.list(environment())), class = c("tm_scale_intervals", "tm_scale", "list"))
}

#' Scales: discrete scale
#' 
#' Scales in tmap are configured by the family of functions with prefix \code{tm_scale}. Such function should be used for the input of the \code{.scale} arguments in the layer functions (e.g. \code{fill.scale} in \code{\link{tm_polygons}}). The function \code{tm_scale_discrete} is used for discrete numerical data, such as integers.
#' 
#' @param ticks Discrete values. If not specified, it is determined automatically: unique values are put on a discrete scale.
#' @param midpoint The data value that is interpreted as the midpoint. By default it is set to 0 if negative and positive values are present. Useful when values are diverging colors. In that case, the two sides of the color palette are assigned to negative respectively positive values. If all values are positive or all values are negative, then the midpoint is set to \code{NA}, which means that the value that corresponds to the middle color class (see \code{style}) is mapped to the middle color. If it is specified for sequential color palettes (e.g. \code{"Blues"}), then this color palette will be treated as a diverging color palette.
#' @param as.count Should the data variable be processed as a count variable? For instance, if \code{style = "pretty"}, \code{n = 2}, and the value range of the variable is 0 to 10, then the column classes for \code{as.count = TRUE} are 0; 1 to 5; 6 to 10 (note that 0 is regarded as an own category) whereas for \code{as.count = FALSE} they are 0 to 5; 5 to 10. Only applicable if \code{style} is \code{"pretty"}, \code{"fixed"}, or \code{"log10_pretty"}. By default, \code{TRUE} if \code{style} is one of these, and the variable is an integer. 
#' @param values (generic scale argument) The visual values. For colors (e.g. \code{fill} or \code{col} for \code{\link{tm_polygons}}) this is a palette name from the `cols4all` package (see \code{\link[cols4all:c4a]{c4a}}) or vector of colors, for size (e.g. \code{size} for \code{tm_symbols}) these are a set of sizes (if two values are specified they are interpret as range), for symbol shapes (e.g. \code{shape} for \code{\link{tm_symbols}}) these are a set of symbols, etc. The tmap option \code{values.var} contains the default values per visual variable and in some cases also per data type.
#' @param values.repeat (generic scale argument) Should the values be repeated in case there are more categories?
#' @param values.range (generic scale argument) Range of the values. Vector of two numbers (both between 0 and 1) where the first determines the minimum and the second the maximum. Full range, which means that all values are used, is encoded as \code{c(0, 1)}. For instance, when a grey scale is used for color (from black to white), \code{c(0,1)} means that all colors are used, \code{0.25, 0.75} means that only colors from dark grey to light grey are used (more precisely \code{"grey25"} to \code{"grey75"}), and \code{0, 0.5} means that only colors are used from black to middle grey (\code{"grey50"}). When only one number is specified, this is interpreted as the second number (where the first is set to 0). Default values can be set via the tmap option \code{values.range}.
#' @param values.scale (generic scale argument) Scaling of the values. Only useful for size-related visual variables, such as \code{size} of \code{\link{tm_symbols}} and \code{lwd} of \code{\link{tm_lines}}.
#' @param value.na (generic scale argument) Value used for missing values. See tmap option \code{"value.na"} for defaults per visual variable.
#' @param value.null (generic scale argument) Value used for NULL values. See tmap option \code{"value.null"} for defaults per visual variable. Null data values occur when out-of-scope features are shown (e.g. for a map of Europe showing a data variable per country, the null values are applied to countries outside Europe).
#' @param value.neutral (generic scale argument) Value that can be considered neutral. This is used for legends of other visual variables of the same map layer. E.g. when both \code{fill} and \code{size} are used for \code{\link{tm_symbols}} (using filled circles), the size legend items are filled with the \code{value.neutral} color from the \code{fill.scale} scale, and fill legend items are bubbles of size \code{value.neutral} from the \code{size.scale} scale.
#' @param levels Levels to show. Other values are treated as missing.
#' @param levels.drop Should unused levels be dropped (and therefore are not assigned to a visual value and shown in the legend)?
#' @param labels (generic scale argument) Labels
#' @param label.na (generic scale argument) Label for missing values
#' @param label.null (generic scale argument) Label for null (out-of-scope) values
#' @param label.format (generic scale argument) Label formatting (similar to legend.format in tmap3)
#' @export
tm_scale_discrete = function(ticks = NA,
							 #step = NA,
							 midpoint = NULL,
							 values = NA,
							 values.repeat = FALSE,
							 values.range  = NA,
							 values.scale = 1,
							 value.na = NA,
							 value.null = NA,
							 value.neutral = NA,
							 labels = NULL,
							 label.na = NA,
							 label.null = NA,
							 label.format = NA) {
	structure(c(list(FUN = "tmapScaleDiscrete"), as.list(environment())), class = c("tm_scale_discrete", "tm_scale", "list"))
}

#' Scales: continuous scale
#' 
#' Scales in tmap are configured by the family of functions with prefix \code{tm_scale}. Such function should be used for the input of the \code{.scale} arguments in the layer functions (e.g. \code{fill.scale} in \code{\link{tm_polygons}}). The function \code{tm_scale_continuous} is used for continuous data. The functions \code{tm_scale_continuous_<x>} use transformation functions x.
#' 
#' @param n Preferred number of tick labels. Only used if \code{ticks} is not specified
#' @param limits Limits of the data values that are mapped to the continuous scale
#' @param outliers.trunc Should outliers be truncated? An outlier is a data value that is below or above the respectively lower and upper limit. A logical vector of two values is expected. The first and second value determines whether values lower than the lower limit respectively higher than the upper limit are truncated to the lower respectively upper limit. If `FALSE` (default), they are considered as missing values.
#' @param ticks Tick values. If not specified, it is determined automatically with \code{n}
#' @param trans Transformation function. One of `"identity"` (default), `"log"`, and `"log1p"`. Note: the base of the log scale is irrelevant, since the log transformed values are normalized before mapping to visual values.
#' @param midpoint The data value that is interpreted as the midpoint. By default it is set to 0 if negative and positive values are present. Useful when values are diverging colors. In that case, the two sides of the color palette are assigned to negative respectively positive values. If all values are positive or all values are negative, then the midpoint is set to \code{NA}, which means that the value that corresponds to the middle color class (see \code{style}) is mapped to the middle color. If it is specified for sequential color palettes (e.g. \code{"Blues"}), then this color palette will be treated as a diverging color palette.
#' @param as.count Should the data variable be processed as a count variable? For instance, if \code{style = "pretty"}, \code{n = 2}, and the value range of the variable is 0 to 10, then the column classes for \code{as.count = TRUE} are 0; 1 to 5; 6 to 10 (note that 0 is regarded as an own category) whereas for \code{as.count = FALSE} they are 0 to 5; 5 to 10. Only applicable if \code{style} is \code{"pretty"}, \code{"fixed"}, or \code{"log10_pretty"}. By default, \code{TRUE} if \code{style} is one of these, and the variable is an integer. 
#' @param values (generic scale argument) The visual values. For colors (e.g. \code{fill} or \code{col} for \code{\link{tm_polygons}}) this is a palette name from the `cols4all` package (see \code{\link[cols4all:c4a]{c4a}}) or vector of colors, for size (e.g. \code{size} for \code{\link{tm_symbols}}) these are a set of sizes (if two values are specified they are interpret as range), for symbol shapes (e.g. \code{shape} for \code{\link{tm_symbols}}) these are a set of symbols, etc. The tmap option \code{values.var} contains the default values per visual variable and in some cases also per data type.
#' @param values.repeat (generic scale argument) Should the values be repeated in case there are more categories?
#' @param values.range (generic scale argument) Range of the values, especially useful for color palettes. Vector of two numbers (both between 0 and 1) where the first determines the minimum and the second the maximum. Full range, which means that all values are used, is encoded as \code{c(0, 1)}. For instance, when a grey scale is used for color (from black to white), \code{c(0,1)} means that all colors are used, \code{0.25, 0.75} means that only colors from dark grey to light grey are used (more precisely \code{"grey25"} to \code{"grey75"}), and \code{0, 0.5} means that only colors are used from black to middle grey (\code{"grey50"}). When only one number is specified, this is interpreted as the second number (where the first is set to 0). Default values can be set via the tmap option \code{values.range}.
#' @param values.scale (generic scale argument) Scaling of the values. Only useful for size-related visual variables, such as \code{size} of \code{\link{tm_symbols}} and \code{lwd} of \code{\link{tm_lines}}.
#' @param value.na (generic scale argument) Value used for missing values. See tmap option \code{"value.na"} for defaults per visual variable.
#' @param value.null (generic scale argument) Value used for NULL values. See tmap option \code{"value.null"} for defaults per visual variable. Null data values occur when out-of-scope features are shown (e.g. for a map of Europe showing a data variable per country, the null values are applied to countries outside Europe).
#' @param value.neutral (generic scale argument) Value that can be considered neutral. This is used for legends of other visual variables of the same map layer. E.g. when both \code{fill} and \code{size} are used for \code{\link{tm_symbols}} (using filled circles), the size legend items are filled with the \code{value.neutral} color from the \code{fill.scale} scale, and fill legend items are bubbles of size \code{value.neutral} from the \code{size.scale} scale.
#' @param levels Levels to show. Other values are treated as missing.
#' @param levels.drop Should unused levels be dropped (and therefore are not assigned to a visual value and shown in the legend)?
#' @param labels (generic scale argument) Labels
#' @param label.na (generic scale argument) Label for missing values
#' @param label.null (generic scale argument) Label for null (out-of-scope) values
#' @param label.format (generic scale argument) Label formatting (similar to legend.format in tmap3)
#' @export
#' @rdname tm_scale_continuous
#' @name tm_scale_continuous
tm_scale_continuous = function(n = NULL,
							   limits = NULL,
							   outliers.trunc = NULL,
							   ticks = NULL,
							   trans = NULL,
							   midpoint = NULL,
							   values = NA,
							   values.repeat = FALSE,
							   values.range  = NA,
							   values.scale = 1,
							   value.na = NA,
							   value.null = NA,
							   value.neutral = NA,
							   labels = NULL,
							   label.na = NA,
							   label.null = NA,
							   label.format = NA) {
	
	structure(c(list(FUN = "tmapScaleContinuous"), as.list(environment())), class = c("tm_scale_continuous", "tm_scale", "list"))
}



#' #' @export
#' tm_scale_rank = function(...) {
#' 	tmc = tm_scale_continuous(...)
#' 	class(tmc) = c("tm_scale_rank", "tm_scale", "list")
#' 	tmc
#' }

#' @export
#' @rdname tm_scale_continuous
#' @name tm_scale_continuous_log
tm_scale_continuous_log = function(...) {
	tm_scale_continuous(trans = "log", ...)
}

#' @export
#' @rdname tm_scale_continuous
#' @name tm_scale_continuous_log1p
tm_scale_continuous_log1p = function(...) {
	tm_scale_continuous(trans = "log1p", ...)
}

# 
# #' @export
# #' @rdname tm_scale_continuous
# #' @name tm_scale_continuous_logistic
# tm_scale_continuous_logistic = function(...) {
# 	tm_scale_continuous(trans = "logistic", ...)
# }


#' Scales: RGB
#' 
#' Scales: RGB
#'
#' @param value.na value for missing values
#' @param maxValue maximum value
#' @export
tm_scale_rgb = function(value.na = NA,
						maxValue = 255) {
	structure(c(list(FUN = "tmapScaleRGB"), as.list(environment())), class = c("tm_scale_rgb", "tm_scale", "list"))
}

# tm_scale_na = function() {
# 	structure(c(list(FUN = "tmapScaleNA"), as.list(environment())), class = c("tm_scale_na", "tm_scale", "list"))
# }
