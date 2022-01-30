#' @export
tm_const = function() {
	tmapOption("value.const")
}

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

#' Scales: categorical and ordinal scale
#' 
#' Scales in tmap are configured by the family of functions with prefix \code{tm_scale}. Such function should be used for the input of the \code{.scale} arguments in the layer functions (e.g. \code{fill.scale} in \code{\link{tm_polygons}}). The functions \code{tm_scale_categorical} and \code{tm_scale_ordinal} are used for categorical data. The only difference between these functions is that the former assumes unordered categories whereas the latter assumes ordered categories. For colors (the visual variable \code{fill} or \code{col}), different default color palettes are used (see the tmap option \code{values.var}).
#' 
#' @param n.max Maximum number of categories (factor levels). In case there are more, they are grouped into \code{n.max} groups.
#' @param values (generic scale argument) The visual values. For colors (e.g. \code{fill} or \code{col} for \code{tm_polygons}) this is a palette name (see \code{\link{tmap_show_palettes}}) or vector of colors, for size (e.g. \code{size} for \code{tm_symbols}) these are a set of sizes (if two values are specified they are interpret as range), for symbol shapes (e.g. \code{shape} for \code{tm_symbols}) these are a set of symbols, etc. The tmap option \code{values.var} contains the default values per visual variable and in some cases also per data type.
#' @param values.repeat (generic scale argument) Should the values be repeated in case there are more categories?
#' @param values.contrast (generic scale argument) Contrast of the values. Vector of two numbers (both between 0 and 1) where the first determines the minimum and the second the maximum. Full contrast, which means that all values are used, is encoded as \code{c(0, 1)}. For instance, when a grey scale is used for color (from black to white), \code{c(0,1)} means that all colors are used, \code{0.25, 0.75} means that only colors from dark grey to light grey are used (more precisely \code{"grey25"} to \code{"grey75"}), and \code{0, 0.5} means that only colors are used from black to middle grey (\code{"grey50"}). When only one number is specified, this is interpreted as the second number (where the first is set to 0). Default values can be set via the tmap option \code{values.contrast}.
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
tm_scale_ordinal = function(n.max = 20,
							values = NA, # e.g. palette, shape numbers, set of sizes (if two values are specified they are interpret as range)
							values.repeat = FALSE,
							values.contrast  = 1,
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



#' @export
tm_scale_categorical = function(n.max = 20,
								values = NA, # e.g. palette, shape numbers, set of sizes (if two values are specified they are interpret as range)
								values.repeat = TRUE,
								values.contrast  = NA,
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
							  values.contrast  = NA,
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

#' @export
tm_scale_discrete = function(ticks = NA,
							 step = NA,
							 midpoint = NULL,
							 values = NA,
							 values.repeat = FALSE,
							 values.contrast  = NA,
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

#' @export
tm_scale_rank = function(...) {
	tmc = tm_scale_continuous(...)
	class(tmc) = c("tm_scale_rank", "tm_scale", "list")
	tmc
}

#' @export
tm_scale_log10 = function(...) {
	tmc = tm_scale_continuous(...)
	class(tmc) = c("tm_scale_log10", "tm_scale", "list")
	tmc
}


#' @export
tm_scale_continuous = function(n = 5,
							   ticks = NULL,
							   midpoint = NULL,
							   values = NA,
							   values.repeat = FALSE,
							   values.contrast  = NA,
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


#' @export
tm_scale_rgb = function(value.na = NA,
						maxValue = 255) {
	structure(c(list(FUN = "tmapScaleRGB"), as.list(environment())), class = c("tm_scale_rgb", "tm_scale", "list"))
}

# tm_scale_na = function() {
# 	structure(c(list(FUN = "tmapScaleNA"), as.list(environment())), class = c("tm_scale_na", "tm_scale", "list"))
# }

