#' tmap function to define a constant visual value
#'
#' tmap function to define a constant visual value
#'
#' @export
tm_const = function() {
	tmapOption("value.const")
}

#' Scales: automatic scale
#'
#' Scales in tmap are configured by the family of functions with prefix `tm_scale`.
#' Such function should be used for the input of the `.scale` arguments in the
#' layer functions (e.g. `fill.scale` in [tm_polygons()]). The function `tm_scale()`
#' is a scale that is set automatically given by the data type (factor, numeric, and integer)
#' and the visual variable. The tmap option `scales.var` contains information
#' which scale is applied when.
#'
#' @param ... arguments passed on to the applied scale function `tm_scale_*()`
#' @seealso [tm_scale_asis()], [tm_scale_ordinal()], [tm_scale_categorical()],
#' [tm_scale_intervals()], [tm_scale_discrete()], [tm_scale_continuous()],
#' [tm_scale_rank()], [tm_scale_continuous_log()], [tm_scale_continuous_log2()],
#' [tm_scale_continuous_log10()], [tm_scale_continuous_log1p()], [tm_scale_continuous_sqrt()],
#' [tm_scale_continuous_pseudo_log()], [tm_scale_rgb()], [tm_scale_bivariate()]
#' @export
tm_scale = function(...) {
	# maybe add the generic scales parameters after ... here?
	structure(c(list(FUN = "tmapScaleAuto"), list(...)), class = c("tm_scale_auto", "tm_scale", "list"))
}

#' Scales: as is
#'
#' Scales in tmap are configured by the family of functions with prefix `tm_scale`.
#' Such function should be used for the input of the `.scale` arguments in the
#' layer functions (e.g. `fill.scale` in [tm_polygons()]).
#' The function `tm_scale_asis()` is used to take data values as they are and use them as such for the visual variable.
#'
#' @inheritParams tm_scale_categorical
#' @param ... Arguments caught (and not used) from the automatic function [tm_scale()]
#' @seealso [tm_scale()]
#' @export
tm_scale_asis = function(values.scale = NA, value.neutral = NA, value.na = NA, ...) {
	structure(c(list(FUN = "tmapScaleAsIs"), c(list(values.scale = values.scale, value.neutral = value.neutral, value.na = value.na), list(...))), class = c("tm_scale_asis", "tm_scale", "list"))
}

#' @export
#' @rdname tm_scale_categorical
tm_scale_ordinal = function(n.max = 30,
							values = NA, # e.g. palette, shape numbers, set of sizes (if two values are specified they are interpret as range)
							values.repeat = FALSE,
							values.range  = 1,
							values.scale = NA,
							value.na = NA, # value for NA
							value.null = NA, # value for NULL (needed?)
							value.neutral = NA, # value for neutral (used in other legends)
							levels = NULL, # levels to show (other values are treated as missing)
							levels.drop = FALSE,
							labels = NULL, # labels for the categories (by default the factor levels)
							label.na = NA, # label for missing values
							label.null = NA,
							label.format = list()) {
	structure(c(list(FUN = "tmapScaleCategorical"), as.list(environment())), class = c("tm_scale_ordinal", "tm_scale", "list"))
}

#' Scales: categorical and ordinal scale
#'
#' Scales in tmap are configured by the family of functions with prefix `tm_scale`.
#' Such function should be used for the input of the `.scale` arguments in the
#' layer functions (e.g. `fill.scale` in [tm_polygons()]).
#' The functions `tm_scale_categorical()` and `tm_scale_ordinal()` are used
#' for categorical data. The only difference between these functions is that the
#' former assumes unordered categories whereas the latter assumes ordered categories.
#' For colors (the visual variable `fill` or `col`), different default color
#' palettes are used (see the tmap option `values.var`).
#'
#' @param n.max Maximum number of categories (factor levels). In case there are more, they are grouped into `n.max` groups.
#' @param values (generic scale argument) The visual values. For colors (e.g. `fill` or `col` for `tm_polygons()`) this is a palette name from the `cols4all` package (see [cols4all::c4a()]) or vector of colors, for size (e.g. `size` for `tm_symbols()`) these are a set of sizes (if two values are specified they are interpret as range), for symbol shapes (e.g. `shape` for [tm_symbols()]) these are a set of symbols, etc. The tmap option `values.var` contains the default values per visual variable and in some cases also per data type.
#' @param values.repeat (generic scale argument) Should the values be repeated in case there are more categories?
#' @param values.range (generic scale argument) Range of the values. Vector of two numbers (both between 0 and 1) where the first determines the minimum and the second the maximum. Full range, which means that all values are used, is encoded as `c(0, 1)`. For instance, when a gray scale is used for color (from black to white), `c(0,1)` means that all colors are used, `0.25, 0.75` means that only colors from dark gray to light gray are used (more precisely `"grey25"` to `"grey75"`), and `0, 0.5` means that only colors are used from black to middle gray (`"gray50"`). When only one number is specified, this is interpreted as the second number (where the first is set to 0). Default values can be set via the tmap option `values.range`.
#' @param values.scale (generic scale argument) Scaling of the values. Only useful for size-related visual variables, such as `size` of [tm_symbols()] and `lwd` of [tm_lines()].
#' @param value.na (generic scale argument) Value used for missing values. See tmap option `"value.na"` for defaults per visual variable.
#' @param value.null (generic scale argument) Value used for NULL values. See tmap option `"value.null"` for defaults per visual variable. Null data values occur when out-of-scope features are shown (e.g. for a map of Europe showing a data variable per country, the null values are applied to countries outside Europe).
#' @param value.neutral (generic scale argument) Value that can be considered neutral. This is used for legends of other visual variables of the same map layer. E.g. when both `fill` and `size` are used for [tm_symbols()] (using filled circles), the size legend items are filled with the `value.neutral` color from the `fill.scale` scale, and fill legend items are bubbles of size `value.neutral` from the `size.scale` scale.
#' @param levels Levels to show. Other values are treated as missing.
#' @param levels.drop Should unused levels be dropped (and therefore are not assigned to a visual value and shown in the legend)?
#' @param labels (generic scale argument) Labels
#' @param label.na (generic scale argument) Label for missing values
#' @param label.null (generic scale argument) Label for null (out-of-scope) values
#' @param label.format (generic scale argument) Label formatting (similar to `legend.format` in tmap3)
#' @seealso [tm_scale()]
#' @export
tm_scale_categorical = function(n.max = 30,
								values = NA, # e.g. palette, shape numbers, set of sizes (if two values are specified they are interpret as range)
								values.repeat = TRUE,
								values.range  = NA,
								values.scale = NA,
								value.na = NA, # value for NA
								value.null = NA, # value for NULL (needed?)
								value.neutral = NA, # value for neutral (used in other legends)
								levels = NULL, # levels to show (other values are treated as missing)
								levels.drop = FALSE,
								labels = NULL, # labels for the categories (by default the factor levels)
								label.na = NA, # label for missing values
								label.null = NA,
								label.format = list()) { # label for null values (needed?)
	structure(c(list(FUN = "tmapScaleCategorical"), as.list(environment())), class = c("tm_scale_categorical", "tm_scale", "list"))
}

#' Scales: interval scale
#'
#' Scales in tmap are configured by the family of functions with prefix `tm_scale`.
#' Such function should be used for the input of the `.scale` arguments in the
#' layer functions (e.g. `fill.scale` in [tm_polygons()]).
#' The function `tm_scale_intervals()` is used for numerical data.
#'
#' @param n Number of intervals. For some styles (see argument `style` below) it is the preferred number rather than the exact number.
#' @param style Method to create intervals. Options are `"cat"`, `"fixed"`, `"sd"`, `"equal"`, `"pretty"`, `"quantile"`, `"kmeans"`, `"hclust"`, `"bclust"`, `"fisher"`, `"jenks"`, `"dpih"`, `"headtails"`, and `"log10_pretty"`. See the details in [classInt::classIntervals()] (extra arguments can be passed on via `style.args`).
#' @param style.args List of extra arguments passed on to [classInt::classIntervals()].
#' @param breaks Interval breaks (only used and required when `style = "fixed"`)
#' @param interval.closure value that determines whether where the intervals are closed: `"left"` or `"right"`. If `as.count = TRUE`, `inverval.closure` is always set to `"left"`.
#' @param midpoint The data value that is interpreted as the midpoint. By default it is set to 0 if negative and positive values are present. Useful when values are diverging colors. In that case, the two sides of the color palette are assigned to negative respectively positive values. If all values are positive or all values are negative, then the midpoint is set to `NA`, which means that the value that corresponds to the middle color class (see `style`) is mapped to the middle color. If it is specified for sequential color palettes (e.g. `"Blues"`), then this color palette will be treated as a diverging color palette.
#' @param as.count Should the data variable be processed as a count variable? For instance, if `style = "pretty"`, `n = 2`, and the value range of the variable is 0 to 10, then the column classes for `as.count = TRUE` are 0; 1 to 5; 6 to 10 (note that 0 is regarded as an own category) whereas for `as.count = FALSE` they are 0 to 5; 5 to 10. Only applicable if `style` is `"pretty"`, `"fixed"`, or `"log10_pretty"`. By default `FALSE`.
#' @inheritParams tm_scale_categorical
#' @param values (generic scale argument) The visual values. For colors (e.g. `fill` or `col` for [tm_polygons()]) this is a palette name from the `cols4all` package (see [cols4all::c4a()]) or vector of colors, for size (e.g. `size` for `tm_symbols`) these are a set of sizes (if two values are specified they are interpret as range), for symbol shapes (e.g. `shape` for `tm_symbols`) these are a set of symbols, etc. The tmap option `values.var` contains the default values per visual variable and in some cases also per data type.
#' @param values.range (generic scale argument) Range of the values. Vector of two numbers (both between 0 and 1) where the first determines the minimum and the second the maximum. Full range, which means that all values are used, is encoded as `c(0, 1)`. For instance, when a gray scale is used for color (from black to white), `c(0,1)` means that all colors are used, `0.25, 0.75` means that only colors from dark gray to light gray are used (more precisely `"gray25"` to `"gray75"`), and `0, 0.5` means that only colors are used from black to middle grey (`"grey50"`). When only one number is specified, this is interpreted as the second number (where the first is set to 0). Default values can be set via the tmap option `values.range`.
#' @param labels (generic scale argument) Labels
#' @param label.na (generic scale argument) Label for missing values
#' @param label.null (generic scale argument) Label for null (out-of-scope) values
#' @param label.format (generic scale argument) Label formatting (similar to legend.format in tmap3)
#' @seealso [tm_scale()]
#' @export
tm_scale_intervals = function(n = 5,
							  style = ifelse(is.null(breaks), "pretty", "fixed"),
							  style.args = list(),
							  breaks = NULL,
							  interval.closure = "left",
							  midpoint = NULL,
							  as.count = FALSE,
							  values = NA,
							  values.repeat = FALSE,
							  values.range  = NA,
							  values.scale = NA,
							  value.na = NA,
							  value.null = NA,
							  value.neutral = NA,
							  labels = NULL,
							  label.na = NA,
							  label.null = NA,
							  label.format = list()) {
	structure(c(list(FUN = "tmapScaleIntervals"), as.list(environment())), class = c("tm_scale_intervals", "tm_scale", "list"))
}

#' Scales: discrete scale
#'
#' Scales in tmap are configured by the family of functions with prefix `tm_scale`.
#' Such function should be used for the input of the `.scale` arguments in the
#' layer functions (e.g. `fill.scale` in [tm_polygons()]).
#' The function [tm_scale_discrete()] is used for discrete numerical data, such as integers.
#'
#' @param ticks Discrete values. If not specified, it is determined automatically: unique values are put on a discrete scale.
#' @param midpoint The data value that is interpreted as the midpoint. By default it is set to 0 if negative and positive values are present. Useful when values are diverging colors. In that case, the two sides of the color palette are assigned to negative respectively positive values. If all values are positive or all values are negative, then the midpoint is set to `NA`, which means that the value that corresponds to the middle color class (see `style`) is mapped to the middle color. If it is specified for sequential color palettes (e.g. `"Blues"`), then this color palette will be treated as a diverging color palette.
#' @param values (generic scale argument) The visual values. For colors (e.g. `fill` or `col` for [tm_polygons()]) this is a palette name from the `cols4all` package (see [cols4all::c4a()]) or vector of colors, for size (e.g. `size` for `tm_symbols`) these are a set of sizes (if two values are specified they are interpret as range), for symbol shapes (e.g. `shape` for [tm_symbols()]) these are a set of symbols, etc. The tmap option `values.var` contains the default values per visual variable and in some cases also per data type.
#' @param values.repeat (generic scale argument) Should the values be repeated in case there are more categories?
#' @param values.range (generic scale argument) Range of the values. Vector of two numbers (both between 0 and 1) where the first determines the minimum and the second the maximum. Full range, which means that all values are used, is encoded as `c(0, 1)`. For instance, when a gray scale is used for color (from black to white), `c(0,1)` means that all colors are used, `0.25, 0.75` means that only colors from dark gray to light gray are used (more precisely `"grey25"` to `"grey75"`), and `0, 0.5` means that only colors are used from black to middle grey (`"grey50"`). When only one number is specified, this is interpreted as the second number (where the first is set to 0). Default values can be set via the tmap option `values.range`.
#' @param values.scale (generic scale argument) Scaling of the values. Only useful for size-related visual variables, such as `size` of [tm_symbols()] and `lwd` of [tm_lines()].
#' @param value.na (generic scale argument) Value used for missing values. See tmap option `"value.na"` for defaults per visual variable.
#' @param value.null (generic scale argument) Value used for NULL values. See tmap option `"value.null"` for defaults per visual variable. Null data values occur when out-of-scope features are shown (e.g. for a map of Europe showing a data variable per country, the null values are applied to countries outside Europe).
#' @param value.neutral (generic scale argument) Value that can be considered neutral. This is used for legends of other visual variables of the same map layer. E.g. when both `fill` and `size` are used for [tm_symbols()] (using filled circles), the size legend items are filled with the `value.neutral` color from the `fill.scale` scale, and fill legend items are bubbles of size `value.neutral` from the `size.scale` scale.
#' @param labels (generic scale argument) Labels
#' @param label.na (generic scale argument) Label for missing values
#' @param label.null (generic scale argument) Label for null (out-of-scope) values
#' @param label.format (generic scale argument) Label formatting (similar to `legend.format` in tmap3)
#' @seealso [tm_scale()]
#' @export
tm_scale_discrete = function(ticks = NA,
							 #step = NA,
							 midpoint = NULL,
							 values = NA,
							 values.repeat = FALSE,
							 values.range  = NA,
							 values.scale = NA,
							 value.na = NA,
							 value.null = NA,
							 value.neutral = NA,
							 labels = NULL,
							 label.na = NA,
							 label.null = NA,
							 label.format = list()) {
	structure(c(list(FUN = "tmapScaleDiscrete"), as.list(environment())), class = c("tm_scale_discrete", "tm_scale", "list"))
}

#' Scales: continuous scale
#'
#' Scales in tmap are configured by the family of functions with prefix `tm_scale`.
#' Such function should be used for the input of the `.scale` arguments in the layer
#' functions (e.g. `fill.scale` in [tm_polygons()]).
#' The function `tm_scale_continuous()` is used for continuous data.
#' The functions `tm_scale_continuous_<x>()` use transformation functions x.
#'
#' @param n Preferred number of tick labels. Only used if `ticks` is not specified
#' @param limits Limits of the data values that are mapped to the continuous scale. When `NA`, the range of data values is taken. When only one value is provided, the range of data values with this provided value is taken. The default depends on the visual variable: it is 0 for all visual variables other than color when `tm_scale_continuous` is used. For the transformation scale functions, it is `NA`.
#' @param outliers.trunc Should outliers be truncated? An outlier is a data value that is below or above the respectively lower and upper limit. A logical vector of two values is expected. The first and second value determines whether values lower than the lower limit respectively higher than the upper limit are truncated to the lower respectively upper limit. If `FALSE` (default), they are considered as missing values.
#' @param ticks Tick values. If not specified, it is determined automatically with `n`
#' @param trans Transformation function. One of `"identity"` (default), `"log"`, and `"log1p"`. Note: the base of the log scale is irrelevant, since the log transformed values are normalized before mapping to visual values.
#' @param midpoint The data value that is interpreted as the midpoint. By default it is set to 0 if negative and positive values are present. Useful when values are diverging colors. In that case, the two sides of the color palette are assigned to negative respectively positive values. If all values are positive or all values are negative, then the midpoint is set to `NA`, which means that the value that corresponds to the middle color class (see `style`) is mapped to the middle color. If it is specified for sequential color palettes (e.g. `"Blues"`), then this color palette will be treated as a diverging color palette.
#' @param values (generic scale argument) The visual values. For colors (e.g. `fill` or `col` for [tm_polygons()]) this is a palette name from the `cols4all` package (see [cols4all::c4a()]) or vector of colors, for size (e.g. `size` for [tm_symbols()]) these are a set of sizes (if two values are specified they are interpret as range), for symbol shapes (e.g. `shape` for [tm_symbols()]) these are a set of symbols, etc. The tmap option `values.var` contains the default values per visual variable and in some cases also per data type.
#' @param values.repeat (generic scale argument) Should the values be repeated in case there are more categories?
#' @param values.range (generic scale argument) Range of the values, especially useful for color palettes. Vector of two numbers (both between 0 and 1) where the first determines the minimum and the second the maximum. Full range, which means that all values are used, is encoded as `c(0, 1)`. For instance, when a gray scale is used for color (from black to white), `c(0,1)` means that all colors are used, `0.25, 0.75` means that only colors from dark gray to light gray are used (more precisely `"grey25"` to `"grey75"`), and `0, 0.5` means that only colors are used from black to middle gray (`"grey50"`). When only one number is specified, this is interpreted as the second number (where the first is set to 0). Default values can be set via the tmap option `values.range`.
#' @param values.scale (generic scale argument) Scaling of the values. Only useful for size-related visual variables, such as `size` of [tm_symbols()] and `lwd` of [tm_lines()].
#' @param value.na (generic scale argument) Value used for missing values. See tmap option `"value.na"` for defaults per visual variable.
#' @param value.null (generic scale argument) Value used for NULL values. See tmap option `"value.null"` for defaults per visual variable. Null data values occur when out-of-scope features are shown (e.g. for a map of Europe showing a data variable per country, the null values are applied to countries outside Europe).
#' @param value.neutral (generic scale argument) Value that can be considered neutral. This is used for legends of other visual variables of the same map layer. E.g. when both `fill` and `size` are used for [tm_symbols()] (using filled circles), the size legend items are filled with the `value.neutral` color from the `fill.scale` scale, and fill legend items are bubbles of size `value.neutral` from the `size.scale` scale.
#' @param labels (generic scale argument) Labels
#' @param label.na (generic scale argument) Label for missing values
#' @param label.null (generic scale argument) Label for null (out-of-scope) values
#' @param label.format (generic scale argument) Label formatting (similar to `legend.format` in tmap3)
#' @param trans.args list of additional argument for the transformation (generic transformation arguments)
#' @inheritParams scales::transform_pseudo_log
#' @example ./examples/tm_scale_continuous.R
#' @seealso [tm_scale()]
#' @export
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
							   values.scale = NA,
							   value.na = NA,
							   value.null = NA,
							   value.neutral = NA,
							   labels = NULL,
							   label.na = NA,
							   label.null = NA,
							   label.format = list(),
							   trans.args = list()) {

	structure(c(list(FUN = "tmapScaleContinuous"), as.list(environment())), class = c("tm_scale_continuous", "tm_scale", "list"))
}


#' Scales: rank scale
#'
#' Scales in tmap are configured by the family of functions with prefix `tm_scale`.
#' Such function should be used for the input of the `.scale` arguments in the layer
#' functions (e.g. `fill.scale` in [tm_polygons()]).
#' The function [tm_scale_rank()] is used to rank numeric data.
#'
#' @param n Preferred number of tick labels. Only used if `ticks` is not specified
#' @param ticks Tick values. If not specified, it is determined automatically with `n`
#' @param values (generic scale argument) The visual values. For colors (e.g. `fill` or `col` for [tm_polygons()]) this is a palette name from the `cols4all` package (see [cols4all::c4a()]) or vector of colors, for size (e.g. `size` for [tm_symbols()]) these are a set of sizes (if two values are specified they are interpret as range), for symbol shapes (e.g. `shape` for [tm_symbols()]) these are a set of symbols, etc. The tmap option `values.var` contains the default values per visual variable and in some cases also per data type.
#' @param values.repeat (generic scale argument) Should the values be repeated in case there are more categories?
#' @param values.range (generic scale argument) Range of the values, especially useful for color palettes. Vector of two numbers (both between 0 and 1) where the first determines the minimum and the second the maximum. Full range, which means that all values are used, is encoded as `c(0, 1)`. For instance, when a gray scale is used for color (from black to white), `c(0,1)` means that all colors are used, `0.25, 0.75` means that only colors from dark gray to light gray are used (more precisely `"grey25"` to `"grey75"`), and `0, 0.5` means that only colors are used from black to middle gray (`"grey50"`). When only one number is specified, this is interpreted as the second number (where the first is set to 0). Default values can be set via the tmap option `values.range`.
#' @param values.scale (generic scale argument) Scaling of the values. Only useful for size-related visual variables, such as `size` of [tm_symbols()] and `lwd` of [tm_lines()].
#' @param value.na (generic scale argument) Value used for missing values. See tmap option `"value.na"` for defaults per visual variable.
#' @param value.null (generic scale argument) Value used for NULL values. See tmap option `"value.null"` for defaults per visual variable. Null data values occur when out-of-scope features are shown (e.g. for a map of Europe showing a data variable per country, the null values are applied to countries outside Europe).
#' @param value.neutral (generic scale argument) Value that can be considered neutral. This is used for legends of other visual variables of the same map layer. E.g. when both `fill` and `size` are used for [tm_symbols()] (using filled circles), the size legend items are filled with the `value.neutral` color from the `fill.scale` scale, and fill legend items are bubbles of size `value.neutral` from the `size.scale` scale.
#' @param labels (generic scale argument) Labels
#' @param label.na (generic scale argument) Label for missing values
#' @param label.null (generic scale argument) Label for null (out-of-scope) values
#' @param label.format (generic scale argument) Label formatting (similar to `legend.format` in tmap3)
#' @param unit The unit name of the values. By default `"rank"`.
#' @seealso [tm_scale()]
#' @export
tm_scale_rank = function(n = NULL,
						 ticks = NULL,
						 values = NA,
						 values.repeat = FALSE,
						 values.range  = NA,
						 values.scale = NA,
						 value.na = NA,
						 value.null = NA,
						 value.neutral = NA,
						 labels = NULL,
						 label.na = NA,
						 label.null = NA,
						 label.format = list(),
						 unit = "rank") {
	structure(c(list(FUN = "tmapScaleRank"), as.list(environment())), class = c("tm_scale_rank", "tm_scale", "list"))
}


#' #' @export
#' tm_scale_rank = function(...) {
#' 	tmc = tm_scale_continuous(...)
#' 	class(tmc) = c("tm_scale_rank", "tm_scale", "list")
#' 	tmc
#' }

#' @export
#' @param ... passed on to [tm_scale_continuous()]
#' @rdname tm_scale_continuous
tm_scale_continuous_log = function(..., base = exp(1)) {
	tm_scale_continuous(trans = "log", trans.args = list(base = base), ...)
}

#' @export
#' @rdname tm_scale_continuous
tm_scale_continuous_log2 = function(...) {
	tm_scale_continuous(trans = "log", trans.args = list(base = 2), ...)
}

#' @export
#' @rdname tm_scale_continuous
tm_scale_continuous_log10 = function(...) {
	x = tm_scale_continuous(trans = "log", trans.args = list(base = 10), ...)
	class(x) = c("tm_scale_continuous_log10", "tm_scale", "list")
	x
}


#' @export
#' @rdname tm_scale_continuous
tm_scale_continuous_log1p = function(...) {
	tm_scale_continuous(trans = "log1p", ...)
}

#' @export
#' @rdname tm_scale_continuous
tm_scale_continuous_sqrt = function(...) {
	tm_scale_continuous(trans = "sqrt", ...)
}

#' @export
#' @rdname tm_scale_continuous
tm_scale_continuous_pseudo_log = function(..., base = exp(1), sigma = 1) {
	tm_scale_continuous(trans = "pseudo_log", trans.args = list(base = base, sigma = sigma), ...)
}

#
# #' @export
# #' @rdname tm_scale_continuous
# tm_scale_continuous_logistic = function(...) {
# 	tm_scale_continuous(trans = "logistic", ...)
# }


#' Scales: RGB
#'
#' Scales in tmap are configured by the family of functions with prefix `tm_scale`.
#' Such function should be used for the input of the `.scale` arguments in the layer
#' functions (e.g. `fill.scale` in [tm_polygons()]).
#' The function [tm_scale_rgb()] is used to transform r, g, b band variables to colors. This function is adopted from (and works similar as) [stars::st_rgb()]
#'
#' @param value.na value for missing values
#' @param stretch should each (r, g, b) band be stretched? Possible values: `"percent"` (same as `TRUE`), `"histogram"`, `FALSE`.
#'   In the first case, the values are stretched to `probs[1]...probs[2]`. In the second case, a histogram equalization is performed
#' @param probs probability (quantile) values when `stretch = "percent"`
#' @param max_color_value maximum value
#' @seealso [tm_scale()] and [stars::st_rgb()]
#' @example ./examples/tm_scale_rgb.R
#' @export
tm_scale_rgb = function(value.na = NA,
						stretch = FALSE,
						probs = c(0, 1),
						max_color_value = 255L) {
	structure(c(list(FUN = "tmapScaleRGB"), as.list(environment())), class = c("tm_scale_rgb", "tm_scale", "list"))
}

#' @rdname tm_scale_rgb
#' @export
tm_scale_rgba = function(value.na = NA,
						 stretch = FALSE,
						 probs = c(0, 1),
						 max_color_value = 255) {
	structure(c(list(FUN = "tmapScaleRGBA"), as.list(environment())), class = c("tm_scale_rgba", "tm_scale", "list"))
}

# tm_scale_na = function() {
# 	structure(c(list(FUN = "tmapScaleNA"), as.list(environment())), class = c("tm_scale_na", "tm_scale", "list"))
# }
