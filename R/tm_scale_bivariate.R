#' Scales: bivariate scale
#'
#' Scales in tmap are configured by the family of functions with prefix `tm_scale`.
#' Such function should be used for the input of the `.scale` arguments in the
#' layer functions (e.g. `fill.scale` in [tm_polygons()]).
#' The function `tm_scale_bivariate()` is used for `bivariate.scales`.
#'
#' @param scale1,scale2 two `tm_scale` objects. Currently, all `tm_scale_*()` functions are supported except `tm_scale_continuous()`.
#' @param values (generic scale argument) The visual values. For colors
#'   (e.g. `fill` or `col` for `tm_polygons()`) this is a palette name from the `cols4all` package (see [cols4all::c4a()])
#'   or vector of colors, for size (e.g. `size` for `tm_symbols()`) these are a set of sizes (if two values are specified they are interpret as range),
#'   for symbol shapes (e.g. `shape` for [tm_symbols()]) these are a set of symbols, etc.
#'   The tmap option `values.var` contains the default values per visual variable and in some cases also per data type.
#' @param values.repeat (generic scale argument) Should the values be repeated in case there are more categories?
#' @param values.range (generic scale argument) Range of the values. Vector of two numbers (both between 0 and 1) where the first determines the minimum and the second the maximum.
#'   Full range, which means that all values are used, is encoded as `c(0, 1)`.
#'   For instance, when a grey scale is used for color (from black to white), `c(0,1)`
#'   means that all colors are used, `0.25, 0.75` means that only colors from dark grey to light
#'   grey are used (more precisely `"grey25"` to `"grey75"`), and `0, 0.5` means
#'   that only colors are used from black to middle grey (`"grey50"`).
#'   When only one number is specified, this is interpreted as the second number
#'   (where the first is set to 0). Default values can be set via the tmap option `values.range`.
#' @param values.scale (generic scale argument) Scaling of the values. Only useful for size-related visual variables, such as `size` of [tm_symbols()] and `lwd` of [tm_lines()].
#' @param value.na (generic scale argument) Value used for missing values. See tmap option `"value.na"` for defaults per visual variable.
#' @param value.null (generic scale argument) Value used for NULL values.
#'   See tmap option `"value.null"` for defaults per visual variable. Null data
#'   values occur when out-of-scope features are shown (e.g. for a map of Europe showing a data variable per country,
#'   the null values are applied to countries outside Europe).
#' @param value.neutral (generic scale argument) Value that can be considered neutral.
#'   This is used for legends of other visual variables of the same map layer.
#'   E.g. when both `fill` and `size` are used for [tm_symbols()] (using filled circles),
#'   the size legend items are filled with the `value.neutral` color from the `fill.scale` scale,
#'   and fill legend items are bubbles of size `value.neutral` from the `size.scale` scale.
#' @param labels (generic scale argument) Labels
#' @param label.na (generic scale argument) Label for missing values
#' @param label.null (generic scale argument) Label for null (out-of-scope) values
#' @seealso [tm_scale()]
#' @export
tm_scale_bivariate = function(scale1 = tm_scale(),
							  scale2 = tm_scale(),
							  values = NA,
							  values.repeat = FALSE,
							  values.range  = NA,
							  values.scale = 1,
							  value.na = NA,
							  value.null = NA,
							  value.neutral = NA,
							  labels = NULL,
							  label.na = NA,
							  label.null = NA) {
	structure(c(list(FUN = "tmapScaleBivariate"), as.list(environment())), class = c("tm_scale_bivariate", "tm_scale", "list"))
}
