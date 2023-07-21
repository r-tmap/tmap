#' Scales: bivariate scale
#' 
#' Scales in tmap are configured by the family of functions with prefix \code{tm_scale}. Such function should be used for the input of the \code{.scale} arguments in the layer functions (e.g. \code{fill.scale} in \code{\link{tm_polygons}}). The function \code{tm_scale_bivariat} is usedf or bivariate.scales
#' 
#' @param scale1,scale2 two `tm_scale` objects. Currently, all `tm_scale_` functions are supported except `tm_scale_continous`.
#' @export
#' @name tm_scale_bivariate
#' @rdname tm_scale_bivariate
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
