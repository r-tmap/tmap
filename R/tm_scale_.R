#' @export
tm_const = function() {
	tmapOption("value.const")
}

#' @export
tm_shape_vars = function() {
	structure(list(), class = c("tm_shape_vars", "list"))
}

#' @export
tm_scale = function(...) {
	structure(c(list(FUN = "tmapScaleAuto"), list(...)), class = c("tm_scale_auto", "tm_scale", "list"))
}

#' @export
tm_scale_not_implemented = function() {
	structure(list(), class = c("tm_scale", "list"))
}

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
							label.null = NA) { # label for null values (needed?)
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
								label.null = NA) { # label for null values (needed?)
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
							  label.null = NA) {
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
							 label.null = NA) {
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
							   label.null = NA) {
	
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

