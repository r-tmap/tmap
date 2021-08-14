tm_const = function() {
	tmapOption("value.const")
}

tm_scale_auto = function() {
	structure(c(list(FUN = "tmapScaleAuto"), as.list(environment())), class = c("tm_scale_auto", "tm_scale"))
}

tm_scale_not_implemented = function() {
	structure(list(), class = "tm_scale")
}

tm_scale_ordinal = function(n.max = 20,
							values = NA, # e.g. palette, shape numbers, set of sizes (if two values are specified they are interpret as range)
							values.repeat = TRUE,
							values.contrast  = 1,
							value.na = NA, # value for NA
							value.null = NA, # value for NULL (needed?)
							value.neutral = NA, # value for neutral (used in other legends)
							levels = NULL, # levels to show (other values are treated as missing)
							levels.drop = FALSE,
							labels = NULL, # labels for the categories (by default the factor levels)
							label.na = NA, # label for missing values
							label.null = NA) { # label for null values (needed?)
	structure(c(list(FUN = "tmapScaleCategorical"), as.list(environment())), class = c("tm_scale_ordinal", "tm_scale"))
}



tm_scale_categorical = function(n.max = 20,
								values = NA, # e.g. palette, shape numbers, set of sizes (if two values are specified they are interpret as range)
								values.repeat = TRUE,
								values.contrast  = NA,
								value.na = NA, # value for NA
								value.null = NA, # value for NULL (needed?)
								value.neutral = NA, # value for neutral (used in other legends)
								levels = NULL, # levels to show (other values are treated as missing)
								levels.drop = FALSE,
								labels = NULL, # labels for the categories (by default the factor levels)
								label.na = NA, # label for missing values
								label.null = NA) { # label for null values (needed?)
	structure(c(list(FUN = "tmapScaleCategorical"), as.list(environment())), class = c("tm_scale_categorical", "tm_scale"))
}

tm_scale_intervals = function(n = 5, 
							  values = NA,
							  values.repeat = TRUE,
							  values.contrast  = NA,
							  value.na = NA,
							  value.null = NA,
							  value.neutral = NA,
							  style = ifelse(is.null(breaks), "pretty", "fixed"),
							  style.args = list(),
							  #as.count = NA,
							  breaks = NULL,
							  interval.closure = "left",
							  midpoint = NULL,
							  labels = NULL,
							  label.na = "Missing",
							  label.null = "Undefined") {
	structure(c(list(FUN = "tmapScaleIntervals"), as.list(environment())), class = c("tm_scale_intervals", "tm_scale"))
}

tm_scale_continuous = function(value.range = NA,
							   valueNA = NA,
							   valueNULL = NA,
							   showNA = NA,
							   showNULL = NA,
							   data.range = NA,
							   data.legend = NA,
							   outliers = c("show", "hide", "trunc"),
							   type = c("length", "area", "volume"),
							   perceptual = FALSE,
							   neutral.value = NA,
							   labels = NULL,
							   labelNA = "Missing",
							   labelNULL = "Undefined") {
	
	structure(c(list(FUN = "tmapScaleContinuous"), as.list(environment())), class = c("tm_scale_continuous", "tm_scale"))
}


# tm_scale_order = function(aes,
# 						  valueNA = NA,
# 						  valueNULL = NA,
# 						  labels = NULL,
# 						  labelNA = "Missing",
# 						  labelNULL = "Undefined") {
# 	
# }
# 
# tm_scale_log10 = function(aes,
# 						  valuerange = NA,
# 						  valueNA = NA,
# 						  valueNULL = NA,
# 						  labels = NULL,
# 						  labelNA = "Missing",
# 						  labelNULL = "Undefined") {
# 	
# }
# 		
# tm_scale_continuous_area = function(aes,
# 									valuerange = NA,
# 									valueNA = NA,
# 									valueNULL = NA,
# 									labels = NULL,
# 									labelNA = "Missing",
# 									labelNULL = "Undefined") {
# 	
# }
# tm_scale_log10_area = function(aes,
# 							   valuerange = NA,
# 							   valueNA = NA,
# 							   valueNULL = NA,
# 							   labels = NULL,
# 							   labelNA = "Missing",
# 							   labelNULL = "Undefined") {
# 	
# }

