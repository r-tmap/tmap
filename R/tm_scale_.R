tm_const = function(name) {
	tmapOption("value.const", name)
}

tm_scale_auto = function(aes) {
	structure(c(list(FUN = "tmapScaleAuto"), as.list(environment())), class = c("tm_scale_auto", "tm_scale"))
}

tm_scale_not_implemented = function() {
	structure(list(), class = "tm_scale")
}

tm_scale_categorical = function(aes,
								values = NA,
								valueNA = NA,
								valueNULL = NA,
								showNA = NA,
								showNULL = NA,
								levels = NA,
								drop.levels = FALSE,
								stretch.values = FALSE,
								contrast.values = 1,
								labels = NULL,
								labelNA = "Missing",
								labelNULL = "Undefined") {
	structure(c(list(FUN = "tmapScaleCategorical"), as.list(environment())), class = c("tm_scale_categorical", "tm_scale"))
}

tm_scale_class_int = function(aes,
							  values = NA,
							  valueNA = NA,
							  valueNULL = NA,
							  n = 5,
							  style = ifelse(is.null(breaks), "pretty", "fixed"),
							  style.args = list(),
							  as.count = NA,
							  breaks = NULL,
							  interval.closure = "left",
							  midpoint = NULL,
							  stretch.values = FALSE,
							  contrast.values = 1,
							  labels = NULL,
							  labelNA = "Missing",
							  labelNULL = "Undefined") {
	structure(c(list(FUN = "tmapScaleClassInt"), as.list(environment())), class = c("tm_scale_class_int", "tm_scale"))
}

tm_scale_continuous = function(aes,
							   value.range = NA,
							   valueNA = NA,
							   valueNULL = NA,
							   data.range = NA,
							   data.legend = NA,
							   outliers = c("show", "hide", "trunc"),
							   type = c("length", "area", "volume"),
							   perceptual = FALSE,
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

