tmapScaleRGB = function(x1, x2, x3, scale, legend, opt, aes, layer, p) {
	values = grDevices::rgb(x1, x2, x3, maxColorValue = scale$maxValue)
	
	if (!(p %in% c("col", "fill"))) stop("tm_scale_rgb cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)
	
	legend = list(title = NA, 
				  nitems = 0,
				  labels = NA, 
				  dvalues = NA, 
				  vvalues = NA,
				  vneutral = "grey50",
				  na.show = NA,
				  setup = list(show = FALSE))
	
	format_aes_results(values, legend)
	
}

tmapScaleNA = function(x1, scale, legend, opt, aes, layer, p) {
	legend = list(title = NA, 
				  nitems = 0,
				  labels = NA, 
				  dvalues = NA, 
				  vvalues = NA,
				  vneutral = "grey50",
				  na.show = NA,
				  setup = list(show = FALSE))
	
	format_aes_results(values, legend)
	
}
