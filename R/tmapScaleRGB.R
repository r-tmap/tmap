tmapScaleRGB = function(x1, x2, x3, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {
	
	cls1 = data_class(x1)
	cls2 = data_class(x2)
	cls3 = data_class(x3)
	
	if (!(aes %in% c("col", "fill"))) stop("tm_scale_rgb cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)
	if (cls1[1] != "num" || cls2[1] != "num" || cls3[1] != "num") {
		stop("tm_scale_rgb requires three numeric variables", call. = FALSE)
	}
	
	#scale = get_scale_defaults(scale, opt, aes, layer, cls)
	scale$value.na = if (is.na(scale$value.na) || identical(scale$value.na, TRUE)) getAesOption("value.na", o, aes, layer, cls = cls1) else scale$value.na

	
	
	isna = is.na(x1) | is.na(x2) | is.na(x3)
	if (any(isna)) {
		values = rep(scale$value.na, length(x1)) 
		values[!isna] = grDevices::rgb(x1[!isna], x2[!isna], x3[!isna], maxColorValue = scale$maxValue)
		
	} else {
		values = grDevices::rgb(x1, x2, x3, maxColorValue = scale$maxValue)
	}
	

	legend = list(title = NA, 
				  nitems = 0,
				  labels = NA, 
				  dvalues = NA, 
				  vvalues = NA,
				  vneutral = "grey50",
				  na.show = NA,
				  scale = "RGB",
				  show = FALSE)
	
	chart = list(show = FALSE)
	
	if (submit_legend) {
		if (bypass_ord) {
			format_aes_results(values, legend = legend, chart = chart)
		} else {
			format_aes_results(values, ids, legend, chart = chart)			
		}
	} else {
		list(vals = values, ids = ids, legend = legend, chart = chart, bypass_ord = bypass_ord)
	}
}


tmapScaleAsIs = function(x1, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {
	legend = list(title = NA, 
				  nitems = 0,
				  labels = NA, 
				  dvalues = NA, 
				  vvalues = NA,
				  vneutral = NA,
				  na.show = NA,
				  scale = "AsIs",
				  show = FALSE)

	sfun = paste0("tmapValuesScale_", aes)
	cfun = paste0("tmapValuesColorize_", aes)
	
	x2 = do.call(sfun, list(x = x1, scale = o$scale))
	values = do.call(cfun, list(x = x2, pc = o$pc))
	
	if (submit_legend) {
		if (bypass_ord) {
			format_aes_results(values, legend = legend, chart = chart)
		} else {
			format_aes_results(values, ord = 1L, legend = legend, chart = chart)		
		}
	} else {
		list(vals = values, ids = 1L, legend = legend, chart = chart, bypass_ord = bypass_ord)
	}
}