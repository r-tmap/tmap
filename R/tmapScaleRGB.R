tmapScaleRGB_RGBA = function(xlist, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE, called) {
	
	k = length(xlist)
	n = length(xlist[[1]])
	
	cls = vapply(xlist, function(xi) {
		data_class(xi)[1]
	}, FUN.VALUE = character(1))
	
	
	# cls1 = data_class(x1)
	# cls2 = data_class(x2)
	# cls3 = data_class(x3)
	
	if (!(aes %in% c("col", "fill"))) stop("tm_scale_rgb cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)
	if (any(cls != "num")) {
		stop(called, " requires ", {if (called == "tm_scale_rgb") 3 else 4}, " variables", call. = FALSE)
	}
	
	#scale = get_scale_defaults(scale, opt, aes, layer, cls)
	scale$value.na = if (is.na(scale$value.na) || identical(scale$value.na, TRUE)) getAesOption("value.na", o, aes, layer, cls = cls[1]) else scale$value.na
	
	isna = Reduce("|", lapply(xlist, is.na))
	
	if (any(isna)) {
		values = rep(scale$value.na, n) 
		values[!isna] = do.call(grDevices::rgb, c(xlist, list(maxColorValue = scale$maxValue)))
	} else {
		values = do.call(grDevices::rgb, c(xlist, list(maxColorValue = scale$maxValue)))
	}
	
	
	legend = list(title = NA, 
				  nitems = 0,
				  labels = NA, 
				  dvalues = NA, 
				  vvalues = NA,
				  vneutral = "grey50",
				  na.show = NA,
				  scale = "RGB",
				  show = FALSE,
				  active = FALSE)
	
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


tmapScaleRGB = function(x1, x2, x3, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {
	tmapScaleRGB_RGBA(xlist = list(x1, x2, x3), scale = scale, legend = legend, chart = chart, o = o, aes = aes, layer = layer, layer_args = layer_args, sortRev = sortRev, bypass_ord = bypass_ord, submit_legend = submit_legend)
}

tmapScaleRGBA = function(x1, x2, x3, x4, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {
	tmapScaleRGB_RGBA(xlist = list(x1, x2, x3, x4), scale = scale, legend = legend, chart = chart, o = o, aes = aes, layer = layer, layer_args = layer_args, sortRev = sortRev, bypass_ord = bypass_ord, submit_legend = submit_legend)
}