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

	mx = max(unlist(xlist, recursive = FALSE, use.names = FALSE), na.rm = TRUE)

	scale = within(scale, {
		if (is.logical(stretch)) {
			if (stretch)
				stretch.method = "percent"
			else max_color_value = max(max_color_value, mx)
		}
		if (is.character(stretch)) {
			if (!stretch %in% c("percent", "histogram"))
				stretch.method = "percent"
			else stretch.method = stretch
			stretch = TRUE
		}
	})
	cutoff = function(x, probs, stretch.method = "percent", max_color_value = 255L) {
		if (stretch.method == "percent") {
			qs = if (all(probs == c(0, 1)))
				range(x, na.rm = TRUE)
			else quantile(x, probs, na.rm = TRUE)
			x = (x - qs[1])/(qs[2] - qs[1])
			x[x > 1] = 1
			x[x < 0] = 0
			x * max_color_value
		}
		else if (stretch.method == "histogram") {
			x = (stats::ecdf(x))(x)
			x * max_color_value
		}
		else {
			qs = range(x)
			(x - qs[1])/(qs[2] - qs[1]) * max_color_value
		}
	}

	if (scale$stretch) {
		xlist2 = lapply(xlist, cutoff, probs = scale$probs, stretch.method = scale$stretch.method, max_color_value = scale$max_color_value)
	} else {
		xlist2 = xlist
	}

	if (any(isna)) {
		values = rep(scale$value.na, n)
		values[!isna] = do.call(grDevices::rgb, c(lapply(xlist2, function(xl) xl[!isna]), list(maxColorValue = scale$max_color_value)))
	} else {
		values = do.call(grDevices::rgb, c(xlist2, list(maxColorValue = scale$max_color_value)))
	}

	if (layer_args$saturation != 1) {
		pc = o$pc
		pc$saturation = pc$saturation * layer_args$saturation
		values = do.call(process_color, c(list(col = values), pc))
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
