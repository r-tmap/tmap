tmapAesColorDiscrete = function(x1, setup, legend, opt) {
	if (!is.numeric(x1) && !is.factor(x1)) x1 = factor(x1)

	pals = if (is.factor(x1)) {
		pals::brewer.set3(nlevels(x1))
	} else {
		pals::brewer.blues(setup$n)
	}
	
	if (is.factor(x1)) {
		x1 = droplevels(x1)
		values = pals[as.integer(x1)]
		levs = levels(x1)
		brks = NA
	} else {
		ci = suppressWarnings({classInt::classIntervals(x1, n = setup$n, style = setup$style)})
		values = pals[classInt::findCols(ci)]
		levs = NA
		brks = ci$brks
	}
	
	values[is.na(values)] = "#BFBFBF"
	
	legend = list(title = legend$title, brks = brks, pals = pals, levs = levs)
	
	format_aes_results(values, legend)
	
}


tmapAesColorRGB = function(x1, x2, x3, setup, legend, opt) {
	values = grDevices::rgb(x1, x2, x3, maxColorValue = setup$maxValue)
	
	format_aes_results(values, list())
}

tmapAes2dSize = function(x1, setup, legend, opt) {
	max = if (is.na(setup$max)) max(x1, na.rm = TRUE) else setup$max
	values = x1 / max
	legend = list(max = max)
	
	format_aes_results(values, legend)
}

tmapAesShape = function(x1, legend, setup) {
	if (is.numeric(x1)) x1 = as.integer(x1)
	if (is.character(x1)) x1 = as.factor(x1)
	if (is.factor(x1)) x1 = as.integer(x1)
	#n = length(shapes)
	values = setup$shapes[x1]
	legend = list(shapes = setup$shapes)
	
	format_aes_results(values, legend)
}

