tmapAesColorDiscrete = function(x1, setup, opt) {
	if (!is.numeric(x1) && !is.factor(x1)) x1 = factor(x1)

	pals = if (is.factor(x1)) {
		pals::brewer.set2(nlevels(x1))
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
	
	legend = list(title = setup$legend$title, brks = brks, pals = pals, levs = levs)
	
	format_aes_results(values, legend)
	
}
