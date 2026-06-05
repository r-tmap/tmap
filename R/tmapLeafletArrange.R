tmapLeafletArrange = function(tms, nx, ncol, nrow, opts, knit, show, args, options) {
	res = lapply(tms, function(tm) {
		print(tm, show = FALSE)
	})

	# Apply the requested height (interactive modes only). leafsync::latticeView
	# sizes its grid from the widgets themselves, so set each leaflet widget's
	# height. A numeric height (pixels) is the height of the WHOLE arrangement,
	# so split it across the rows; a string (e.g. "80vh") is applied per map.
	if (!is.null(opts$height)) {
		per = if (is.numeric(opts$height)) opts$height / nrow else opts$height
		res = lapply(res, function(w) {
			if (inherits(w, "htmlwidget")) w$height = per
			w
		})
	}

	res2 = do.call(leafsync::latticeView, c(res, list(ncol = ncol, sync = ifelse(identical(opts$sync, TRUE), "all", "none"), no.initial.sync = FALSE)))

	if (show) {
		if (knit) {
			kp = get("knit_print", asNamespace("knitr"))
			return(do.call(kp, c(list(x = res2), args, list(options = options))))
		} else {
			return(print(res2))
		}
	} else res2
}
