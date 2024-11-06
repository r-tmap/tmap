tmapLeafletArrange = function(tms, nx, ncol, nrow, opts, knit, show, args, options) {
	res = lapply(tms, function(tm) {
		print(tm, show = FALSE)
	})
	res2 = do.call(leafsync::latticeView, c(res, list(ncol=ncol, sync=ifelse(identical(opts$sync, TRUE), "all", "none"), no.initial.sync = FALSE)))

	#
	# lfs <- lapply(tms, function(tm) {
	# 	tmap_leaflet(tm, add.titles = FALSE)
	# })
	# lfmv <- do.call(leafsync::latticeView, c(lfs, list(ncol=ncol, sync=ifelse(opts$sync, "all", "none"))))
	#
	# if (add.titles) lfmv <- view_add_leaflet_titles(lfmv)

	if (show) {
		if (knit) {
			kp <- get("knit_print", asNamespace("knitr"))
			return(do.call(kp, c(list(x=res2), args, list(options=options))))
		} else {
			return(print(res2))
		}
	} else res2
}
