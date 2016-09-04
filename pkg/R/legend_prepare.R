legend_prepare <- function(gp, gt, scaleFactor) {

	varnames <- c("fill", "symbol.size", "symbol.col", "symbol.shape", "line.col", "line.lwd", "raster", "text.size", "text.col")
	varnames_hist <- c("fill", "symbol.col", "line.col", "raster")
	
	# todo hist: "fill_hist"
	# is.portrait
	
	if (gt$legend.show) {
		x <- lapply(gp, function(gpl) {
			y <- lapply(varnames, function(v) {
				if (!is.na(gpl$varnames[[v]][1])) {
					if (gpl[[paste(v, "legend.show", sep=".")]]) {
						legend.labels <- paste(v, "legend.labels", sep=".")
						legend.text <- paste(v, "legend.text", sep=".")
						legend.palette <- paste(v, "legend.palette", sep=".")
						legend.title <- paste(v, "legend.title", sep=".")
						legend.is.portrait <- paste(v, "legend.is.portrait", sep=".")
						legend.z <- paste(v, "legend.z", sep=".")
						legend.misc <- paste(v, "legend.misc", sep=".")
						list_misc <- gpl[[legend.misc]]
						if (v=="symbol.size") list_misc$legend.sizes <- list_misc$legend.size * scaleFactor / 2
						if (v %in% c("symbol.col", "symbol.shape")) list_misc$symbol.max.size <- list_misc$symbol.max.size * scaleFactor / 2
						
						
						c(list(legend.type=v,
							   legend.title=gpl[[legend.title]],
							   legend.is.portrait=gpl[[legend.is.portrait]],
							   legend.z=gpl[[legend.z]],
							   legend.labels=gpl[[legend.labels]],
							   legend.text=gpl[[legend.text]],
							   legend.palette=gpl[[legend.palette]]),
						  list_misc)
					}
				}
			})
			
			yhist <- lapply(varnames_hist, function(v) {
				vh <- paste(v, "hist", sep="_")
				legend.hist <- paste(v, "legend.hist", sep=".")
				
				if (!is.na(gpl$varnames[[v]][1])) {
					if (gpl[[legend.hist]]) {
						legend.labels <- paste(v, "legend.labels", sep=".")
						legend.palette <- paste(v, "legend.palette", sep=".")
						legend.title <- paste(v, "legend.hist.title", sep=".")
						legend.hist.z <- paste(v, "legend.hist.z", sep=".")
						legend.hist.misc <- paste(v, "legend.hist.misc", sep=".")
						list_hist_misc <- gpl[[legend.hist.misc]]
						c(list(legend.type="hist",
							   legend.title=gpl[[legend.title]],
							   legend.is.portrait=TRUE,
							 legend.z=gpl[[legend.hist.z]],
							 legend.labels=gpl[[legend.labels]],
							 legend.palette=gpl[[legend.palette]]),
						  list_hist_misc)
					} else NULL
				} else NULL
			})
			
			c(y[!sapply(y, is.null)], yhist[!sapply(yhist, is.null)])
		})
		legelem <- do.call("c", x)
	} else legelem <- list(NULL)
	

	if (all(sapply(legelem, is.null))) {
		return(NULL)
	} else {
		legelem
	}
}