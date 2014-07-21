process_meta <- function(gt, gf, gg, nx, varnames) {
	
	gf <- within(gf, {
		by <- NULL
		if (is.null(ncol) && is.null(nrow)) {
			## default setting: place next to each other, or in grid
			if (nx <= 3) {
				ncol <- nx
				nrow <- 1
			} else {
				ncol <- ceiling(sqrt(nx))
				nrow <- ceiling(nx / ncol)
			}
		} else {
			if (is.null(ncol)) ncol <- ceiling(nx / nrow)
			if (is.null(nrow)) nrow <- ceiling(nx / ncol)
		}
	})
	
	m <- gf$ncol * gf$nrow
	
	gt <- within(gt, {
		statvars <- names(varnames[-1])
		nvars <- sum(sapply(varnames[-1], function(x)!is.na(x)[1]))
		
		# create complete list of legend.title vectors
		if (is.vector(legend.titles)) {
			if (!length(names(legend.titles))) {
				legend.titles <- rep(legend.titles, length.out=5)
				names(legend.titles) <- statvars
			} else {
				legend.titles_tmp <- rep(NA, length(statvars))
				names(legend.titles_tmp) <- statvars
				legend.titles_tmp[names(legend.titles)] <- legend.titles
				legend.titles <- legend.titles_tmp
			}
			legend.titles <- as.list(legend.titles)
		}

		# create complete list of legend.title lists
		legend.titles_tmp <- as.list(rep(NA, length(statvars)))
		names(legend.titles_tmp) <- statvars
		legend.titles_tmp[names(legend.titles)] <- legend.titles
		legend.titles <- legend.titles_tmp
		
		legend.titles_specified <- !all(sapply(legend.titles, function(x)is.na(x)[1]))
		
		
		rm(legend.titles_tmp)
		
		if (is.na(title[1])) {
			id <- which(as.logical(sapply(varnames, function(x)sum(!is.na(x[1])))))[1]
		} else id <- switch(title[1],
							by=1,
							fill=2,
							bubble.size=3,
							bubble.col=4,
							line.col=5,
							line.lwd=6,
							0)

		if (is.na(id)) {
			title <- rep("", nx)
		} else {
			# replace NA's with varnames
			legend.titles <- lapply(legend.titles, function(x) {
				idx <- substitute(x)[[3]] + 1
				if (is.na(x) && id!=idx && (!(id==0 && nvars==1))) varnames[[idx]] else x
			})
			if (id!=0 && !legend.titles_specified) {
				title <- rep(varnames[[id]], length.out=nx)
			} else {
				if (is.na(title[1])) title <- ""
				title <- rep(title, length.out=nx)
			}
		}
		rm(id)
		rm(nvars)
		
		legend.titles <- lapply(legend.titles, function(x) if (is.na(x[1])) "" else x)
		legend.titles <- lapply(legend.titles, function(x) rep(x, length.out=nx))
	
		legend.titles <- legend.titles[names(which(sapply(varnames[-1], function(x)!is.na(x)[1])))]
		
		
		scale <- scale / m
		
		title.cex <- title.cex * scale
		legend.title.cex <- legend.title.cex * scale
		legend.text.cex <- legend.text.cex * scale
		legend.hist.cex <- legend.hist.cex * scale
				
		if (is.null(bg.color)) bg.color <- ifelse(is.na(varnames$fill[1]), "white", "grey85")
		
		if (identical(title.bg.color, TRUE)) title.bg.color <- bg.color
		if (identical(legend.bg.color, TRUE)) legend.bg.color <- bg.color
	})	
	
	if (!is.null(gg)) {
		gg <- within(gg, {
			grid.show <- TRUE
		})
	} else {
		gg <- list(grid.show=FALSE)
	}
	
	c(gt, gf, gg)
}
