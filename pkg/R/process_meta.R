process_meta <- function(gt, gf, gg, nx, varnames) {
	legend.config <- NULL
	
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
		idname <- names(which(sapply(varnames, function(x)as.logical(sum(!is.na(x[1]))))[legend.config]))[1]
		if (!is.na(varnames[[1]][1])) {
			idname <- "by"
		}
		one <- sum(sapply(varnames[-1], function(x)!is.na(x[1])))==1
		spec.title <- !is.na(title[1])
		add.title <- !identical(title, "")
		
		## if no names, repeat for all sublegends except for heading
		if (!length(names(legend.titles))) {
			legend.titles <- rep(legend.titles, length.out=length(varnames)-1)
			names(legend.titles) <- names(varnames[-1])
			if (add.title && !is.na(idname) && idname != "by") legend.titles[idname] <- ""
		}
		if (is.vector(legend.titles)) legend.titles <- as.list(legend.titles)
		
		tmp <- as.list(rep(NA, length(varnames)-1))
		names(tmp) <- names(varnames[-1])

		if (!is.na(idname) && (add.title || one) && idname != "by") tmp[[idname]] <- ""
		nna <- sapply(legend.titles, function(x)!is.na(x[1]))
		if (any(nna)) tmp[names(legend.titles[nna])] <- legend.titles[nna]
		
		legend.titles <- tmp[names(which(sapply(varnames[-1], function(x)!is.na(x)[1])))]
		
		if (is.na(idname)) {
			if (!spec.title) title <- rep("", nx)
			legend.titles <- NA
		} else {
			# replace NA's with varnames
			legend.titles <- lapply(legend.titles, function(x) {
				id <- eval.parent(quote(names(X)))[substitute(x)[[3]]]
				y <- if (is.na(x[1])) varnames[[id]] else x
				rep(y, length.out=nx)
			})
			if (!spec.title) {
				title <- rep(varnames[[idname]], length.out=nx)
			} else {
				title <- rep(title, length.out=nx)
			}
		}
		
		scale <- scale / sqrt(m)
		
		title.cex <- title.cex * scale
		legend.title.cex <- legend.title.cex * scale
		legend.text.cex <- legend.text.cex * scale
		legend.hist.cex <- legend.hist.cex * scale
				
		if (is.null(bg.color)) bg.color <- ifelse(is.na(varnames$fill[1]), "white", "grey75")
		
		if (identical(title.bg.color, TRUE)) title.bg.color <- bg.color
		if (identical(legend.bg.color, TRUE)) legend.bg.color <- bg.color
		
		outer.margins <- rep(outer.margins, length.out=4)
		inner.margins <- rep(inner.margins, length.out=4)
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
