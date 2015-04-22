process_meta <- function(gt, gf, gg, nx, varnames, asp_ratio) {
	legend.config <- NULL
	
	
	gf <- within(gf, {
		by <- NULL
		if (is.null(ncol) && is.null(nrow)) {
			#           asp ~ nrow      
			#       |-------------- 
			#   1   |
			# ~ncol |       nx
			#       | 
			ncol_init <- sqrt(nx/asp_ratio)
			nrow_init <- nx / ncol_init
			
			# rounding:
			nrow_ceiling <- min(ceiling(nrow_init), nx)
			ncol_ceiling <- min(ceiling(ncol_init), nx)
			
			# find minimal change
			nrow_xtra <- abs(nrow_ceiling - nrow_init) * ncol_init
			ncol_xtra <- abs(ncol_ceiling - ncol_init) * nrow_init
			
			# calculaet the other, and subtract 1 when possible
			if (nrow_xtra < ncol_xtra) {
				nrow <- nrow_ceiling
				ncol <- ceiling(nx / nrow)
				if ((nrow-1) * ncol >= nx) nrow <- nrow - 1
			} else {
				ncol <- ncol_ceiling
				nrow <- ceiling(nx / ncol)
				if ((ncol-1) * nrow >= nx) ncol <- ncol - 1
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
			legend.titles <- mapply(function(x, id) {
				y <- if (is.na(x[1])) varnames[[id]] else x
				rep(y, length.out=nx)
			}, legend.titles, names(legend.titles), SIMPLIFY=FALSE)
			if (!spec.title) {
				title <- rep(varnames[[idname]], length.out=nx)
			} else {
				title <- rep(title, length.out=nx)
			}
		}

		if (asp_ratio>1) {
			asp_w <- 1
			asp_h <- 1/asp_ratio
		} else {
			asp_w <- asp_ratio
			asp_h <- 1
		}
		
		scale <- scale * (min(1/ (asp_w * gf$ncol), 1 / (asp_h * gf$nrow))) ^ (1/gf$scale.factor)

		title.size <- title.size * scale
		legend.title.size <- legend.title.size * scale
		legend.text.size <- legend.text.size * scale
		legend.hist.size <- legend.hist.size * scale
				
		if (is.null(bg.color)) bg.color <- ifelse(is.na(varnames$fill[1]), "white", "grey75")
		
		legend.inside.box <- !is.logical(legend.bg.color) 
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
