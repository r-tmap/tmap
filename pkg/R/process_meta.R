process_meta <- function(gt, gst, gf, gg, gc, gsb, gcomp, nx, by_names, asp_ratio, shp_info) {
	
	
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
	
	legend.only <- legend.frame <- NULL
	gt <- within(gt, {
		if (legend.only) {
			title <- rep("", nx)
			legend.width <- 1
			legend.height <- 1
		} else {
			title <- if (is.na(title[1])) {
				if (is.na(by_names[1])) "" else by_names
			} else title
			title <- rep(title, length.out=nx)
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
				
		if (is.null(bg.color)) bg.color <- "white" #ifelse(is.na(varnames$fill[1]), "white", "grey75")
		
		legend.inside.box <- if (!is.logical(legend.frame)) TRUE else legend.frame
		if (identical(title.bg.color, TRUE)) title.bg.color <- bg.color
		if (identical(legend.bg.color, TRUE)) legend.bg.color <- bg.color
		
		outer.margins <- rep(outer.margins, length.out=4)
		
		inner.margins <- if (is.na(inner.margins[1])) {
			if (shp_info$is_raster) rep(0, 4) else rep(0.02, 4)
		} else rep(inner.margins, length.out=4)
		
		
		if (gst$sepia.intensity!=0 || gst$saturation!=1) {
			if (!is.null(bg.color)) bg.color <- do.call("process_color", c(list(col=bg.color), gst))
			if (!is.null(outer.bg.color)) outer.bg.color <- do.call("process_color", c(list(col=outer.bg.color), gst))
			if (!is.na(legend.bg.color)) legend.bg.color <- do.call("process_color", c(list(col=legend.bg.color, alpha=legend.bg.alpha), gst))
			if (!is.na(legend.hist.bg.color)) legend.hist.bg.color <- do.call("process_color", c(list(col=legend.hist.bg.color, alpha=legend.hist.bg.alpha), gst))
			if (!is.na(title.bg.color)) title.bg.color <- do.call("process_color", c(list(col=title.bg.color, alpha=title.bg.alpha), gst))
		}
		
	})	
	
	if (!is.null(gg)) {
		gg <- within(gg, {
			grid.show <- TRUE
		})
	} else {
		gg <- list(grid.show=FALSE)
	}
	
	if (!is.null(gc)) {
		gc <- within(gc, {
			credits.size <- credits.size * gt$scale
			credits.show <- TRUE
		})
	} else {
		gc <- list(credits.show=FALSE)
	}

	if (!is.null(gsb)) {
		gsb <- within(gsb, {
			scale.size <- scale.size * gt$scale
			scale.show <- TRUE
		})
	} else {
		gsb <- list(scale.show=FALSE)
	}
	
	if (!is.null(gcomp)) {
		gcomp <- within(gcomp, {
			compass.fontsize <- compass.fontsize * gt$scale
			compass.show <- TRUE
			if (is.na(compass.type)) compass.type <- gst$compass.type
			if (is.na(compass.size)) compass.size <- switch(compass.type, arrow=2, radar=6, rose=6, 4)
			compass.nlines <- compass.size + ifelse(compass.show.labels==0, 0, ifelse(compass.show.labels==1, 1, 2))
		})
	} else {
		gcomp <- list(compass.show=FALSE)
	}
	
	gst$compass.type <- NULL
	
	c(gt, gst, gf, gg, gc, gsb, gcomp, shp_info)
}
