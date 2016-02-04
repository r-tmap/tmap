process_meta <- function(gt, gf, gg, gc, gsb, gcomp, nx, by_names, asp_ratio, shpM_asp_marg, shp_info) {
	attr.color <- aes.colors <- aes.color <- pc <- grid.alpha <- NULL
	
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
	
	legend.only <- legend.frame <- legend.bg.alpha <- legend.hist.bg.alpha <- title.bg.alpha <- NULL
	
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
				
		#if (is.null(bg.color)) bg.color <- "white"
		if (is.null(space.color)) space.color <- bg.color
		
		legend.inside.box <- if (!is.logical(legend.frame)) TRUE else legend.frame
		if (identical(title.bg.color, TRUE)) title.bg.color <- bg.color
		
		if (identical(frame, TRUE)) frame <- attr.color else if (identical(frame, FALSE)) frame <- NA 
		if (is.logical(legend.frame)) if (identical(legend.frame, TRUE)) legend.frame <- attr.color else legend.frame <- NA 
# 		
# 		between.margin.in <- convertHeight(unit(between.margin, "lines") * scale, "inch", valueOnly=TRUE)
# 		
# 		between.margin.y <-convertHeight(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE) * gf$nrow
# 		between.margin.x <-convertWidth(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE) * gf$ncol
# 		
		
		outer.margins <- rep(outer.margins, length.out=4)
		#outer.margins <- rep(0, length.out=4)
		# 		if (m>1) {
# 			outer.margins <- c(between.margin.y, between.margin.x, between.margin.y, between.margin.x)
# 		}
		
		inner.margins <- if (is.na(inner.margins[1])) {
			if (shp_info$is_raster) rep(0, 4) else rep(0.02, 4)
		} else rep(inner.margins, length.out=4)
		
 		attr.color.light <- is_light(attr.color)
 		aes.color.light <- is_light(aes.colors)

		title.color <- do.call("process_color", c(list(col=title.color), pc))
		legend.text.color <- do.call("process_color", c(list(col=legend.text.color), pc))
		if (!is.na(frame)) frame <- do.call("process_color", c(list(col=frame), pc))
		if (!is.na(legend.frame)) legend.frame <- do.call("process_color", c(list(col=legend.frame), pc))
		
		if (is.na(earth.boundary.color)) earth.boundary.color <- attr.color
		earth.boundary.color <- do.call("process_color", c(list(col=earth.boundary.color), pc))
		
		#attr.color <- do.call("process_color", c(list(col=attr.color), pc))
		bg.color <- do.call("process_color", c(list(col=bg.color), pc))
		bg.overlay <- do.call("process_color", c(list(col=bg.overlay), pc))
		
		if (!is.null(outer.bg.color)) outer.bg.color <- do.call("process_color", c(list(col=outer.bg.color), pc))
		if (!is.na(legend.bg.color)) legend.bg.color <- do.call("process_color", c(list(col=legend.bg.color, alpha=legend.bg.alpha), pc))
		if (!is.na(legend.hist.bg.color)) legend.hist.bg.color <- do.call("process_color", c(list(col=legend.hist.bg.color, alpha=legend.hist.bg.alpha), pc))
		if (!is.na(title.bg.color)) title.bg.color <- do.call("process_color", c(list(col=title.bg.color, alpha=title.bg.alpha), pc))
		if (!is.na(earth.boundary.color)) earth.boundary.color <- do.call("process_color", c(list(col=earth.boundary.color), pc))
		space.color <- do.call("process_color", c(list(col=space.color), pc))
		
		earth.bounds <- if (is.logical(earth.boundary)) {
			c(-180, 180, -90, 90)
		} else {
			as.vector(extent(earth.boundary))
		}
		earth.boundary <- !identical(earth.boundary, FALSE)
		
		earth.boundary.lwd <- earth.boundary.lwd * scale
		frame.lwd <- frame.lwd * scale
	})	

	if (!is.null(gg)) {
		gg <- within(gg, {
			grid.show <- TRUE
			if (is.na(grid.col)) grid.col <- ifelse(gt$attr.color.light, darker(gt$attr.color, .5), lighter(gt$attr.color, .5))
			if (is.na(grid.labels.col)) grid.labels.col <- ifelse(gt$attr.color.light, darker(gt$attr.color, .2), lighter(gt$attr.color, .2))
			grid.col <- do.call("process_color", c(list(col=grid.col, alpha=grid.alpha), gt$pc))
			grid.labels.col <- do.call("process_color", c(list(col=grid.labels.col), gt$pc))
			grid.lwd <- grid.lwd * gt$scale
		})
	} else {
		gg <- list(grid.show=FALSE)
	}

	if (!is.null(gc)) {
		gc <- within(gc, {
		 	credits.col[is.na(credits.col)] <- gt$attr.color
			credits.col <- do.call("process_color", c(list(col=credits.col), gt$pc))
			credits.size <- credits.size * gt$scale
			credits.fontface[is.na(credits.fontface)] <-gt$fontface
			credits.fontfamily[is.na(credits.fontfamily)] <-gt$fontfamily
			credits.text <- lapply(credits.text, rep, length.out=nx)
			credits.show <- lapply(credits.text, function(ct) ct!="")
		})
	} else {
		gc <- list(credits.show=list(rep(FALSE, nx)))
	}

	if (!is.null(gsb)) {
		gsb <- within(gsb, {
			scale.size <- scale.size * gt$scale
			scale.lwd <- scale.lwd * gt$scale
			scale.show <- TRUE
		})
	} else {
		gsb <- list(scale.show=FALSE)
	}
	
	compass.show.labels <- NULL
	if (!is.null(gcomp)) {
		gcomp <- within(gcomp, {
			if (is.na(compass.text.color)) compass.text.color <- gt$attr.color
			compass.text.color <- do.call("process_color", c(list(col=compass.text.color), gt$pc))

			if (is.na(compass.color.dark)) compass.color.dark <- ifelse(gt$attr.color.light, "black", gt$attr.color)
			if (is.na(compass.color.light)) compass.color.light <- ifelse(gt$attr.color.light, gt$attr.color, "white")
			compass.color.dark <- do.call("process_color", c(list(col=compass.color.dark), gt$pc))
			compass.color.light <- do.call("process_color", c(list(col=compass.color.light), gt$pc))
			
			compass.fontsize <- compass.fontsize * gt$scale
			compass.lwd <- compass.lwd * gt$scale
			
			compass.show <- TRUE
			if (is.na(compass.type)) compass.type <- gt$compass.type
			if (is.na(compass.size)) compass.size <- switch(compass.type, arrow=2, radar=6, rose=6, 4)
			compass.nlines <- compass.size + ifelse(compass.show.labels==0, 0, ifelse(compass.show.labels==1, 1, 2))
		})
	} else {
		gcomp <- list(compass.show=FALSE)
	}
	
	gt[c("compass.type", "compass.size")] <- NULL
	
	c(gt, gf, gg, gc, gsb, gcomp, shp_info)
}
