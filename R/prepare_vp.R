prepare_vp <- function(vp, gm, interactive, gt) {
	
	if (interactive) {
		devsize <- dev.size()
		dasp <- devsize[1] / devsize[2]
		iasp <- gm$shape.masp
		asp_ratio <- iasp / dasp
	} else {
		if (is.null(vp)) {
			grid.newpage()
		} else {
			if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
		}
		
		## calculate device aspect ratio (needed for small multiples' nrow and ncol)
		inner.margins <- gt$inner.margins
		inner.margins <- if (is.na(inner.margins[1])) {
			if (gm$shape.is_raster_master) rep(0, 4) else rep(0.02, 4)
		} else rep(inner.margins, length.out=4)
		xmarg <- sum(inner.margins[c(2,4)])
		ymarg <- sum(inner.margins[c(1,3)])
		if (xmarg >= .8) stop("Inner margins too large", call. = FALSE)
		if (ymarg >= .8) stop("Inner margins too large", call. = FALSE)
		iasp <- gm$shape.masp * (1+(xmarg/(1-xmarg))) / (1+(ymarg/(1-ymarg)))
		dasp <- convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE)/convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE) # it may be different than dev.size, since vp can be defined
		asp_ratio <- iasp / dasp
	}
	list(shape.dasp = dasp,
		 shape.asp_ratio = asp_ratio)
}
