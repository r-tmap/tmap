set_bounding_box <- function(shps, gp, gt) {
	dw <- convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE)
	dh <- convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE)
	dasp <- dw/dh
	pasp <- gt$asp
	
	if (identical(pasp, 0)) pasp <- dasp
	mapply(function(shp, gpl) {
		bb <- shp@bbox
		bbrange <- bb[,2] - bb[,1]
		bbmarg <- gt$inner.margins[c(2,1,4,3)]
		bbmarg[c(1,2)] <- -bbmarg[c(1,2)]
		bb <- bb + rep(bbrange, 2) * bbmarg
		
		xlim <- bb[1,]
		ylim <- bb[2,]
		
		longlat <- !is.projected(shp)
		
		sasp <- if(longlat) {
			(diff(xlim)/diff(ylim)) * cos((mean(ylim) * pi)/180)
		} else {
			(diff(xlim)/diff(ylim))# * 2
		}

		if (!is.na(pasp)) {
			if (pasp > sasp) {
				## landscape device
				xdiff <- if (longlat) diff(ylim) * pasp / cos((mean(ylim) * pi)/180) else diff(ylim) * (pasp)
				bb[1, ] <- mean(xlim) + (xdiff * c(-.5, .5))
			} else {
				## portrait device
				ydiff <- if (longlat) (diff(xlim) * cos((mean(ylim) * pi)/180)) / pasp else diff(xlim) / (pasp)
				bb[2, ] <- mean(ylim) + (ydiff * c(-.5, .5))
			}
		}
		
		
		## try to crop the shape file at the bounding box in order to place bubbles and text labels inside the frame
		shp2 <- tryCatch({
			l <- length(shp)
			shp <- crop_shape(shp, bbox=bb)
			indices <- attr(shp, "matchID")
			
			if (length(gpl$fill)==l) gpl$fill <- gpl$fill[indices]
			if (length(gpl$bubble.size)==l) gpl$bubble.size <- gpl$bubble.size[indices]
			if (length(gpl$bubble.col)==l) gpl$bubble.col <- gpl$bubble.col[indices]
			shp
		}, error = function(e) {
			shp@bbox <- bb
			shp
		})
		
		list(shp=shp2, layer=gpl, sasp=ifelse(is.na(pasp), sasp, pasp), dasp=dasp)
	}, shps, gp, SIMPLIFY=FALSE)
}