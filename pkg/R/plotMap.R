plotMap <- function(gp, gt, gz) {
	
	add <- rep(TRUE, length(gp))	
	add[1] <- FALSE
	
	nlayers <- length(gp)
	shps <- lapply(gp, function(g) get(g$shp))
	
	## set bounding boxes
	shps <- lapply(shps, function(shp) {
		bb <- shp@bbox
		if (gz$units == "rel") {
			steps <- bb[,2] - bb[,1]
			bb[1, ] <- shp@bbox[1, 1] + gz$xlim * steps[1]
			bb[2, ] <- shp@bbox[2, 1] + gz$ylim * steps[2]
		} else {
			bb <- matrix(c(gz$xlim, gz$ylim), ncol = 2, byrow=TRUE)
		}
		shp@bbox <- bb
		shp
	})
	
	
	## plot shapee
	for (l in 1:nlayers) {
		gpl <- gp[[l]]
		plot(shps[[l]], col=gpl$fill, border = gpl$col, lwd=gpl$lwd, lty=gpl$lty, add=add[l])
	}
	
	vps <- baseViewports()
	
	## needed for small multiples
	vps$figure[c("x", "y", "width", "height")] <- vps$plot[c("x", "y", "width", "height")]
	
	pushViewport(vps$inner, vps$figure, vps$plot)
	#browser()
	
	vpWidth <- convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE)
	vpHeight <- convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE)
	aspVp <- vpWidth / vpHeight
	mapDim <- shps[[1]]@bbox[,2] - shps[[1]]@bbox[,1]
	
	aspMap <- mapDim[1] / mapDim[2]
	if (aspVp > aspMap) {
		vpWidth <- aspMap * vpHeight
	} else {
		vpHeight <- vpWidth / aspMap
	}
	
	vpArea <- vpWidth * vpHeight
	
	for (l in 1:nlayers) {
		gpl <- gp[[l]]
		npol <- length(shps[[l]])
		if (!is.na(gpl$bubble.size[1])) {
			sizes <- gpl$bubble.size
			if (length(sizes)!=npol) {
				if (length(sizes)!=1) warning("less bubble size values than objects")
				sizes <- rep(sizes, length.out=npol)
			}
				
			sizes <- sizes * (sqrt(vpArea) / 100)
			
			cols <- rep(gpl$bubble.col, length.out=npol)
			borders <- gpl$bubble.border
			co <- coordinates(shps[[l]])
			
			if (length(sizes)!=1) {
				decreasing <- order(-sizes)
				co <- co[decreasing,]
				sizes <- sizes[decreasing]
				cols <- if (length(cols)==1) cols else cols[decreasing]
			}
			grid.circle(x=unit(co[,1], "native"), y=unit(co[,2], "native"),
						r=unit(sizes, "inches"),
						gp=gpar(col=borders, fill=cols))
		}
		if (!is.na(gpl$text)) {
			co <- coordinates(shps[[l]])
			labels <- shps[[l]][[gpl$text]]
			grid.text(labels, x=unit(co[,1], "native"), y=unit(co[,2], "native"), 
					  gp=gpar(cex=gpl$cex))
		}
	
	}
	popViewport(3)
	
}


plotAll <- function(gp) {
	gt <- gp$geo_theme
	gz <- gp$geo_zoom
	
	gp[c("geo_theme", "geo_zoom")] <- NULL
	
	margins <- gt$margins
	draw.frame <- gt$draw.frame
	frame.lwd <- gt$frame.lwd
	title.position <- gt$title.position
	
	
	gridLayoutMap <- grid.layout(3, 3, 
								 heights=unit(c(margins[3], 1, margins[1]), 
								 			 c("npc", "null", "npc")), 
								 widths=unit(c(margins[2], 1, margins[4]), 
								 			c("npc", "null", "npc")))
	
	if (!gt$legend.only) {
		pushViewport(viewport(layout=gridLayoutMap))
	
		cellplot(2, 2, e={
			par(new=TRUE, fig=gridFIG(), mai=c(0,0,0,0))
			plotMap(gp, gt, gz)
			if (draw.frame) {
				grid.rect(gp=gpar(lwd=frame.lwd, fill=NA))
			}
		})
		
		upViewport()
	}
	
	#find which layer is choropleth
	choroID <- which(sapply(gp, function(x)!is.na(x$varnames$choro.fill[1])))[1]
	bubbleSizeID <- which(sapply(gp, function(x)!is.na(x$varnames$bubble.size[1])))[1]
	bubbleColID <- which(sapply(gp, function(x)!is.na(x$varnames$bubble.col[1])))[1]
	
	# possible conflict between choro and bubble: for the time being, choose the first, or choose bubbles
	if (!is.na(choroID)) {
		isChoroLegend <- TRUE
	} else if (!is.na(bubbleSizeID) || !is.na(bubbleColID)) {
		isChoroLegend <- FALSE
	} else {
		isChoroLegend <- NA
	}
	
	if (!is.na(isChoroLegend)) {
		if (isChoroLegend) {
			gc <- gp[[choroID]]
			if (gt$show.legend.text || gt$type.legend.plot!="none") {
				cellplot(2, 2, e={
					legendPlot(gt=gt, 
							   legend.palette=gc$choro.legend.palette, 
							   legend.labels=gc$choro.legend.labels, 
							   values=gc$choro.values, 
							   breaks=gc$choro.breaks, 
							   legend.bubbles=FALSE, 
							   legend.bubble.sizes=NULL, 
							   legend.bubble.labels=NULL, 
							   plot.bubble.borders=TRUE)
				})
			}
		} else {
			browser()
			gb <- gp[[bubbleSizeID]]
			if (gt$show.legend.text) {
				gt$type.legend.plot <- ifelse(is.na(bubbleSizeID), "none", "bubble")
				cellplot(2, 2, e={
					legendPlot(gt=gt, 
							   legend.palette=gb$bubble.legend.palette, 
							   legend.labels=gb$bubble.legend.labels, 
							   legend.bubbles=!is.na(bubbleSizeID), 
							   legend.bubble.sizes=gb$bubble.legend.sizes, 
							   legend.bubble.labels=gb$bubble.legend.size_labels, 
							   plot.bubble.borders=TRUE)
				})
			}
		}
	}
	
	#popViewport()
}