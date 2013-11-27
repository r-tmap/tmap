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
	
	for (l in 1:nlayers) {
		gpl <- gp[[l]]
		npol <- length(shps[[l]])
		if (!is.na(gpl$bubble.size[1])) {
			sizes <- gpl$bubble.size
			if (length(sizes)!=npol) {
				warning("less bubble size values than objects")
				sizes <- rep(sizes, length.out=npol)
			}
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
	
	gridLayoutLegend <- grid.layout(3, 3, 
									heights=unit(c(margins[3], 1, margins[1]), 
												 c("npc", "null", "npc")), 
									widths=unit(c(margins[2]+0.05, 1, margins[4]), 
												c("npc", "null", "npc")))
	
	browser()
	titleHeight <- convertHeight(unit(gt$title.cex, "lines"), "npc", valueOnly=TRUE)
	titleWidth <- convertWidth(stringWidth(gt$title), "npc", valueOnly=TRUE)
	
	if (is.character(title.position)) 
		title.position <- c(switch(title.position[1], left=0, center=(1-legendWidth)/2, centre=(1-legendWidth)/2, right=1-legendWidth),
							 switch(title.position[2], top=1-legendHeight, center=(1-legendHeight)/2, centre=(1-legendHeight)/2, bottom=0))	
	
	grid.text(gt$title, x=margins[2] + 0.05, y=unit(1,"npc") - unit(.75, "lines"), just="left", gp=gpar(cex=gt$title.cex))

	
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
	
	
	pushViewport(viewport(layout=gridLayoutLegend))
	
	#find which layer is choropleth
	choroID <- which(sapply(gp, function(x)!is.na(x$choro.values[1])))[1]

	if (!is.na(choroID)) {
		gc <- gp[[choroID]]
			if (gt$show.legend.text || gt$type.legend.plot!="none") {
				cellplot(2, 2, e={
					legendPlot(gt,
							   values = gc$choro.values,
							   breaks=gc$choro.breaks)
				})
			}
	}
	
	popViewport()
}