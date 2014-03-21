set_bounding_box <- function(gp, gt, gz) {
	lapply(gp, function(gpl) {
		shp <- gpl$shp
		
		bb <- shp@bbox
		if (gz$units == "rel") {
			steps <- bb[,2] - bb[,1]
			bb[1, ] <- shp@bbox[1, 1] + gz$xlim * steps[1]
			bb[2, ] <- shp@bbox[2, 1] + gz$ylim * steps[2]
		} else {
			bb <- matrix(c(gz$xlim, gz$ylim), ncol = 2, byrow=TRUE)
		}
		shp@bbox <- bb
		
		if (gt$draw.frame) {
			bbcoords <- cbind(x=bb[1,][c(1, 1, 2, 2, 1)], y=bb[2,][c(1, 2, 2, 1, 1)])
			
			BB <- SpatialPolygons(list(Polygons(list(Polygon(bbcoords)), "1")),
								  proj4string=CRS(proj4string(shp)))
			
			shp2 <- gIntersection(shp, BB, byid=TRUE)
			shpdata <- shp@data
			ids <- getIDs(shp)
			ids2 <- gsub( " .*$", "", getIDs(shp2))
			indices <- match(ids2, ids)
			shp2 <- SpatialPolygonsDataFrame(shp2, shpdata[indices, ], match.ID = FALSE)
			shp <- shp2
			
			if (length(gpl$fill)==length(ids)) gpl$fill <- gpl$fill[indices]
			if (length(gpl$bubble.size)==length(ids)) gpl$bubble.size <- gpl$bubble.size[indices]
			
		}
		gpl$shp <- shp
		gpl
	})
}

plotMap <- function(gp, gt, gz) {
	draw.frame <- gt$draw.frame
	frame.lwd <- gt$frame.lwd
	
	nlayers <- length(gp)
	
	## add shape objects to layers
	gp <- lapply(gp, function(g) {g$shp <- get(g$shp); g})
	
	## set bounding box and frame
	gp <- set_bounding_box(gp, gt, gz)
	
	## plot shapes
	add <- c(FALSE, rep(TRUE, length(gp)-1))	
	for (l in 1:nlayers) {
		gpl <- gp[[l]]
		plot(gpl$shp, col=gpl$fill, border = gpl$col, lwd=gpl$lwd, lty=gpl$lty, add=add[l], xaxs="i", yaxs="i")
	}
	
	## set grid viewport (second line needed for small multiples)
	vps <- baseViewports()
	vps$figure[c("x", "y", "width", "height")] <- vps$plot[c("x", "y", "width", "height")]
	pushViewport(vps$inner, vps$figure, vps$plot)

	bb <- gp[[1]]$shp@bbox
 	ys <- convertY(unit(bb[2,], "native"), "npc", valueOnly=TRUE)
 	xs <- convertX(unit(bb[1,], "native"), "npc", valueOnly=TRUE)
	
	npc.w <- xs[2] - xs[1]
	npc.h <- ys[2] - ys[1]

	npc <- max(npc.w, npc.h)
	
 	ys.inch <- convertY(unit(bb[2,], "native"), "inch", valueOnly=TRUE)
 	xs.inch <- convertX(unit(bb[1,], "native"), "inch", valueOnly=TRUE)

	vpWidth <- ys.inch[2] - ys.inch[1]
 	vpHeight <- xs.inch[2] - xs.inch[1]

	aspVp <- vpWidth / vpHeight
	
	## correct npc's such that the aspect ratio will be preserved
	if (npc==npc.w) {
		npc.h <- npc.w * aspVp
	} else {
		npc.w <- npc.h / aspVp
	}

	pushViewport(viewport(layout=grid.layout(nrow=3, ncol=3, 
				widths=unit(c(1,npc.w, 1), c("null", "snpc", "null")),
				heights=unit(c(1,npc.h, 1), c("null", "snpc", "null")))))
	pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))	
	if (draw.frame) grid.rect(gp=gpar(fill=NA, lwd=frame.lwd))
	vpArea <- vpWidth * vpHeight
	scaleFactor <- (sqrt(vpArea) / 100)
	
	for (l in 1:nlayers) {
		
		gpl <- gp[[l]]
		shp <- gpl$shp
		
		npol <- length(shp)
		
		co <- coordinates(shp)
		bb <- shp@bbox
		
		co.npc <- co
		co.npc[,1] <- (co.npc[,1]-bb[1,1]) / (bb[1, 2]-bb[1,1])
		co.npc[,2] <- (co.npc[,2]-bb[2,1]) / (bb[2, 2]-bb[2,1])
		if (!is.na(gpl$bubble.size[1])) {
			sizes <- gpl$bubble.size
			if (length(sizes)!=npol) {
				if (length(sizes)!=1) warning("less bubble size values than objects")
				sizes <- rep(sizes, length.out=npol)
			}
				
			sizes <- sizes * scaleFactor
			
			cols <- rep(gpl$bubble.col, length.out=npol)
			borders <- gpl$bubble.border
			
			if (length(sizes)!=1) {
				decreasing <- order(-sizes)
				co.npc <- co.npc[decreasing,]
				sizes <- sizes[decreasing]
				cols <- if (length(cols)==1) cols else cols[decreasing]
			}
			grid.circle(x=unit(co.npc[,1], "npc"), y=unit(co.npc[,2], "npc"),
						r=unit(sizes, "inches"),
						gp=gpar(col=borders, fill=cols))
		}
		if (!is.na(gpl$text)) {
			labels <- shp[[gpl$text]]
			grid.text(labels, x=unit(co.npc[,1], "npc"), y=unit(co.npc[,2], "npc"), 
					  gp=gpar(cex=gpl$cex))
		}
	
	}
	popViewport(5)
	scaleFactor
}


plotAll <- function(gp) {
	gt <- gp$geo_theme
	gz <- gp$geo_zoom
	
	gp[c("geo_theme", "geo_zoom")] <- NULL
	
	margins <- gt$margins
	title.position <- gt$title.position
	
	
	gridLayoutMap <- grid.layout(3, 3, 
								 heights=unit(c(margins[3], 1, margins[1]), 
								 			 c("npc", "null", "npc")), 
								 widths=unit(c(margins[2], 1, margins[4]), 
								 			c("npc", "null", "npc")))
	if (!gt$legend.only) {
		pushViewport(viewport(layout=gridLayoutMap))
		cellplot(2, 2, e={
			par(new=TRUE, fig=gridFIG(), mai=c(0,0,0,0))#, xaxs="i", yaxs="i")
			scaleFactor <- plotMap(gp, gt, gz)
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
			gb <- gp[[bubbleSizeID]]
			gt$type.legend.plot <- ifelse(is.na(bubbleSizeID), "none", "bubble")
			cellplot(2, 2, e={
				legendPlot(gt=gt, 
						   legend.palette = if(is.na(bubbleColID)) gb$bubble.col else gb$bubble.legend.palette, 
						   legend.labels=gb$bubble.legend.labels, 
						   legend.bubbles=!is.na(bubbleSizeID), 
						   legend.bubble.sizes=gb$bubble.legend.sizes * scaleFactor, 
						   legend.bubble.labels=gb$bubble.legend.size_labels, 
						   plot.bubble.borders=TRUE)
			})
		}
	}
	
	#popViewport()
}