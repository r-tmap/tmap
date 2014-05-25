
plot_map <- function(gp, gt, shps.env) {
	draw.frame <- gt$draw.frame
	frame.lwd <- gt$frame.lwd
	
	nlayers <- length(gp)
	
	shps <- get("shps", envir=shps.env)
	
	## plot shapes
	dasp <- attr(shps, "dasp")
	sasp <- attr(shps, "sasp")

	if (dasp > sasp) {
		gridLayoutMap <- viewport(layout=grid.layout(3, 3, 
			heights=unit(c(1, 1, 1), c("null", "npc", "null")), 
			widths=unit(c(1, sasp/dasp, 1), c("null", "npc", "null"))), 
			name="aspgrid")
		
		#vp <- viewport(width=unit(sasp/dasp, "npc"), height=unit(1, "npc"), name="aspvp")
	} else {
		gridLayoutMap <- viewport(layout=grid.layout(3, 3, 
			heights=unit(c(1, dasp/sasp, 1), c("null", "npc", "null")), 
			widths=unit(c(1, 1, 1), c("null", "npc", "null"))), 
			name="aspgrid")
								  
		#vp <- viewport(height=unit(dasp/sasp, "npc"), width=unit(1, "npc"), name="aspvp")
	}
	vp <- viewport(layout.pos.row=2, layout.pos.col=2, name="aspvp")
	pushViewport(gridLayoutMap, vp)
	if (draw.frame) grid.rect(gp=gpar(fill=gt$bg.color, col=NA))
	
	vpWidth <- convertWidth(unit(1, "npc"), "inch", valueOnly=TRUE)
	vpHeight <- convertHeight(unit(1, "npc"), "inch", valueOnly=TRUE)
	
	vpArea <- vpWidth * vpHeight
	scaleFactor <- (sqrt(vpArea) / 100)
	
	mapply(function(gpl, shp) {
		bb <- shp@bbox
		if (inherits(shp, "SpatialLines")) {
			co <- gCentroid(shp, byid=TRUE)@coords
		} else {
			co <- coordinates(shp)
		}
		co.npc <- co
		co.npc[,1] <- (co.npc[,1]-bb[1,1]) / (bb[1, 2]-bb[1,1])
		co.npc[,2] <- (co.npc[,2]-bb[2,1]) / (bb[2, 2]-bb[2,1])
		
		
		plot_geo_fill <- function() {
			#if (inherits(shp, "SpatialPolygons")) {
				grid.shape(shp, gp=gpar(fill=gpl$fill, col=gpl$col, lwd=gpl$lwd, ltw=gpl$lty))
			#}	
		}
		
		plot_geo_lines <- function() {
			#if (inherits(shp, "SpatialLines")) {
				grid.shplines(shp, gp=gpar(col=gpl$line.col, lwd=gpl$line.lwd, lty=gpl$line.lty))
			#}
		}
		
		plot_geo_bubbles <- function() plot_bubbles(co.npc, gpl, scaleFactor)
		plot_geo_text <- function() plot_text(co.npc, gpl)

		e <- environment()
		fnames <- paste("plot", gpl$plot.order, sep="_")
		lapply(fnames, do.call, args=list(), envir=e)
	}, gp, shps)
	
	
	
	upViewport()
	if (draw.frame) {
		cellplot(1,1:3, e=grid.rect(gp=gpar(col="white", fill="white")))
		cellplot(2,1, e=grid.rect(gp=gpar(col="white", fill="white")))
		cellplot(2,3, e=grid.rect(gp=gpar(col="white", fill="white")))
		cellplot(3,1:3, e=grid.rect(gp=gpar(col="white", fill="white")))
	}
	cellplot(2,2, e={
		if (draw.frame) grid.rect(gp=gpar(fill=NA, lwd=frame.lwd)) else grid.rect(gp=gpar(col=gt$bg.color, fill=NA))
	})
	
	upViewport()
	scaleFactor
}


plot_bubbles <- function(co.npc, g, scaleFactor) {
	with(g, {
		co.npc[, 1] <- co.npc[, 1] + bubble.xmod
		co.npc[, 2] <- co.npc[, 2] + bubble.ymod
		npol <- nrow(co.npc)
		if (length(bubble.size)!=npol) {
			if (length(bubble.size)!=1) warning("less bubble size values than objects")
			bubble.size <- rep(bubble.size, length.out=npol)
		}
		
		bubble.size <- bubble.size * scaleFactor
		
		cols <- rep(bubble.col, length.out=npol)
		borders <- bubble.border
		
		if (length(bubble.size)!=1) {
			decreasing <- order(-bubble.size)
			co.npc2 <- co.npc[decreasing,]
			bubble.size2 <- bubble.size[decreasing]
			cols2 <- if (length(cols)==1) cols else cols[decreasing]
		} else {
			co.npc2 <- co.npc
			bubble.size2 <- bubble.size
			col2 <- cols
		}
		grid.circle(x=unit(co.npc2[,1], "npc"), y=unit(co.npc2[,2], "npc"),
					r=unit(bubble.size2, "inches"),
					gp=gpar(col=borders, fill=cols2))
	})
}

plot_text <- function(co.npc, g, just=c("center", "center"), bg.margin=.10) {
	npol <- nrow(co.npc)
	with(g, {
		tG <- textGrob(text[text_sel], x=unit(co.npc[text_sel,1], "npc"), y=unit(co.npc[text_sel,2], "npc"), just=just, gp=gpar(col=text.fontcolor[text_sel], cex=text.cex[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
		nlines <- rep(1, length(text))
		
		
		if (!is.na(text.bg.color)) {
			
			lineH <- convertHeight(unit(text.cex[text_sel], "lines"), "npc", valueOnly=TRUE)
			lineW <- convertWidth(unit(text.cex[text_sel], "lines"), "npc", valueOnly=TRUE)
			
			tGH <- mapply(text[text_sel], text.cex[text_sel], nlines[text_sel], FUN=function(x,y,z){
				convertHeight(grobHeight(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"npc", valueOnly=TRUE) * z/(z-0.25)}, USE.NAMES=FALSE)
			
			tGW <- mapply(text[text_sel], text.cex[text_sel], FUN=function(x,y){
				convertWidth(grobWidth(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"npc", valueOnly=TRUE)}, USE.NAMES=FALSE)
			tGX <- tG$x + unit(ifelse(just[1]=="left", (tGW * .5), 
									  ifelse(just[1]=="right", -(tGW * .5), 0)), "npc")
			tGY <- tG$y + unit(ifelse(just[2]=="top", -(tGH * .5), 
									  ifelse(just[2]=="bottom", tGH * .5, 0)), "npc")
		
			tGH <- tGH + lineH * bg.margin
			tGW <- tGW + lineW * bg.margin
			bcktG <- rectGrob(x=tGX, y=tGY, width=tGW, height=tGH, gp=gpar(fill=text.bg.color, col=NA))
			grid.draw(bcktG)
		}
		grid.draw(tG)
	})
}


plot_all <- function(gp, shps.env) {
	gt <- gp$geo_theme
	
	gp[c("geo_theme")] <- NULL
	
	margins <- gt$outer.margins
	
	# set outer margins
	if (!gt$draw.frame) grid.rect(gp=gpar(fill=gt$bg.color, col=NA))
	gridLayoutMap <- viewport(layout=grid.layout(3, 3, 
								 heights=unit(c(margins[3], 1, margins[1]), 
								 			 c("npc", "null", "npc")), 
								 widths=unit(c(margins[2], 1, margins[4]), 
								 			c("npc", "null", "npc"))),
							  name="maingrid")
	
	# plot map
	if (!gt$legend.only) {
		pushViewport(gridLayoutMap)
		cellplot(2, 2, e={
			scaleFactor <- plot_map(gp, gt, shps.env)
		})
		upViewport()
	}

	#find statistic variables
	leg <- legend_prepare(gp, gt, scaleFactor)
	
	if (!is.null(leg)) {
		if (!gt$legend.only) {
			d <- downViewport("aspvp")
		}
		legend_plot(gt, leg)
		if (!gt$legend.only) upViewport(d)
	}
}