
plot_map <- function(gp, gt, shps.env) {
	nlayers <- length(gp)
	
	shps <- get("shps", envir=shps.env)
	
	bubbleHeight <- convertHeight(unit(.5, "lines"), "inch", valueOnly=TRUE)
	
	bb <- shps[[1]]@bbox
	if (gt$grid.show && !gt$grid.on.top) plot_grid(gt, bb, scale=gt$scale)
	
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
		
		
		plot_tm_fill <- function() {
			fill <- if (is.null(gpl$fill)) NA else gpl$fill
			#if (inherits(shp, "SpatialPolygons")) {
				grid.shape(shp, gp=gpar(fill=fill, col=gpl$col, lwd=gpl$lwd, ltw=gpl$lty), bg.col=gt$bg.color)
			#}	
		}
		
		plot_tm_lines <- function() {
			#if (inherits(shp, "SpatialLines")) {
				grid.shplines(shp, gp=gpar(col=gpl$line.col, lwd=gpl$line.lwd, lty=gpl$line.lty,
										   lineend="butt"))
			#}
		}
		
		plot_tm_bubbles <- function() plot_bubbles(co.npc, gpl, bubbleHeight)
		plot_tm_text <- function() plot_text(co.npc, gpl)
		e <- environment()
		fnames <- paste("plot", gpl$plot.order, sep="_")
		lapply(fnames, do.call, args=list(), envir=e)
		
	}, gp, shps)
	
	if (gt$grid.show && gt$grid.on.top) plot_grid(gt, bb, scale=gt$scale)
	
	bubbleHeight
}

plot_grid <- function(gt, bb, scale) {
	
	gridx <- pretty(bb[1,], n=gt$grid.n.x)
	gridx <- gridx[gridx>bb[1,1] & gridx<bb[1,2]]
	gridy <- pretty(bb[2,], n=gt$grid.n.y)
	gridy <- gridy[gridy>bb[2,1] & gridy<bb[2,2]]
	
	cogridx <- (gridx-bb[1,1]) / (bb[1,2] - bb[1,1])
	cogridy <- (gridy-bb[2,1]) / (bb[2,2] - bb[2,1])
	
	labelsx <- format(gridx, big.mark = ",")
	labelsy <- format(gridy, big.mark = ",")
	
	
	labelsYw <- max(convertWidth(stringWidth(labelsy), "npc", valueOnly=TRUE)) * .75
	labelsXw <- max(convertHeight(stringHeight(labelsx), "npc", valueOnly=TRUE)) * .75
	spacerY <- convertWidth(unit(.75, "lines"), unitTo="npc", valueOnly=TRUE)
	spacerX <- convertHeight(unit(.75, "lines"), unitTo="npc", valueOnly=TRUE)
	
	selx <- cogridx >= labelsYw + spacerY
	sely <- cogridy >= labelsXw + spacerX
	
	if (any(selx)) {
		cogridx <- cogridx[selx]
		labelsx <- labelsx[selx]
		
		grid.polyline(x=rep(cogridx, each=2), y=rep(c(labelsXw+spacerX,1), length(cogridx)), 
					  id=rep(1:length(cogridx), each=2), gp=gpar(col=gt$grid.col, lwd=scale))
	}
	if (any(sely)) {
		cogridy <- cogridy[sely]
		labelsy <- labelsy[sely]
		
		grid.polyline(y=rep(cogridy, each=2), x=rep(c(labelsYw+spacerY,1), length(cogridy)), 
					  id=rep(1:length(cogridy), each=2), gp=gpar(col=gt$grid.col, lwd=scale))
	}
	
	if (any(selx)) grid.text(labelsx, y=labelsXw+spacerX*.5, x=cogridx, just="top", gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.cex*scale))
	if (any(sely)) grid.text(labelsy, x=labelsYw+spacerY*.5, y=cogridy, just="right", gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.cex*scale))
}

plot_bubbles <- function(co.npc, g, bubbleHeight) {
	with(g, {
		co.npc[, 1] <- co.npc[, 1] + bubble.xmod
		co.npc[, 2] <- co.npc[, 2] + bubble.ymod
		npol <- nrow(co.npc)
		if (length(bubble.size)!=npol) {
			if (length(bubble.size)!=1) warning("less bubble size values than objects")
			bubble.size <- rep(bubble.size, length.out=npol)
		}
		
		bubble.size <- bubble.size * bubbleHeight
		
		cols <- rep(bubble.col, length.out=npol)
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
					r=unit(bubble.size2, "inch"),
					gp=gpar(col=bubble.border.col, lwd=bubble.border.lwd, fill=cols2))
	})
}

plot_text <- function(co.npc, g, just=c("center", "center"), bg.margin=.10) {
	npol <- nrow(co.npc)
	with(g, {
		if (!any(text_sel)) {
			warning("No text to display. Check if all cex values are smaller than lowerbound.cex, or if all positions fall outside the plotting area.")
			return(NULL)
		}
		
		co.npc[, 1] <- co.npc[, 1] + text.xmod
		co.npc[, 2] <- co.npc[, 2] + text.ymod
		
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


plot_all <- function(gp, shps.env, dasp, sasp) {
	gt <- gp$tm_layout
	
	gp[c("tm_layout")] <- NULL
	
	if (!gt$legend.only) {
		## determine aspvp
		margins <- gt$outer.margins
		height <- 1 - sum(margins[c(1,3)])
		width <- 1 - sum(margins[c(2,4)])
		if (dasp > sasp) {
			width <- width * (sasp/dasp)
		} else {
			height <- height * (dasp/sasp)
		}
	
		if (!gt$draw.frame) grid.rect(gp=gpar(fill=gt$bg.color, col=NA))
		gridLayoutMap <- viewport(layout=grid.layout(3, 3, 
													 heights=unit(c(1, height, 1), 
													 			 c("null", "npc", "null")), 
													 widths=unit(c(1, width, 1), 
													 			c("null", "npc", "null"))),
								  name="maingrid")
		pushViewport(gridLayoutMap)
	
		cellplot(2, 2, name="aspvp", e={
			if (gt$draw.frame) grid.rect(gp=gpar(fill=gt$bg.color, col=NA))
			bubbleHeight <- plot_map(gp, gt, shps.env)
		})
		
		
		bgcol <- ifelse(gt$draw.frame, gt$outer.bg.color, gt$bg.color)
		cellplot(1,1:3, e=grid.rect(gp=gpar(col=bgcol, fill=bgcol)))
		cellplot(2,1, e=grid.rect(gp=gpar(col=bgcol, fill=bgcol)))
		cellplot(2,3, e=grid.rect(gp=gpar(col=bgcol, fill=bgcol)))
		cellplot(3,1:3, e=grid.rect(gp=gpar(col=bgcol, fill=bgcol)))

		cellplot(2,2, e={
			if (gt$draw.frame) grid.rect(gp=gpar(fill=NA, lwd=gt$frame.lwd)) else grid.rect(gp=gpar(col=gt$bg.color, fill=NA))
		})
		
		upViewport()
	} else {
		bubbleHeight <- convertHeight(unit(1, "lines"), "inch", valueOnly=TRUE) * gt$legend.text.cex * 2
	}
	
	#find statistic variables
	leg <- legend_prepare(gp, gt, bubbleHeight)
	
	if (!is.null(leg)) {
		if (!gt$legend.only) {
			d <- downViewport("aspvp")
		} else {
			grid.rect(gp=gpar(fill=gt$bg.color, col=NA))
		}
		legend_plot(gt, leg)
		if (!gt$legend.only) upViewport(d)
	}
}