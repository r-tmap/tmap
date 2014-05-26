
plot_map <- function(gp, gt, shps.env) {
	nlayers <- length(gp)
	
	shps <- get("shps", envir=shps.env)
	
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


plot_all <- function(gp, shps.env, dasp, sasp) {
	gt <- gp$geo_theme
	
	gp[c("geo_theme")] <- NULL
	
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
			scaleFactor <- plot_map(gp, gt, shps.env)
		})
		if (gt$draw.frame) {
			cellplot(1,1:3, e=grid.rect(gp=gpar(col=gt$outer.bg.color, fill=gt$outer.bg.color)))
			cellplot(2,1, e=grid.rect(gp=gpar(col=gt$outer.bg.color, fill=gt$outer.bg.color)))
			cellplot(2,3, e=grid.rect(gp=gpar(col=gt$outer.bg.color, fill=gt$outer.bg.color)))
			cellplot(3,1:3, e=grid.rect(gp=gpar(col=gt$outer.bg.color, fill=gt$outer.bg.color)))
		}
		cellplot(2,2, e={
			if (gt$draw.frame) grid.rect(gp=gpar(fill=NA, lwd=gt$frame.lwd)) else grid.rect(gp=gpar(col=gt$bg.color, fill=NA))
		})
		
		upViewport()
	}

	#find statistic variables
	leg <- legend_prepare(gp, gt, scaleFactor)
	
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