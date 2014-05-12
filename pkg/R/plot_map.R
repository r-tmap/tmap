

plot_map <- function(shps, gp, gt) {
	draw.frame <- gt$draw.frame
	frame.lwd <- gt$frame.lwd
	
	nlayers <- length(gp)
	
	## add shape objects to layers
	#gp <- lapply(gp, function(g) {g$shp <- get(g$shp); g})
	
	## set bounding box and frame
	sgp <- set_bounding_box(shps, gp, gt)
	shps <- lapply(sgp, function(x)x$shp)
	gp <- lapply(sgp, function(x)x$layer)
	
	## plot shapes
	add <- c(FALSE, rep(TRUE, length(gp)-1))
	
	#browser()
	dasp <- sgp[[1]]$dasp
	sasp <- sgp[[1]]$sasp
	
	if (dasp > sasp) {
		vp <- viewport(width=sasp/dasp)
	} else {
		vp <- viewport(height=dasp/sasp)
	}
	pushViewport(vp)
	if (draw.frame) grid.rect(gp=gpar(fill=gt$bg.color, col=NA))
	
	par(new=TRUE, fig=gridFIG(), xaxs="i", yaxs="i")
	for (l in 1:nlayers) {
		gpl <- gp[[l]]
		plot(shps[[l]], col=gpl$fill, bg=NA, border = gpl$col, lwd=gpl$lwd, lty=gpl$lty, add=add[l], xpd=TRUE)
	}

	
 	vpWidth <- convertWidth(unit(1, "npc"), "inch", valueOnly=TRUE)
  	vpHeight <- convertHeight(unit(1, "npc"), "inch", valueOnly=TRUE)

	if (draw.frame) grid.rect(gp=gpar(fill=NA, lwd=frame.lwd)) else grid.rect(gp=gpar(col=gt$bg.color, fill=NA))
	vpArea <- vpWidth * vpHeight
	scaleFactor <- (sqrt(vpArea) / 100)
	for (l in 1:nlayers) {
		
		gpl <- gp[[l]]
		shp <- shps[[l]]
		
		#npol <- length(shp)
		co <- coordinates(shp)
		
		bb <- shp@bbox
		
		co.npc <- co
		co.npc[,1] <- (co.npc[,1]-bb[1,1]) / (bb[1, 2]-bb[1,1])
		co.npc[,2] <- (co.npc[,2]-bb[2,1]) / (bb[2, 2]-bb[2,1])

		p_bubbles <- function() {
			if (!is.null(gpl$bubble.size)) {
				co.npc[, 1] <- co.npc[, 1] + gpl$bubble.xmod
				co.npc[, 2] <- co.npc[, 2] + gpl$bubble.ymod
				
				plot_bubbles(co.npc, gpl$bubble.size, gpl$bubble.col, gpl$bubble.border, scaleFactor)
			}
		}
		
		p_text <- function() {
			if (!is.null(gpl$text)) {
				cex <- gpl$text.cex
				if (is.character(cex)) {
					if (substr(cex, 1, 4)=="AREA") {
						nc <- nchar(cex)
						p <- if (nc>4) as.numeric(substr(cex, 5, nc)) else 2
						cex <- approx_areas(shp, units="norm")^(1/p)
					} else {
						cex <- shp[[gpl$text.cex]]
						cex <- cex / max(cex, na.rm=TRUE)
					}
				} else cex <- rep(cex, lenght.out=length(shp))
				gpl$text.fontcolor <- if (is.na(gpl$text.fontcolor[1])) {
					fillrgb <- col2rgb(gpl$fill)
					light <- apply(fillrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
					rep(ifelse(light, "black", "white"), length.out=length(shp))
				} else rep(gpl$text.fontcolor, length.out=length(shp))
				co.npc[, 1] <- co.npc[, 1] + gpl$text.xmod
				co.npc[, 2] <- co.npc[, 2] + gpl$text.ymod
				plot_text(co.npc, shp[[gpl$text]], cex, gpl$text.cex.lowerbound, gpl$text.fontcolor, gpl$text.bg.color, gpl$text.bg.alpha, gpl$text.scale, gpl$text.print.tiny, gpl$text.fontface, gpl$text.fontfamily)
			}
		}
		
		if (gpl$plotorder=="bubble_text") {
			p_bubbles()
			p_text()
		} else {
			p_text()
			p_bubbles()
		}
		
	}
	upViewport()
	list(scaleFactor=scaleFactor, vp=vp)
}


plot_bubbles <- function(co.npc, sizes, bubble.col, bubble.border, scaleFactor) {
	npol <- nrow(co.npc)
	if (length(sizes)!=npol) {
		if (length(sizes)!=1) warning("less bubble size values than objects")
		sizes <- rep(sizes, length.out=npol)
	}
	
	sizes <- sizes * scaleFactor
	
	cols <- rep(bubble.col, length.out=npol)
	borders <- bubble.border
	
	if (length(sizes)!=1) {
		decreasing <- order(-sizes)
		co.npc2 <- co.npc[decreasing,]
		sizes2 <- sizes[decreasing]
		cols2 <- if (length(cols)==1) cols else cols[decreasing]
	} else {
		co.npc2 <- co.npc
		sizes2 <- sizes
		col2 <- cols
	}
	grid.circle(x=unit(co.npc2[,1], "npc"), y=unit(co.npc2[,2], "npc"),
				r=unit(sizes2, "inches"),
				gp=gpar(col=borders, fill=cols2))
}

plot_text <- function(co.npc, labels, cex, text.cex.lowerbound, text.fontcolor, text.bg.color, text.bg.alpha, text.scale, text.print.tiny, text.fontface, text.fontfamily, just=c("center", "center"), bg.margin=.10) {
	npol <- nrow(co.npc)
	
	text_sel <- (cex >= text.cex.lowerbound)
	text_empty <- is.na(labels)
	
	if (text.print.tiny) {
		cex[!text_sel & !text_empty] <- text.cex.lowerbound
		text_sel <- !text_empty
	} else {
		text_sel <- text_sel & !text_empty
	}
	#cex[!text_sel] <- 0
	cex <- rep(cex * text.scale, length.out=npol)
	tG <- textGrob(labels[text_sel], x=unit(co.npc[text_sel,1], "npc"), y=unit(co.npc[text_sel,2], "npc"), just=just, gp=gpar(col=text.fontcolor[text_sel], cex=cex[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
	nlines <- rep(1, length(labels))
	
	
	if (!is.na(text.bg.color)) {
		bgcols <- col2rgb(text.bg.color)
		bgcols <- rgb(bgcols[1,], bgcols[2,], bgcols[3,], 
					  alpha=text.bg.alpha, maxColorValue=255)
		
		lineH <- convertHeight(unit(cex[text_sel], "lines"), "npc", valueOnly=TRUE)
		lineW <- convertWidth(unit(cex[text_sel], "lines"), "npc", valueOnly=TRUE)
		
		tGH <- mapply(labels[text_sel], cex[text_sel], nlines[text_sel], FUN=function(x,y,z){
			convertHeight(grobHeight(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"npc", valueOnly=TRUE) * z/(z-0.25)}, USE.NAMES=FALSE)
		
		tGW <- mapply(labels[text_sel], cex[text_sel], FUN=function(x,y){
			convertWidth(grobWidth(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"npc", valueOnly=TRUE)}, USE.NAMES=FALSE)
		tGX <- tG$x + unit(ifelse(just[1]=="left", (tGW * .5), 
								  ifelse(just[1]=="right", -(tGW * .5), 0)), "npc")
		tGY <- tG$y + unit(ifelse(just[2]=="top", -(tGH * .5), 
								  ifelse(just[2]=="bottom", tGH * .5, 0)), "npc")
	
		tGH <- tGH + lineH * bg.margin
		tGW <- tGW + lineW * bg.margin
		bcktG <- rectGrob(x=tGX, y=tGY, width=tGW, height=tGH, gp=gpar(fill=bgcols, col=NA))
		grid.draw(bcktG)
	}
	grid.draw(tG)
}


plot_all <- function(shps, gp) {
	main_vp <- current.viewport()
	gt <- gp$geo_theme
	
	gp[c("geo_theme")] <- NULL
	
	margins <- gt$outer.margins
	
	# set outer margins
	if (!gt$draw.frame) grid.rect(gp=gpar(fill=gt$bg.color, col=NA))
	gridLayoutMap <- viewport(layout=grid.layout(3, 3, 
								 heights=unit(c(margins[3], 1, margins[1]), 
								 			 c("npc", "null", "npc")), 
								 widths=unit(c(margins[2], 1, margins[4]), 
								 			c("npc", "null", "npc"))))
	
	# plot map
	if (!gt$legend.only) {
		pushViewport(gridLayoutMap)
		cellplot(2, 2, e={
			result <- plot_map(shps, gp, gt)
			scaleFactor <- result[[1]]
			vp <- result[[2]]
		})
		upViewport()
	}

	#find statistic variables
	leg <- legend_prepare(gp, gt, scaleFactor)
	
	
	
	if (!is.null(leg)) {
		if (!gt$legend.only) seekViewport(vp$name)	
		legend_plot(gt, leg)
	}
	
	
	seekViewport(main_vp$name)
}