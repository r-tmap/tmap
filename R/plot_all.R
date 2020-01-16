plot_all <- function(i, gp, gal, shps, dasp, sasp, inner.margins.new, legend_pos, use_facets) {
	
	gt <- gp$tm_layout
	
	## in case of small multiples, get i'th shape
	if (any(gt$shp_nr!=0) && (gt$drop.units || gt$free.coords)) {
		shps <- shps[[i]]
	}

	emptyShp <- is.null(shps[[gt$shape.masterID]]) || (nrow(shps[[gt$shape.masterID]]) == 0)
	
	#if (emptyShp && gt$legend.only) return(NULL)
	if (!emptyShp) {
		bbx <- attr(shps[[gt$shape.masterID]], "bbox")
		proj <- sf::st_crs(shps[[gt$shape.masterID]])
		
		if (gt$grid.show) {
			gt <- process_grid(gt, bbx, proj, sasp)
		}
	}

	gp[c("tm_layout")] <- NULL
	
	if (!gt$legend.only) {
		## calculate width and height of the shape based on the device asp ratio (dasp) and the shape aspect ratio (sasp)
		
		## background rectangle (inside frame)
		if (!is.na(gt$frame)) {
			grobBGframe <- rectGrob(gp=gpar(fill=gt$bg.color, col=NA), name="mapBG")
		} else {
			grobBGframe <- NULL
		}
		
		if (gt$design.mode) {
			grobBGframe <- rectGrob(gp=gpar(fill="blue", col="blue"), name="mapBG")
			
			aspWidth <- 1-sum(inner.margins.new[c(2,4)])
			aspHeight <- 1-sum(inner.margins.new[c(1,3)])
			grobAsp <- rectGrob(x = (inner.margins.new[2]+1-inner.margins.new[4])/2, y=(inner.margins.new[1]+1-inner.margins.new[3])/2, width=aspWidth, height=aspHeight, gp=gpar(fill="red", col="red"), name="aspRect")
		} else {
			grobAsp <- NULL
		}

		## the thematic map
		
		if (!emptyShp) {
			res <- plot_map(i, gp, gt, shps, bbx, proj, sasp)
			treeElemGrid <- res$treeElemGrid
			lineInch <- res$lineInch
			metaX <- res$metaX
			metaY <- res$metaY
			treeMap <- gList(grobBGframe, grobAsp, treeElemGrid)
		} else {
			treeMap <-gList(grobBGframe, grobAsp)
		}
		
		treeFrame <- if (!is.na(gt$frame)) {
			pH <- convertHeight(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*gt$frame.lwd
			pW <- convertWidth(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*gt$frame.lwd
			if (gt$frame.double.line) {
				gList(
					rectGrob(width = 1-4*pW, height=1-4*pH, gp=gpar(col=gt$bg.color, fill=NA, lwd=5*gt$frame.lwd, lineend="square")),
					rectGrob(gp=gpar(col=gt$frame, fill=NA, lwd=3*gt$frame.lwd, lineend="square")),
					rectGrob(width = 1-8*pW, height=1-8*pH, gp=gpar(col=gt$frame, fill=NA, lwd=gt$frame.lwd, lineend="square")))
			} else {
				rectGrob(gp=gpar(col=gt$frame, fill=NA, lwd=gt$frame.lwd, lineend="square"))
			}
		} else if (!gt$earth.boundary) {
			NULL
			#rectGrob(gp=gpar(col=gt$bg.color, fill=NA)) # to prevent polygon lines at cropped rect. Solved with wider bouding box and clipping enabled
		} else NULL
		
		#treeMapX <- gTree(children=gList(grobBG, gTree(children=gList(treeMap, treeFrame), vp=gridLayoutMap, name="outer_map")), name="BG")

		if (emptyShp) {
			treeMapX <- gTree(children=gList(treeMap, treeFrame), name="BG")
			return(treeMapX)	
		}  else {
			treeMapX <- gTree(children=gList(treeMap, treeFrame), name="BG", vp = viewport(xscale = bbx[c(1,3)], yscale = bbx[c(2,4)]))
			
		}
		
		#upViewport()
	} else {
		treeMapX <- NULL
		## bubble height needed to align with bubbles in legend
		lineInch <- convertHeight(unit(1, "lines"), "inch", valueOnly=TRUE)# * gt$legend.text.size
		metaX <- 0
		metaY <- 0
	}

	## prepare legend items
	leg <- legend_prepare(gp, gal, gt, lineInch)

		

	## legend, title, and other thinks such as compass
	if (!is.null(leg) || nonempty_text(gt$title) || gt$credits.show || gt$logo.show || gt$scale.show || gt$compass.show) {
		if (!is.na(gt$frame)) {
			pH <- convertHeight(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*gt$frame.lwd
			pW <- convertWidth(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*gt$frame.lwd
			if (gt$frame.double.line) {
				frameX <- 4.5 * pW
				frameY <- 4.5 * pH
			} else {
				frameX <- pW/2
				frameY <- pH/2
			}
		} else {
			frameX <- 0
			frameY <- 0
		}
		treeMeta <- meta_plot(gt, leg, legend_pos, bbx, metaX, metaY, frameX, frameY, use_facets)
		treeMetaX <- gTree(children=gList(treeMeta), name="meta_with_bg") #, vp = treeMetaVP) # previously with grobLegendBG
		if (!gt$legend.only) {
			treeMapX <- addGrob(treeMapX, child=treeMetaX) #, gPath=gPath("outer_map"))#, "aspvp"))
			#upViewport(d)
		} else {
			treeMapX <- if (gt$design.mode) {
				attrBG <- rectGrob(gp=gpar(fill="purple", col="black"), name="attrBG")
				gTree(children=gList(attrBG, treeMetaX), name="meta_with_bg")
			} else {
				treeMetaX
			}
		}
	}
	treeMapX
}
