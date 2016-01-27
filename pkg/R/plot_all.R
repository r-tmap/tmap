plot_all <- function(i, gp, shps.env, dasp, sasp, inner.margins.new, legend_pos) {
	gt <- gp$tm_layout
	
	shps <- get("shps", envir=shps.env)
	
	## in case of small multiples, get i'th shape
	if (any(gt$shp_nr!=0) && (gt$drop.shapes || gt$free.coords)) {
		shps <- shps[[i]]
	}
	
	bbx <- attr(shps[[1]], "bbox")
	proj <- attr(shps[[1]], "proj4string")@projargs
	
	if (gt$grid.show) {
		# non inverse projection avaiable PROJ.4 4.8.0 for Winkel Tripel projection
		PROJ4_version_nr <- get_proj4_version()

		if (length(grep("+proj=wintri", proj, fixed = TRUE)) && PROJ4_version_nr < 491 && !is.na(gt$grid.projection)) {
			warning("Unable to reproject grid lines from the Winkel Triple projection with PROJ.4 version < 4.9.1", call. = FALSE)
			gt$grid.show <- FALSE
		} else {
			gt <- process_grid(gt, bbx, proj, sasp)
		}
	}
	
	
	
	
	gp[c("tm_layout")] <- NULL
	
	if (!gt$legend.only) {
		## calculate width and height of the shape based on the device asp ratio (dasp) and the shape aspect ratio (sasp)
		margins <- gt$outer.margins
		mar.y <- sum(margins[c(1,3)])
		mar.x <- sum(margins[c(2,4)])
		
		height <- 1 - mar.y
		width <- 1 - mar.x
		if (dasp > sasp) {
			width <- width * (sasp/dasp)
		} else {
			height <- height * (dasp/sasp)
		}
		
		## calculate outer margins
		margin.left <- (1 - width) * ifelse(mar.x==0, .5, (margins[2]/mar.x))
		margin.top <- (1 - height) * ifelse(mar.y==0, .5, (margins[3]/mar.y))
		
		
		## background rectangle (whole device)
		grobBG <- if (gt$design.mode) {
			rectGrob(gp=gpar(fill="yellow", col="yellow"), name="bg_rect")
		} else if (is.na(gt$frame) && !gt$earth.boundary) {
			rectGrob(gp=gpar(fill=gt$bg.color, col=NA), name="bg_rect")
		} else if (is.na(gt$frame) && gt$earth.boundary) {
			rectGrob(gp=gpar(fill=gt$space.color, col=NA), name="bg_rect")
		} else if (!is.null(gt$outer.bg.color) && !is.na(gt$frame)) {
			rectGrob(gp=gpar(col=gt$outer.bg.color, fill=gt$outer.bg.color), name="bg_rect")
		} else NULL

		
		## create a 3x3 grid layout with the shape to be drawn in the middle cell
		gridLayoutMap <- viewport(layout=grid.layout(3, 3, 
													 heights=unit(c(margin.top, height, 1), 
													 			 c("npc", "npc", "null")), 
													 widths=unit(c(margin.left, width, 1), 
													 			c("npc", "npc", "null"))),
								  name="maingrid")
		pushViewport(gridLayoutMap)
		
		## the thematic map with background
		treeMap <- cellplot(2, 2, name="aspvp", e={
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
			res <- plot_map(i, gp, gt, shps, bbx, proj, sasp)
			treeElemGrid <- res$treeElemGrid
			lineInch <- res$lineInch
			metaX <- res$metaX
			metaY <- res$metaY
			gList(grobBGframe, grobAsp, treeElemGrid)
		})
		
		treeFrame <- cellplot(2,2, e={
			if (!is.na(gt$frame)) {
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
				rectGrob(gp=gpar(col=gt$bg.color, fill=NA))
			} else NULL
		}, name="mapFrame")
		
		treeGridLabels <- if (gt$grid.show && !gt$grid.labels.inside.frame) {
			gTree(children=gList(
				cellplot(3,2, e=plot_grid_labels_x(gt, scale=gt$scale), name="gridLabelsX"),
				cellplot(2,1, e=plot_grid_labels_y(gt, scale=gt$scale), name="gridLabelsY")), name="gridLabels")
		} else NULL
		
		
		
		treeMapX <- gTree(children=gList(grobBG, gTree(children=gList(treeMap, treeFrame, treeGridLabels), vp=gridLayoutMap, name="outer_map")), name="BG")
		
		upViewport()
	} else {
		## bubble height needed to align with bubbles in legend
		lineInch <- convertHeight(unit(1, "lines"), "inch", valueOnly=TRUE) * gt$legend.text.size
		treeMapX <- NULL
		metaX <- 0
		metaY <- 0
	}
	
	## prepare legend items
	leg <- legend_prepare(gp, gt, lineInch)
	
	## legend, title, and other thinks such as compass
	if (!is.null(leg) || gt$title!="" || gt$credits.show || gt$scale.show || gt$compass.show) {
		if (!gt$legend.only) {
			vpLeg <- vpPath("maingrid", "aspvp")
			d <- downViewport(vpLeg)
			grobLegendBG <- NULL
			treeMetaVP <- vpList(gridLayoutMap, viewport(layout.pos.row=2, layout.pos.col=2, name="meta_aspvp", clip=TRUE))
		} else {
			vpLeg <- current.viewport()
			grobLegendBG <- rectGrob(gp=gpar(fill=gt$bg.color, col=NA))
			treeMetaVP <- NULL
		}
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
		treeMeta <- meta_plot(gt, leg, legend_pos, bbx, metaX, metaY, frameX, frameY)
		treeMetaX <- gTree(children=gList(grobLegendBG, treeMeta), name="meta_with_bg", 
						   vp = treeMetaVP)
		
		if (!gt$legend.only) {
			treeMapX <- addGrob(treeMapX, child=treeMetaX, gPath=gPath("outer_map"))#, "aspvp"))
			upViewport(d)
		} else {
			treeMapX <- treeMetaX
		}
		
	}
	
	treeMapX
}
