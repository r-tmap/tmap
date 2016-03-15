gridplot <- function(gmeta, fun, nx, gps, shps, dasp, sasp, inner.margins.new, legend_pos, gp_leg) {
	mfrow <- gmeta$nrow
	mfcol <- gmeta$ncol

	## number of pages
	np <- ceiling(nx / (mfrow * mfcol))
	
	## number of plots (small multiples) per page
	pp <- min(mfrow * mfcol, nx)
	
	## panels
	panel.mode <- gmeta$panel.mode
	panel.names <- gmeta$panel.names
	
	## number of grid rows and colums
	ncl <- length(gmeta$colws)
	nrw <- length(gmeta$rowhs)
	
	multi_shapes <- (is.list(shps[[1]]))
	
	if (multi_shapes) {
		bbxproj <- lapply(shps, function(s) {
			s2 <- s[[1]]
			if (is.null(s2)) NULL else list(bbx = attr(s2, "bbox"), proj = attr(s2, "proj4string")@projargs)
		})
	} else {
		bbxproj <- list(bbx = attr(shps[[1]], "bbox"), proj = attr(shps[[1]], "proj4string")@projargs)
	}
	
	external_grid_labels <- gmeta$grid.show && !gmeta$grid.labels.inside.frame
	
	
	## create a large grid tree per page, and draw it
	treeMlts <- lapply(1:np, function(k) {
		if (k!=1) {
			grid.newpage()
		}
		
		## background rect
		grobBG <- if (gmeta$design.mode) {
			rectGrob(gp=gpar(fill="yellow", col=NA), name="bg_rect")
		} else if (is.na(gmeta$frame) && !gmeta$earth.boundary) {
			rectGrob(gp=gpar(fill=gmeta$bg.color, col=NA), name="bg_rect")
		} else if (is.na(gmeta$frame) && gmeta$earth.boundary) {
			rectGrob(gp=gpar(fill=gmeta$space.color, col=NA), name="bg_rect")
		} else if (!is.null(gmeta$outer.bg.color) && !is.na(gmeta$frame)) {
			rectGrob(gp=  gpar(col=gmeta$outer.bg.color, fill=gmeta$outer.bg.color), name="bg_rect")
		} else NULL
		
		## set grid layout
		vpGrid <- viewport(layout=grid.layout(nrw, ncl, 
											  widths=unit(gmeta$colws, "npc"), 
											  heights=unit(gmeta$rowhs, "npc")), name = "multiples_grid")
		pushViewport(vpGrid)
		
		## additional background rect for design mode only
		grobFacetBG <- if (gmeta$design.mode) {
			cellplot(3:(length(gmeta$rowhs)-2), 3:(length(gmeta$colws)-2), e=rectGrob(gp=gpar(fill="brown", col=NA), name="bg_facets_rect"))
		} else NULL
		
		## draw maps
		istart <- (k-1) * pp + 1
		iend <- min(istart + pp-1, nx)
		ni <- iend-istart+1
		treeMults <- mapply(function(i, rw, cl) {
			cellplot(rw, cl, e=do.call(fun, args=list(i, gps[[i]], shps, dasp, sasp, inner.margins.new, legend_pos, nx>1)), name = paste("multiple", i, sep="_"))
		}, istart:iend, 
		rep(gmeta$rowrange, each=mfcol, length.out=ni), 
		rep(gmeta$colrange, times=mfrow, length.out=ni), SIMPLIFY=FALSE)
		
		## draw outside grid labels
		treeGridLabels <- if (external_grid_labels) {
			mapply(function(i, rw, cl) {
				if (multi_shapes) {
					proj <- bbxproj[[i]]$proj
					bbx <- bbxproj[[i]]$bbx
				} else {
					proj <- bbxproj$proj
					bbx <- bbxproj$bbx
				}
				gt <- gps[[i]]$tm_layout
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
				gTree(children=gList(
					cellplot((rw+1):nrw,cl, e=plot_grid_labels_x(gt, scale=gt$scale), name="gridLabelsX"),
					cellplot(rw,1:(cl-1), e=plot_grid_labels_y(gt, scale=gt$scale), name="gridLabelsY")), name=paste("gridLabels", i, sep="_"))
			}, istart:iend, 
			rep(gmeta$rowrange, each=mfcol, length.out=ni), 
			rep(gmeta$colrange, times=mfrow, length.out=ni), SIMPLIFY=FALSE)
		} else NULL
		
		## draw panels		
		if (panel.mode=="both") {
			rowPanels <- lapply((1:mfrow), function(i) {
				cellplot(gmeta$rowrange[i], gmeta$colpanelrow, e=gList(rectGrob(gp=gpar(fill=gmeta$panel.label.bg.color, lwd=gmeta$frame.lwd)),
									   textGrob(gmeta$panel.names[[1]][i], rot=gmeta$panel.label.rot[1], gp=gpar(col=gmeta$panel.label.color, cex=gmeta$panel.label.size))))
			})
			
			colPanels <- lapply((1:mfcol), function(i) {
				cellplot(gmeta$colpanelrow, gmeta$colrange[i], e=gList(rectGrob(gp=gpar(fill=gmeta$panel.label.bg.color, lwd=gmeta$frame.lwd)),
									   textGrob(gmeta$panel.names[[2]][i], rot=gmeta$panel.label.rot[2], gp=gpar(col=gmeta$panel.label.color, cex=gmeta$panel.label.size))))
			})
		}  else if (panel.mode=="one") {
			colPanels <- mapply(function(i, rw, cl) {
				cellplot(rw, cl, e=gList(rectGrob(gp=gpar(fill=gmeta$panel.label.bg.color, lwd=gmeta$frame.lwd)),
										 textGrob(gmeta$panel.names[i], rot=gmeta$panel.label.rot[2], gp=gpar(col=gmeta$panel.label.color, cex=gmeta$panel.label.size))))
			}, istart:iend, 
			rep(gmeta$rowrange-1, each=mfcol, length.out=ni), 
			rep(gmeta$colrange, times=mfrow, length.out=ni), SIMPLIFY=FALSE)
			rowPanels <- NULL
		} else {
			rowPanels <- NULL
			colPanels <- NULL
		}

		## draw outside legend
		if (!is.null(gp_leg)) {
			legPanel <- gList(cellplot(gmeta$legy, gmeta$legx, e=do.call(fun, args=list(1, gp_leg, shps, dasp, sasp, inner.margins.new, legend_pos, nx>1)), name = "outside_legend"))
		} else {
			legPanel <- NULL
		}
		
		tree <- gTree(children=do.call("gList", c(list(grobBG, grobFacetBG), treeGridLabels, treeMults, rowPanels, colPanels, legPanel)), vp=vpGrid)
		grid.draw(tree)
	})
	upViewport()
	invisible()
}
