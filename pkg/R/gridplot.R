gridplot <- function(gmeta, fun, nx, gps, shps, dasp, sasp, fasp, inner.margins.new, legend_pos) {
	#cl <- rw <- 1
	
	mfrow <- gmeta$nrow
	mfcol <- gmeta$ncol
	
	## number of pages
	np <- ceiling(nx / (mfrow * mfcol))
	
	## number of plots (small multiples) per page
	pp <- min(mfrow * mfcol, nx)
	
	## panels
	panel.mode <- gmeta$panel.mode
	panel.names <- gmeta$panel.names
	
	
	## create a large grid tree per page, and draw it
	treeMlts <- lapply(1:np, function(k) {
		if (k!=1) {
			grid.newpage()
		}
		dev_rect <- if (gmeta$design.mode) rectGrob(gp=gpar(fill="yellow", col="yellow"), name="dev_rect") else NULL

		vpGrid <- viewport(layout=grid.layout(length(gmeta$rowhs), length(gmeta$colws), 
											  widths=unit(gmeta$colws, "npc"), 
											  heights=unit(gmeta$rowhs, "npc")), name = "multiples_grid")
		pushViewport(vpGrid)
		
		istart <- (k-1) * pp + 1
		iend <- min(istart + pp-1, nx)
		ni <- iend-istart+1
		treeMults <- mapply(function(i, rw, cl) {
			cellplot(rw, cl, e=do.call(fun, args=list(i, gps[[i]], shps, dasp, sasp, inner.margins.new, legend_pos, nx>1)), name = paste("multiple", i, sep="_"))
		}, istart:iend, 
		rep(gmeta$rowrange, each=mfcol, length.out=ni), 
		rep(gmeta$colrange, times=mfrow, length.out=ni), SIMPLIFY=FALSE)
		
		if (panel.mode=="both") {
			rowPanels <- lapply((1:mfrow), function(i) {
				cellplot(gmeta$rowrange[i], gmeta$colpanelrow, e=gList(rectGrob(gp=gpar(fill=gmeta$panel.label.bg.color[1])),
									   textGrob(gmeta$panel.names[[1]][i], rot=gmeta$panel.label.rot[1], gp=gpar(col=gmeta$panel.label.color, cex=gmeta$panel.label.size))))
			})
			
			colPanels <- lapply((1:mfcol), function(i) {
				cellplot(gmeta$colpanelrow, gmeta$rowrange[i], e=gList(rectGrob(gp=gpar(fill=gmeta$panel.label.bg.color[2])),
									   textGrob(gmeta$panel.names[[2]][i], rot=gmeta$panel.label.rot[2], gp=gpar(col=gmeta$panel.label.color, cex=gmeta$panel.label.size))))
			})
		}  else if (panel.mode=="one") {
			rowPanels <- mapply(function(i, rw, cl) {
				cellplot(rw, cl, e=gList(rectGrob(gp=gpar(fill=gmeta$panel.label.bg.color[2])),
										 textGrob(gmeta$panel.names[i], rot=gmeta$panel.label.rot[2], gp=gpar(col=gmeta$panel.label.color, cex=gmeta$panel.label.size))))
			}, istart:iend, 
			rep(gmeta$rowrange-1, each=mfcol, length.out=ni), 
			rep(gmeta$colrange, times=mfrow, length.out=ni), SIMPLIFY=FALSE)
			colPanels <- NULL
		} else {
			rowPanels <- NULL
			colPanels <- NULL
		}

		tree <- gTree(children=do.call("gList", c(list(dev_rect), treeMults, rowPanels, colPanels)), vp=vpGrid)
		#tree <- gTree(children=gList(polylineGrob(x=c(0.2, 0.7, 0.8, 0.5, 0.3, 0.2), y=c(0.1, 0.2, 0.9, 0.8, 0.2, 0.1), gp=gpar(col="#0000EE77", lwd=4), vp=viewport(layout.pos.row = 1, layout.pos.col = 1, clip=TRUE))), vp=vpGrid)
		grid.draw(tree)
	})
	upViewport()
	invisible()
}
