gridplot <- function(mfrow, mfcol, fun, nx, gps, shps, dasp, sasp, inner.margins.new, legend_pos, xs, ys, design.mode) {
	#cl <- rw <- 1
	
	## number of pages
	np <- ceiling(nx / (mfrow * mfcol))
	
	## number of plots (small multiples) per page
	pp <- min(mfrow * mfcol, nx)
	
	## create a large grid tree per page, and draw it
	treeMlts <- lapply(1:np, function(k) {
		if (k!=1) {
			grid.newpage()
		}
		dev_rect <- if (design.mode) rectGrob(gp=gpar(fill="yellow", col="yellow"), name="dev_rect") else NULL

		vpGrid <- viewport(layout=grid.layout(mfrow+2, mfcol+2, 
											  widths=unit(c(xs/2, rep((1-xs)/mfcol, mfcol), xs/2), "npc"), 
											  heights=unit(c(ys/2, rep((1-ys)/mfrow, mfrow), ys/2), "npc")), name = "multiples_grid")
		pushViewport(vpGrid)
		
		istart <- (k-1) * pp + 1
		iend <- min(istart + pp-1, nx)
		ni <- iend-istart+1
		treeMults <- mapply(function(i, rw, cl) {
			cellplot(rw, cl, e=do.call(fun, args=list(i, gps[[i]], shps, dasp, sasp, inner.margins.new, legend_pos, nx>1)), name = paste("multiple", i, sep="_"))
		}, istart:iend, 
		rep(1:mfrow, each=mfcol, length.out=ni)+1, 
		rep(1:mfcol, times=mfrow, length.out=ni)+1, SIMPLIFY=FALSE)
		
		tree <- gTree(children=do.call("gList", c(list(dev_rect), treeMults)), vp=vpGrid)
		#tree <- gTree(children=gList(polylineGrob(x=c(0.2, 0.7, 0.8, 0.5, 0.3, 0.2), y=c(0.1, 0.2, 0.9, 0.8, 0.2, 0.1), gp=gpar(col="#0000EE77", lwd=4), vp=viewport(layout.pos.row = 1, layout.pos.col = 1, clip=TRUE))), vp=vpGrid)
		grid.draw(tree)
	})
	upViewport()
	invisible()
}
