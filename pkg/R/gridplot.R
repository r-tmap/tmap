gridplot <- function(mfrow, mfcol, fun, nx, gps, shps.env, dasp, sasp, legend_pos) {
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
		vpGrid <- viewport(layout=grid.layout(mfrow, mfcol, widths=unit(1/mfcol-1e-5, "npc"), heights=unit(1/mfrow-1e-5, "npc")), name = "multiples_grid")
		pushViewport(vpGrid)
		
		istart <- (k-1) * pp + 1
		iend <- min(istart + pp, nx)
		ni <- iend-istart+1
		treeMults <- mapply(function(i, rw, cl) {
			cellplot(rw, cl, e=do.call(fun, args=list(i, gps[[i]], shps.env, dasp, sasp, legend_pos)), name = paste("multiple", i, sep="_"))
		}, istart:iend, 
		rep(1:mfrow, each=mfcol, length.out=ni), 
		rep(1:mfcol, times=mfrow, length.out=ni), SIMPLIFY=FALSE)
		
		tree <- gTree(children=do.call("gList", treeMults), vp=vpGrid)
		#tree <- gTree(children=gList(polylineGrob(x=c(0.2, 0.7, 0.8, 0.5, 0.3, 0.2), y=c(0.1, 0.2, 0.9, 0.8, 0.2, 0.1), gp=gpar(col="#0000EE77", lwd=4), vp=viewport(layout.pos.row = 1, layout.pos.col = 1, clip=TRUE))), vp=vpGrid)
		grid.draw(tree)
	})
	
	upViewport(0)
	invisible()
}