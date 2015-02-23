gridplot <- function(mfrow, mfcol, fun, nx, gps, shps.env, dasp, sasp, legend_pos) {
	cl <- rw <- 1
	
	
	np <- ceiling(nx / (mfrow * mfcol))
	
	pp <- min(mfrow * mfcol, nx)

	treeMlts <- lapply(1:np, function(k) {
		if (k!=1) {
			grid.newpage()
		}
		
		vpGrid <- viewport(layout=grid.layout(mfrow, mfcol), name = "multiples_grid")
		pushViewport(vpGrid)
		
		istart <- (k-1) * pp + 1
		iend <- min(istart + pp, nx)
		ni <- iend-istart+1
		treeMults <- mapply(function(i, rw, cl) {
			cellplot2(rw, cl, e=do.call(fun, args=list(gps[[i]], shps.env, dasp, sasp, legend_pos)), name = paste("multiple", i, sep="_"))
		}, istart:iend, 
		rep(1:mfrow, each=mfcol, length.out=ni), 
		rep(1:mfcol, times=mfrow, length.out=ni), SIMPLIFY=FALSE)
		
		tree <- gTree(children=do.call("gList", treeMults), vp=vpGrid)
		grid.draw(tree)
	})
	
	upViewport(0)
	invisible()
}