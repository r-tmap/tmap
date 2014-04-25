gridplot <- function(mfrow, mfcol, fun, nx, shps, gps, assign.to.vp=FALSE) {
	cl <- rw <- 1
	for (i in 1:nx) {
		# set grid layout at each new page page iteration
		if (i%%(mfrow*mfcol)==1 || (mfrow==1 && mfcol==1)) {
			pushViewport(viewport(layout=grid.layout(mfrow, mfcol)))
		}
		if (assign.to.vp) {
			do.call(fun, args=list(shps=shps,
								   gps=gps[[i]], 
								   vp=viewport(layout.pos.row=rw, 
								   			layout.pos.col=cl)))
		} else {
			cellplot(rw, cl, e=do.call(fun, args=list(shps, gps[[i]])))
		}
		
		cl <- cl + 1
		if (cl > mfcol) {
			cl <- 1; rw <- rw + 1
		}
		
		if (i%%(mfrow*mfcol)==0 && i<nx) {
			grid.newpage()
			rw <- 1
		}
	}
	upViewport(0)
	invisible()
}