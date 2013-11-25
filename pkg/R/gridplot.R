gridplot <- function(mfrow, mfcol, fun, nx, args.fixed, args.var, assign.to.vp=FALSE, ...) {
	cl <- rw <- 1
	args.var <- lapply(args.var, rep, length.out=nx)
	args.dots <- list(...)
	for (i in 1:nx) {
		if (i%%(mfrow*mfcol)==1 || (mfrow==1 && mfcol==1)) {
			pushViewport(viewport(layout=grid.layout(mfrow, mfcol)))
		}
		args.once <- lapply(args.var, function(x)x[[i]])
		if (assign.to.vp) {
			args.once <- c(args.once, 
						   list(vp=viewport(layout.pos.row=rw, layout.pos.col=cl)))
			do.call(fun, args=c(args.fixed, args.once, args.dots))
		} else {
			cellplot(rw, cl, e=do.call(fun, args=c(args.fixed, args.once, args.dots)))
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
	popViewport()
	invisible()
}

gridplot2 <- function(mfrow, mfcol, fun, nx, gps, assign.to.vp=FALSE) {
	cl <- rw <- 1
	for (i in 1:nx) {
		# set grid layout at each new page page iteration
		if (i%%(mfrow*mfcol)==1 || (mfrow==1 && mfcol==1)) {
			pushViewport(viewport(layout=grid.layout(mfrow, mfcol)))
		}
		if (assign.to.vp) {
			do.call(fun, args=list(gps=gps[[i]], 
								   vp=viewport(layout.pos.row=rw, 
								   			layout.pos.col=cl)))
		} else {
			cellplot(rw, cl, e=do.call(fun, args=c(list(gps[[i]]))))
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
	popViewport()
	invisible()
}