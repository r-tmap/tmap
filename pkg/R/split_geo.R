split_geo <- function(gp, nx) {
	
	gpnx <- lapply(1:nx, function(i){
		g <- lapply(gp, function(x) {
			x[-2] <- lapply(x[-2], get_i, i, n=x$npol)
			x
		})
	})
	names(gpnx) <- paste0("plot", 1:nx)
	
	gpnx
}

get_i <- function(x, i, n) {
	if (is.null(x)) {
		(NULL)
	} else if (is.matrix(x)) {
		if (ncol(x)>=i) x[,i] else x[,1]
	} else if(is.list(x)) {
		if (length(x)>=i) x[[i]] else x[[1]]
	} else x
}
