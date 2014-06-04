split_geo <- function(gp, nx) {
	gpnx <- lapply(1:nx, function(i){
		g <- lapply(gp, function(x) {
			lapply(x, get_i, i, n=x$npol)
		})
	})
	names(gpnx) <- paste0("plot", 1:nx)
	gpnx
}

get_i <- function(x, i, n) {
	xname <- eval.parent(quote(names(X)))[substitute(x)[[3]]]
	if (is.null(x)) {
		(NULL)
	} else if (is.matrix(x)) {
		if (ncol(x)>=i) x[,i] else x[,1]
	} else if (is.data.frame(x)) {
		stop("is.data.frame")
		if (ncol(x)>=i) x[,i] else x[,1]
	} else if(is.list(x)) {
		ncx <- nchar(xname)
		if (xname == "varnames") {
			x
		} else if (substr(xname, ncx-10, ncx)=="legend.misc") {
			lapply(x, get_i, i, n)
		} else {
			if (length(x)>=i) x[[i]] else x[[1]]
		}
	} else {
		ncx <- nchar(xname)
		if (xname %in% c("bubble.size.legend.palette", "bubble.max.size", "line.lwd.legend.palette", "line.legend.lwd")) {
			if (length(x)>=i) x[i] else x[1]
		} else x
	}
}
