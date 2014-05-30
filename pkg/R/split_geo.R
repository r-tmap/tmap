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
		if (ncol(x)>=i) x[,i] else x[,1]
	} else if(is.list(x)) {
		ncx <- nchar(xname)
		if (substr(xname, ncx-10, ncx) %in% c("gend.labels", "end.palette", "breaks")) {
			if (length(x)>=i) x[[i]] else x[[1]]
		} else if (substr(xname, ncx-10, ncx)=="legend.misc") {
			lapply(x, get_i, i, n)
		} else x
	} else x
}
