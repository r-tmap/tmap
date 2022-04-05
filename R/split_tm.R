split_tm <- function(gp, nx, order_by) {
	gpnx <- lapply(1:nx, function(i){
		g <- mapply(function(x, o) {
			oid <- if(is.null(o)) NULL else o[[i]]
			res <- mapply(get_i, x, names(x), MoreArgs = list(i=i, n=x$npol, oid=oid), SIMPLIFY=FALSE)
			res$npol <- if (is.null(oid)) x$npol else length(oid)
			res
		}, gp, order_by, SIMPLIFY=FALSE)
	})
	names(gpnx) <- paste0("plot", 1:nx)
	gpnx
}

get_i <- function(x, xname, i, n, oid) {
	if (is.null(oid) && is.matrix(x)) oid <- 1:nrow(x)
	if (is.null(x)) {
		NULL
	} else  if (is.matrix(x)) {
		if (ncol(x)>=i) x[oid,i] else x[oid,1]
	} else if (is.data.frame(x)) {
		stop("is.data.frame")
		if (ncol(x)>=i) x[oid,i] else x[oid,1]
	} else if(is.list(x)) {
		ncx <- nchar(xname)
		if (xname %in% c("varnames", "idnames", "clustering")  ||  substr(xname, ncx-11, ncx)=="popup.format") {
			x
		} else if (substr(xname, ncx-4, ncx) ==".misc") {
			# these are lists themselves
			mapply(get_i, x, names(x), MoreArgs = list(i=i, n=n, oid=NULL), SIMPLIFY=FALSE)
		} else {
			if (length(x)>=i) x[[i]] else x[[1]]
		}
	} else {
		ncx <- nchar(xname)
		# split variables that consist of one value, but may differ across small multiples
		if (xname %in% c("symbol.size.legend.palette", "symbol.border.lwd", "symbol.border.col", "symbol.max.size", "symbol.shape", "symbol.size.legend.palette", "symbol.shape.legend.palette", "line.lwd.legend.palette", "line.legend.lwd", "text.max.size", "text.size.legend.palette") || substr(xname, ncx-11, ncx)=="legend.title" || substr(xname, ncx-10, ncx)=="legend.show") {
			if (length(x)>=i) x[i] else x[1]
		} else x
	}
}
