split_tm <- function(gp, nx, order_by) {
	gpnx <- lapply(1:nx, function(i){
		g <- mapply(function(x, o) {
			oid <- if(is.null(o)) NULL else o[[i]]
			mapply(get_i, x, names(x), MoreArgs = list(i=i, n=x$npol, oid=oid), SIMPLIFY=FALSE)
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
		if (xname %in% c("varnames", "idnames")) {
			x
		} else if (substr(xname, ncx-10, ncx) %in% c("d.hist.misc", "legend.misc")) {
			mapply(get_i, x, names(x), MoreArgs = list(i=i, n=n, oid=NULL), SIMPLIFY=FALSE)
		} else {
			if (length(x)>=i) x[[i]] else x[[1]]
		}
	} else {
		ncx <- nchar(xname)
		if (xname %in% c("bubble.size.legend.palette", "bubble.max.size", "line.lwd.legend.palette", "line.legend.lwd") || substr(xname, ncx-11, ncx)=="legend.title") {
			if (length(x)>=i) x[i] else x[1]
		} else x
	}
}
