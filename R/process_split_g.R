# split layer specification g for scaling the aesthetics
split_g <- function(g, n) {
	# single valued-arguments: small multiples can take vectors (vnames)
	# vector-argments: small multiples can take lists (lnames)
	# list-arguments: small multiples can take nested lists (nlnames)
	vnames <- c("col", "size", "shape", "lwd", "text", "alpha", "convert2density", "n", "style", "auto.palette.mapping", "colorNA", "textNA", "showNA", "interval.closure", "shapeNA", "shape.textNA","shapes.n", "shapes.style", "shapes.breaks", "shapes.interval.closure", "perceptual", "scale", "root", "size.lowerbound", "print.tiny")
	lnames <- c("palette", "breaks", "labels", "contrast", "size.lim", "size.lim", "sizes.legend", "sizes.legend.labels", "sizes.legend.values", "lwd.legend", "lwd.legend.labels", "lwd.legend.values", "line.lwd.legend.labels", "line.lwd.legend.values", "shapes")
	nlnames <- c("legend.format", "popup.format")
	lapply(1:n, function(i) {
		g[vnames] <- lapply(g[vnames], function(x) {
			if (length(x)==n) x[i] else x[1]
		})
		g[lnames] <- lapply(g[lnames], function(x) {
			if (is.list(x) && length(x)==n) x[[i]] else x
		})
		g[nlnames] <- lapply(g[nlnames], function(x) {
			if (all(vapply(x, is.list, logical(1))) && length(x)==n) x[[i]] else x
		})
		g
	})
}

split_list <- function(l, n) {
	if (n==1) {
		l
	} else {
		lapply(1:n, function(i) {
			mapply(function(x, y) {
				if (length(x)==n && !(y %in% c("legend.sizes"))) x[i] else x[1]
			}, l, names(l), SIMPLIFY=FALSE)
		})
	}
}






