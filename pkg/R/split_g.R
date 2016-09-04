split_g <- function(g, n) {
	# single valued-arguments: small multiples can take vectors (vnames)
	# vector-argments: small multiples can take lists (lnames)
	# list-arguments: small multiples can take nested lists (nlnames)
	vnames <- c("alpha", "convert2density", "n", "style", "auto.palette.mapping", "contrast", "max.categories", "colorNA", "textNA", "symbol.border.col", "symbol.border.lwd", "symbol.border.alpha", "symbol.scale")
	lnames <- c("palette", "breaks", "labels", "size.lim", "size.lim", "sizes.legend", "sizes.legend.labels", "lwd.legend", "lwd.legend.labels")
	nlnames <- c("legend.format")
	lapply(1:n, function(i) {
		g[vnames] <- lapply(g[vnames], function(x) {
			if (length(x)==n) x[i] else x[1]
		})
		g[lnames] <- lapply(g[lnames], function(x) {
			if (is.list(x) && length(x)==n) x[[i]] else x
		})
		g[nlnames] <- lapply(g[nlnames], function(x) {
			if (all(sapply(x, is.list)) && length(x)==n) x[[i]] else x
		})
		g
	})
}
