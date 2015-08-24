split_g <- function(g, n) {
	vnames <- c("alpha", "convert2density", "n", "style", "auto.palette.mapping", "contrast", "max.categories", "colorNA", "textNA", "bubble.border.col", "bubble.border.lwd", "bubble.border.alpha", "bubble.scale")
	lnames <- c("palette", "breaks", "labels", "size.lim", "size.lim", "sizes.legend", "sizes.legend.labels", "lwd.legend", "lwd.legend.labels", "legend.format")
	lapply(1:n, function(i) {
		g[vnames] <- lapply(g[vnames], function(x) {
			if (length(x)==n) x[i] else x[1]
		})
		g[lnames] <- lapply(g[lnames], function(x) {
			if (is.list(x)) x[[i]] else x
		})
		g
	})
}
