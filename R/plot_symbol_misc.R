# Adjust y value of symbols such that not the centroid is in the middle, but (ymin+ymax)/2
symbol_legend_y_correction <- function(x) {
	is_num <- is.numeric(x)
	res <- lapply(x, function(s) {
		if (is.numeric(s)) {
			ifelse(s %in% c(2, 17, 24), -.025,
			ifelse(s %in% c(6, 25), .025, 0))
		} else 0
	})	
	if (is_num) {
		unlist(res, use.names = FALSE)
	} else {
		res
	}
}

get_symbol_gpar <- function(x, fill, col, lwd, separate=FALSE) {
	is_num <- is.numeric(x)
	n <- max(length(x), length(fill), length(col), length(lwd))
	
	x <- rep(x, length.out=n)
	fill <- rep(fill, length.out=n)
	col <- rep(col, length.out=n)
	lwd <- rep(lwd, length.out=n)
	
	res <- lapply(1:n, function(i) {
		if (is.numeric(x[i])) {
			if (x[i] %in% 21:25) {
				list(fill=fill[i],
					 col=col[i],
					 lwd=lwd[i])
			} else {
				list(fill=as.character(NA),
					 col=fill[i],
					 lwd=lwd[i])
			}
		} else {
			list(fill=fill[i],
				 col=col[i],
				 lwd=lwd[i])
		}
	})	
	
	if (separate) {
		lapply(res, function(r){
			do.call(gpar, r)
		})	
	} else {
		fills <- vapply(res, function(r)r$fill, character(1))
		cols <- vapply(res, function(r)r$col, character(1))
		lwds <- vapply(res, function(r)r$lwd, numeric(1))
		gpar(fill=fills, col=cols, lwd=lwds)
	}
}
