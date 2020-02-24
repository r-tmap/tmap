raster_colors <- function(x, use.colortable = FALSE, max.value = 255) {
	
	anyna <- apply(x, MARGIN = 1, function(i) any(is.na(i)))
	
	x[anyna, ] <- max.value
	
	n <- nrow(x)

	# get alpha transparency
	if (ncol(x)==4) {
		a <- ifelse(anyna, 0, x[,4])
		x <- x[,1:3]
	} else {
		if (any(anyna)) {
			a <- ifelse(anyna, 0, max.value)
		} else {
			a <- NULL
		}
	}

	if (!use.colortable) {
		if (is.null(a)) {
			cols <- rgb(x[,1], x[,2], x[,3], maxColorValue = max.value)
		} else {
			cols <- rgb(x[,1], x[,2], x[,3], a, maxColorValue = max.value)
		}
		return(factor(cols))
	}
	

	storage.mode(x) <- "integer"
	v <- x[, 1] * 1e6L + x[, 2] * 1e3L + x[, 3]

	isna <- is.na(v)
	if (!is.null(a)) isna <- isna & (a!=max.value)

	v <- v[!isna]
	u <- unique(v)
	nu <- length(u)
	m <- match(v, u)
	nc <- (min(256, nu))
	ta <- tabulate(m, nbins = nu)
	mo <- order(ta, decreasing = TRUE)
	ids <- mo[1:nc]

	r <- floor(u/1e6L)
	g <- floor((u-r*1e6L)/1e3L)
	b <- (u - r * 1e6L - g * 1e3L)
	rs <- r[ids]
	gs <- g[ids]
	bs <- b[ids]

	RGB <- cbind(r, g, b)
	RGBs <- cbind(rs, gs, bs)


	dists <- apply(RGBs, MARGIN = 1, function(rw) {
		sqrt((rw[1]-RGB[,1])^2 + (rw[2]-RGB[,2])^2 + (rw[3]-RGB[,3])^2)
	})

	ids2 <- apply(dists, MARGIN = 1, which.min)

	m2 <- ids2[m]

	ind <- integer(length=n)


	ind[!isna] <- m2
	ind[isna] <- NA

	cols <- rgb(rs, gs, bs, maxColorValue = 255)

	factor(ind, labels=cols)
}
