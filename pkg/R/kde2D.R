kde2D <- function (x, bandwidth, gridsize = c(51L, 51L), range.x, truncate = TRUE) 
{
	gcounts <- t(x[nrow(x):1,])
	gcounts[is.na(gcounts)] <- 0
	gridsize <- dim(gcounts)
	n <- sum(!(gcounts==0))
	#range.x <- list(c(1, gridsize[1]), c(1, gridsize[2]))
	
	
	if (!missing(bandwidth) && min(bandwidth) <= 0) 
		stop("'bandwidth' must be strictly positive")
# 	n <- nrow(x)
	M <- gridsize
	h <- bandwidth
	tau <- 3.4
	if (length(h) == 1L) 
		h <- c(h, h)
# 	if (missing(range.x)) {
# 		range.x <- list(0, 0)
# 		for (id in (1L:2L)) range.x[[id]] <- c(min(x[, id]) - 
# 											   	1.5 * h[id], max(x[, id]) + 1.5 * h[id])
# 	}
	a <- c(range.x[[1L]][1L], range.x[[2L]][1L])
	b <- c(range.x[[1L]][2L], range.x[[2L]][2L])
	gpoints1 <- seq(a[1L], b[1L], length = M[1L])
	gpoints2 <- seq(a[2L], b[2L], length = M[2L])
#	gcounts <- linbin2D(x, gpoints1, gpoints2)
	L <- numeric(2L)
	kapid <- list(0, 0)
	for (id in 1L:2L) {
		L[id] <- min(floor(tau * h[id] * (M[id] - 1)/(b[id] - 
													  	a[id])), M[id] - 1L)
		lvecid <- 0:L[id]
		facid <- (b[id] - a[id])/(h[id] * (M[id] - 1L))
		z <- matrix(dnorm(lvecid * facid)/h[id])
		tot <- sum(c(z, rev(z[-1L]))) * facid * h[id]
		kapid[[id]] <- z/tot
	}
	kapp <- kapid[[1L]] %*% (t(kapid[[2L]]))/n
	if (min(L) == 0) 
		warning("Binning grid too coarse for current (small) bandwidth: consider increasing 'gridsize'", call. = FALSE)
	P <- 2^(ceiling(log(M + L)/log(2)))
	L1 <- L[1L]
	L2 <- L[2L]
	M1 <- M[1L]
	M2 <- M[2L]
	P1 <- P[1L]
	P2 <- P[2L]
	rp <- matrix(0, P1, P2)
	rp[1L:(L1 + 1), 1L:(L2 + 1)] <- kapp
	if (L1) 
		rp[(P1 - L1 + 1):P1, 1L:(L2 + 1)] <- kapp[(L1 + 1):2, 
												  1L:(L2 + 1)]
	if (L2) 
		rp[, (P2 - L2 + 1):P2] <- rp[, (L2 + 1):2]
	sp <- matrix(0, P1, P2)
	sp[1L:M1, 1L:M2] <- gcounts
	rp <- fft(rp)
	sp <- fft(sp)
	rp <- Re(fft(rp * sp, inverse = TRUE)/(P1 * P2))[1L:M1, 1L:M2]
	rp <- rp * matrix(as.numeric(rp > 0), nrow(rp), ncol(rp))
	list(x1 = gpoints1, x2 = gpoints2, fhat = rp)
}