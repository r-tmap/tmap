limit_nx <- function(nx) {
	# bound number of facets
	tmap.limits <- get(".tmapOptions", envir = .TMAP_CACHE)$limits
	mode <- getOption("tmap.mode")
	if (is.null(tmap.limits) || any(is.na(tmap.limits)) || !setequal(names(tmap.limits), c("facets.plot", "facets.view")))
		warning("Incorrect global option \"tmap.limits\". See the documentation of tmap_options for details.")
	else {
		tmap.limits.mode <- paste("facets", mode, sep=".")
		nx_lim <- tmap.limits[tmap.limits.mode]
		if (nx_lim < nx) {
			tmap.limits[tmap.limits.mode] <- nx
			
			if (get(".tmapOptions", envir = .TMAP_CACHE)$show.messages) message("The number of facets exceeds the limit of ", nx_lim, ". The limit can be extended to ", nx, " with:\ntmap_options(limits = c(", tmap.limits.mode, " = ", nx , "))")
			nx <- min(nx, nx_lim)
		}
	}
	nx
}

get_arrangement <- function(nx, asp_ratio) {
	#           asp ~ nrow      
	#       |-------------- 
	#   1   |
	# ~ncol |       nx
	#       | 
	ncol_init <- sqrt(nx/asp_ratio)
	nrow_init <- nx / ncol_init
	
	# rounding:
	nrow_ceiling <- min(ceiling(nrow_init), nx)
	ncol_ceiling <- min(ceiling(ncol_init), nx)
	
	# find minimal change
	nrow_xtra <- abs(nrow_ceiling - nrow_init) * ncol_init
	ncol_xtra <- abs(ncol_ceiling - ncol_init) * nrow_init
	
	# calculaet the other, and subtract 1 when possible
	if (nrow_xtra < ncol_xtra) {
		nrow <- nrow_ceiling
		ncol <- ceiling(nx / nrow)
		if ((nrow-1) * ncol >= nx) nrow <- nrow - 1
	} else {
		ncol <- ncol_ceiling
		nrow <- ceiling(nx / ncol)
		if ((ncol-1) * nrow >= nx) ncol <- ncol - 1
	}
	c(nrow=nrow, ncol=ncol)
}
