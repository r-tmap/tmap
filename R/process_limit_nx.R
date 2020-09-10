process_limit_nx <- function(nx) {
	# bound number of facets
	tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)
	tmap.limits <- tmapOptions$limits
	show.warnings <- tmapOptions$show.warnings
	show.messages <- tmapOptions$show.messages
	
	mode <- getOption("tmap.mode")
	if (is.null(tmap.limits) || any(is.na(tmap.limits)) || !setequal(names(tmap.limits), c("facets.plot", "facets.view")))
		if (show.warnings) warning("Incorrect global option \"tmap.limits\". See the documentation of tmap_options for details.")
	else {
		tmap.limits.mode <- paste("facets", mode, sep=".")
		nx_lim <- tmap.limits[tmap.limits.mode]
		if (nx_lim < nx) {
			tmap.limits[tmap.limits.mode] <- nx
			
			if (show.messages) message("The number of facets exceeds the limit of ", nx_lim, ". The limit can be extended to ", nx, " with:\ntmap_options(limits = c(", tmap.limits.mode, " = ", nx , "))")
			nx <- min(nx, nx_lim)
		}
	}
	nx
}
