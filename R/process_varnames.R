process_varnames <- function(gp, nx) {
	
	varnames <- c("by", "fill", "symbol.size", "symbol.col", "symbol.shape", "line.col", "line.lwd", "raster")
	
	varnamesList <- lapply(gp, function(x) x$varnames)
	
	vars <- lapply(varnames, function(v) lapply(varnamesList, function(x) x[[v]]))
	names(vars) <- varnames
	
	vars2 <- lapply(vars, function(v) {
		ids <- which(vapply(v, function(x)!is.na(x), logical(1)))
		
		lapply(ids, function(i) rep(v[[i]], length.out=nx))
		
		#id <- which(sapply(v, function(x)!is.na(x[1])))
		#if (length(id)) rep(v[[id[1]]], length.out=nx) else NA
	})
	
	names(vars2) <- varnames
	vars2
}


process_legend_titles <- function(gp, nx, varnames) {
	
}