process_dtsize <- function(dtsize, g, gt, nx, npol, varysize, col.neutral) {
	is.constant <- FALSE
	if (is.list(dtsize)) {
		# multiple variables for size are defined
		gss <- split_g(g, n=nx)
		#if (!all(sapply(dtsize, is.numeric))) stop("size argument of tm_symbols/tm_dots contains a non-numeric variable", call. = FALSE)
		
		# only get title_append from columns
		title_append <- vapply(mapply(check_num_col, dtsize, gss, SIMPLIFY = FALSE), "[[", character(1), "title_append")
		
		res <- mapply(process_symbols_size_vector, dtsize, gss, MoreArgs = list(rescale=varysize, gt=gt, reverse=g$legend.size.reverse), SIMPLIFY = FALSE)
		symbol.size <- sapply(res, function(r)r$symbol.size)
		symbol.size.legend.labels <- lapply(res, function(r)r$symbol.size.legend.labels)
		symbol.size.legend.values <- lapply(res, function(r)r$symbol.size.legend.values)
		symbol.legend.sizes <- lapply(res, function(r)r$symbol.legend.sizes)
		symbol.max.size <- lapply(res, function(r)r$symbol.max.size)
		
		# emptySizeLegend <- sapply(symbol.size.legend.labels, function(ssll) is.na(ssll[1]))
		# symbol.size.legend.show[emptySizeLegend] <- FALSE
	} else {
		if (!is.numeric(dtsize)) stop("size argument of tm_symbols/tm_dots is not a numeric variable", call. = FALSE)
		
		title_append <- check_num_col(dtsize, g)$title_append

		res <- process_symbols_size_vector(dtsize, g, rescale=varysize, gt=gt, reverse=g$legend.size.reverse)
		symbol.size <- matrix(res$symbol.size, nrow=npol)
		if (varysize) {
			symbol.size.legend.labels <- res$symbol.size.legend.labels
			symbol.size.legend.values <- res$symbol.size.legend.values
			symbol.legend.sizes <- res$symbol.legend.sizes
			symbol.max.size <- res$symbol.max.size
			
		} else {
			symbol.size.legend.labels <- NA
			symbol.size.legend.values <- NA
			symbol.legend.sizes <- NA
			symbol.max.size <- res$symbol.max.size
			xsize <- rep(NA, nx)
			symbol.size.legend.title <- rep(NA, nx)
			is.constant <- TRUE
		}
	}
	
	nonemptyFacets <- if (is.constant) NULL else apply(symbol.size, MARGIN = 2, function(v) !all(is.na(v)))
	
	list(is.constant=is.constant,
		 symbol.size=symbol.size,
		 legend.labels=symbol.size.legend.labels,
		 legend.values=symbol.size.legend.values,
		 legend.sizes = symbol.legend.sizes,
		 legend.palette=col.neutral,
		 legend.misc= list(symbol.border.lwd=g$border.lwd, symbol.normal.size=g$legend.max.symbol.size, symbol.max.size = symbol.max.size), # symbol.border.col added later, symbol.max.size needed for col and shape
		 nonemptyFacets = nonemptyFacets,
		 title_append = title_append)	
}


process_symbols_size_vector <- function(x, g, rescale, gt, reverse) {
	#check_aes_args(g)
	
	if (all(is.na(x))) {
		return(list(symbol.size=rep(NA, length(x)),
					symbol.size.legend.labels=NA,
					symbol.size.legend.values=NA,
					symbol.legend.sizes=NA,
					symbol.max.size=g$size.max))
	}
	
	if (!is.na(g$size.lim[1])) {
		x[x<g$size.lim[1]] <- NA
		x[x>g$size.lim[2]] <- g$size.lim[2]
	}
	
	mx <- max(x, na.rm=TRUE)
	xmax <- ifelse(is.na(g$size.max), mx, g$size.max)
	
	if (mx > xmax) {
		s <- sum(x > xmax, na.rm = TRUE)
		message("Note that ", s, " values of the variable \"", g$size, "\" (the highest being ", mx, ") are larger than size.max, which is currently set to ", xmax, ". It is recommended to set size.max to at least ", mx, ". Another option is to set size.lim = c(0, ", xmax, "), which truncates the size of the ", s, " larger symbols. Use the scale argument to increase the size of all symbols.")
	}
	
	if (is.null(g$sizes.legend)) {
		x_legend <- pretty(c(0, xmax), 5)
		x_legend <- x_legend[x_legend!=0]
		nxl <- length(x_legend)
		if (nxl>5) x_legend <- x_legend[-c(nxl-3, nxl-1)]
	} else {
		x_legend <- g$sizes.legend
	}
	symbol.size.legend.values <- x_legend
	
	if (is.null(g$sizes.legend.labels)) {
		symbol.size.legend.labels <- do.call("fancy_breaks", c(list(vec=x_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of symbols in the legend", call. = FALSE)
		symbol.size.legend.labels <- g$sizes.legend.labels
		attr(symbol.size.legend.labels, "align") <- g$legend.format$text.align
	}
	
	maxX <- ifelse(rescale, xmax, 1)
	scaling <- ifelse(g$perceptual, 0.5716, 0.5)
	symbol.size <- g$scale*(x/maxX)^scaling
	symbol.max.size <- max(symbol.size, na.rm=TRUE)
	symbol.legend.sizes <- g$scale*(x_legend/maxX)^scaling
	
	if (reverse) {
		symbol.legend.sizes <- rev(symbol.legend.sizes)
		symbol.size.legend.labels <- rev(symbol.size.legend.labels)
	}
	
	list(symbol.size=symbol.size,
		 symbol.size.legend.labels=symbol.size.legend.labels,
		 symbol.size.legend.values=symbol.size.legend.values,
		 symbol.legend.sizes=symbol.legend.sizes,
		 symbol.max.size=symbol.max.size)
}

