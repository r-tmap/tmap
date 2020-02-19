process_dtlwd <- function(dtlwd, g, gt, nx, npol, varylwd, col.neutral) {
	reverse <- g$legend.lwd.reverse
	is.constant <- FALSE
	if (is.list(dtlwd)) {
		# multiple variables for lwd are defined
		gsl <- split_g(g, n=nx)
		#if (!all(sapply(dtlwd, is.numeric))) stop("lwd argument of tm_lines contains a non-numeric variable", call. = FALSE)
		# only get title_append from columns
		title_append <- vapply(mapply(check_num_col, dtlwd, gsl, SIMPLIFY = FALSE), "[[", character(1), "title_append")
		
		
		res <- mapply(process_line_lwd_vector, dtlwd, gsl, MoreArgs = list(rescale=varylwd, reverse=reverse), SIMPLIFY = FALSE)
		line.lwd <- sapply(res, function(r)r$line.lwd)
		line.legend.lwds <- lapply(res, function(r)r$line.legend.lwds)
		line.lwd.legend.labels <- lapply(res, function(r)r$line.lwd.legend.labels)
		line.lwd.legend.values <- lapply(res, function(r)r$line.lwd.legend.values)
	} else {
		if (!is.numeric(dtlwd)) stop("lwd argument of tm_lines is not a numeric variable", call. = FALSE)
		
		title_append <- check_num_col(dtlwd, g)$title_append
		
		
		res <- process_line_lwd_vector(dtlwd, g, rescale=varylwd, reverse=reverse)
		line.lwd <- matrix(res$line.lwd, nrow=npol)
		if (varylwd) {
			line.legend.lwds <- res$line.legend.lwds
			line.lwd.legend.labels <- res$line.lwd.legend.labels
			line.lwd.legend.values <- res$line.lwd.legend.values
		} else {
			line.legend.lwds <- NA
			line.lwd.legend.labels <- NA
			line.lwd.legend.values <- NA
			is.constant <- TRUE
		}
	}
	lwd.nonemptyFacets <- if (is.constant) NULL else apply(line.lwd, MARGIN = 2, function(v) !all(is.na(v)))
	
	# 
	# list(is.constant=is.constant,
	# 	 col=col,
	# 	 legend.labels=legend.labels,
	# 	 legend.values=legend.values,
	# 	 legend.palette=legend.palette,
	# 	 col.neutral=col.neutral,
	# 	 legend.misc = legend.misc,
	# 	 legend.hist.misc=list(values=values, breaks=breaks, densities=g$convert2density),
	# 	 nonemptyFacets=nonemptyFacets,
	# 	 title_append=title_append)
	
	list(is.constant=is.constant,
		 lwd=line.lwd,
		 legend.labels=line.lwd.legend.labels,
		 legend.values=line.lwd.legend.values,
		 legend.palette=col.neutral,
		 legend.misc=list(legend.lwds=line.legend.lwds,
		 				 line.legend.lty=g$lty,
		 				 line.legend.alpha=g$alpha),
		 nonemptyFacets = lwd.nonemptyFacets,
		 title_append = title_append)
}



process_line_lwd_vector <- function(x, g, rescale, reverse) {
	check_aes_args(g)
	
	
	if (all(is.na(x))) {
		return(list(
			line.lwd=rep(NA, length.out=length(x)),
			line.legend.lwds=NA,
			line.lwd.legend.labels=NA,
			line.lwd.legend.values=NA))
	}
	
	if (!is.numeric(x)) stop("lwd argument of tm_lines contains a non-numeric variable", call. = FALSE)
	
	if (is.null(g$lwd.legend)) {
		w_legend <- pretty(x, 7)
		w_legend <- w_legend[w_legend!=0]
		w_legend <- w_legend[-c(length(w_legend)-3,length(w_legend)-1)]
	} else {
		w_legend <- g$lwd.legend
	}
	
	
	
	maxW <- ifelse(rescale, max(x, na.rm=TRUE), 1)
	line.legend.lwds <-  g$scale * (w_legend/maxW)
	line.lwd.legend.values <- w_legend
	line.lwd.legend.labels <- format(w_legend, trim=TRUE)
	
	if (is.null(g$lwd.legend.labels)) {
		line.lwd.legend.labels <- do.call("fancy_breaks", c(list(vec=w_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$lwd.legend.labels) != length(w_legend)) stop("length of sizes.legend.labels is not equal to the number of lines in the legend", call. = FALSE)
		line.lwd.legend.labels <- g$lwd.legend.labels
	}
	
	line.lwd <- g$scale * (x/maxW)
	if (reverse) {
		line.legend.lwds <- rev(line.legend.lwds)
		line.lwd.legend.labels <- rev(line.lwd.legend.labels)
	}
	attr(line.lwd.legend.labels, "align") <- g$legend.format$text.align
	
	list(line.lwd=line.lwd,
		 line.legend.lwds=line.legend.lwds,
		 line.lwd.legend.labels=line.lwd.legend.labels,
		 line.lwd.legend.values=line.lwd.legend.values)
}
