process_lines <- function(data, g, free.scales.line.col, legend.digits, legend.NA.text, legend.max.categories) {
	x <- g$lines.col
	nx <- length(x)
	if (nx==1 && valid_colors(x)[1]) {
		return(list(line.col=x,
			 line.lwd=g$lines.lwd,
			 line.lty=g$lines.lty,
			 line.legend.labels=NA,
			 line.legend.palette=NA,
			 xline=NA))
	}
	
	shpcols <- names(data)
	palette <- g$palette
	by <- g$lines.by

	if (is.null(palette)) palette <- "Dark2"

	if (!all(x %in% shpcols)) stop("Incorrect lines col argument")
	
	if (g$lines.by) {
		dat <- data[[x[1]]]
		if (!is.factor(dat)) dat <- factor(dat)
		colsLeg <- cat2pal(dat,
						   palette = palette,
						   legend.NA.text=legend.NA.text,
						   max_levels=nlevels(dat))
		cols <- colsLeg[[3]]
		xline <- colsLeg[[2]]
		col <- matrix(NA, ncol=length(xline), nrow=length(xline))
		diag(col) <- cols
		ids <- as.integer(dat)
		ids[is.na(ids)] <- length(xline)
		col <- col[ids, ]
		line.legend.labels <- NA
		line.legend.palette <- NA
		
		
	} else {
		xline <- x
		X <- unlist(data[, x])
		colsLeg <- cat2pal(X,
						   palette = palette,
						   legend.NA.text=legend.NA.text,
						   max_levels=legend.max.categories)
		col <- matrix(unlist(split(colsLeg[[1]], 
								   rep(1:nx, each=length(colsLeg[[1]])/nx))), ncol=nx)
		line.legend.labels <- colsLeg[[2]]
		line.legend.palette <- colsLeg[[3]]
	}

	list(line.col=col,
		 line.lwd=g$lines.lwd,
		 line.lty=g$lines.lty,
		 line.legend.labels=line.legend.labels,
		 line.legend.palette=line.legend.palette,
		 xline=xline)

}


