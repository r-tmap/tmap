process_line_lwd_vector <- function(x, g, rescale) {
	w_legend <- pretty(x, 7)
	w_legend <- w_legend[w_legend!=0]
	w_legend <- w_legend[-c(length(w_legend)-3,length(w_legend)-1)]
	maxW <- ifelse(rescale, max(x, na.rm=TRUE), 1)
	line.legend.lwds <-  g$lines.scale * (w_legend/maxW)
	line.lwd.legend.labels <- format(w_legend, trim=TRUE)
	line.lwd <- g$lines.scale * (x/maxW)
	list(line.lwd=line.lwd,
		 line.legend.lwds=line.legend.lwds,
		 line.lwd.legend.labels=line.lwd.legend.labels)
}

process_line_col_vector <- function(x, g, gt) {
	line.col.is.numeric <- is.numeric(x)
	if (line.col.is.numeric) {
		palette <- if (is.null(g$palette))  "Blues" else g$palette
		colsLeg <- num2pal(x, g$n, style=g$style, breaks=g$breaks, 
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast, legend.labels=g$labels,
						   legend.digits=gt$legend.digits,
						   legend.NA.text=gt$legend.NA.text)
		line.col <- colsLeg[[1]]
	} else {
		palette <- if (is.null(g$palette))  "Dark2" else g$palette
		#remove unused levels in legend
		colsLeg <- cat2pal(x,
						   palette = palette,
						   colorNA = g$colorNA,
						   legend.NA.text=gt$legend.NA.text,
						   max_levels=g$max.categories)
		
		line.col <- colsLeg[[1]]
		
	}
	line.col.legend.labels <- colsLeg[[2]]
	line.col.legend.palette <- colsLeg[[3]]
	
	list(line.col=line.col,
		 line.col.legend.labels=line.col.legend.labels,
		 line.col.legend.palette=line.col.legend.palette,
		 line.col.is.numeric=line.col.is.numeric)
	
}

process_lines <- function(data, g, gt, gby) {
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]
	
	xcol <- g$lines.col
	xlwd <- g$lines.lwd
	
	if (nlevels(by)>1) {
		xcol <- xcol[1]
		xlwd <- xlwd[1]
	}
	
	nxcol <- length(xcol)
	nxlwd <- length(xlwd)
	
	varycol <- all(xcol %in% shpcols) && !is.null(xcol)
	varylwd <- all(xlwd %in% shpcols) && !is.null(xlwd)
	
	nx <- max(nxcol, nxlwd)
	if (nxcol<nx) xcol <- rep(xcol, length.out=nx)
	if (nxlwd<nx) xlwd <- rep(xlwd, length.out=nx)

	if (!varylwd) {
		if (!all(is.numeric(xlwd))) stop("Line widths are neither numeric nor valid variable names")
		for (i in 1:nx) data[[paste("lwd", i, sep="_")]] <- xlwd[i]
		xlwd <- paste("lwd", 1:nx, sep="_")
		gby$free.scales.line.lwd <- FALSE
	}
	
	if (!varycol) {
		if (!all(valid_colors(xcol))) stop("Invalid line colors")
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- xcol[i]
		xcol <- paste("COLOR", 1:nx, sep="_")
	}
	
	nx <- max(nx, nlevels(by))
	
	dtcol <- process_data(data[, xcol, drop=FALSE], by=by, free.scales=gby$free.scales.line.col)
	dtlwd <- process_data(data[, xlwd, drop=FALSE], by=by, free.scales=gby$free.scales.line.lwd)
	
	if (is.list(dtlwd)) {
		res <- lapply(dtlwd, process_line_lwd_vector, g, rescale=varylwd)
		line.lwd <- sapply(res, function(r)r$line.lwd)
		line.legend.lwds <- lapply(res, function(r)r$line.legend.lwds)
		line.lwd.legend.labels <- lapply(res, function(r)r$line.lwd.legend.labels)
	} else {
		res <- process_line_lwd_vector(dtlwd, g, rescale=varylwd)
		line.lwd <- matrix(res$line.lwd, nrow=npol)
		if (varylwd) {
			line.legend.lwds <- res$line.legend.lwds
			line.lwd.legend.labels <- res$line.lwd.legend.labels
		} else {
			line.legend.lwds <- NA
			line.lwd.legend.labels <- NA
			xlwd <- rep(NA, nx)
		}
	}
	
	if (is.matrix(dtcol)) {
		line.col <- dtcol
		xcol <- rep(NA, nx)
		line.col.legend.labels <- NA
		line.col.legend.palette <- NA
		line.col.is.numeric <- NA
	} else if (is.list(dtcol)) {
		res <- lapply(dtcol, process_line_col_vector, g, gt)
		line.col <- sapply(res, function(r)r$line.col)
		line.col.legend.labels <- lapply(res, function(r)r$line.col.legend.labels)
		line.col.legend.palette <- lapply(res, function(r)r$line.col.legend.palette)
		line.col.is.numeric <- sapply(res, function(r)r$line.col.is.numeric)
	} else {
		res <- process_line_col_vector(dtcol, g, gt)
		line.col <- matrix(res$line.col, nrow=npol)
		line.col.legend.labels <- res$line.col.legend.labels
		line.col.legend.palette <- res$line.col.legend.palette
		line.col.is.numeric <- res$line.col.is.numeric
	}
	
	line.lwd.legend.palette <- if (is.list(line.col.legend.palette)) {
		mapply(function(pal, isnum) {
			if (isnum) pal[length(pal)] else pal[1]
		}, line.col.legend.palette, line.col.is.numeric)
	} else if (is.na(line.col.legend.palette[1])) {
		apply(line.col, 2, function(bc) na.omit(bc)[1])
	} else {
		rep(line.col.legend.palette[1], nx)
	}
	
	line.legend.lwd <- if (is.list(line.legend.lwds)) {
		sapply(line.legend.lwds, median)
	} else if (is.na(line.legend.lwds[1])) {
		apply(line.lwd, 2, function(bc) median(bc, na.rm=TRUE))
	} else {
		rep(median(line.legend.lwds), nx)
	}
	
	list(line.col=line.col,
		 line.lwd=line.lwd,
		 line.lty=g$lines.lty,
		 line.col.legend.labels=line.col.legend.labels,
		 line.col.legend.palette=line.col.legend.palette,
		 line.col.legend.misc=list(line.legend.lwd=line.legend.lwd, 
		 						  line.legend.lty=g$lines.lty),
		 line.lwd.legend.labels=line.lwd.legend.labels,
		 line.lwd.legend.palette=line.lwd.legend.palette,
		 line.lwd.legend.misc=list(legend.lwds=line.legend.lwds),
		 xline=xcol,
		 xlinelwd=xlwd)

}


