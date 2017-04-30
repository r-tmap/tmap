process_lines <- function(data, g, gt, gby, z, interactive) {
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]


	xcol <- g$col
	xlwd <- g$lwd
	
	# if (interactive) {
	# 	if (length(xcol)>1) warning("Facets are not supported in view mode yet. Only line color aesthetic value \"", xcol[1], "\" will be shown.", call.=FALSE)
	# 	if (length(xlwd)>1) warning("Facets are not supported in view mode yet. Only line width aesthetic value \"", xlwd[1], "\" will be shown.", call.=FALSE)
	# 	xcol <- xcol[1]	
	# 	xlwd <- xlwd[1]
	# } 

	if (length(xcol)==1 && is.na(xcol)[1]) xcol <- gt$aes.colors["lines"]
	if (is.null(g$colorNA)) g$colorNA <- "#00000000"
	if (is.na(g$colorNA)[1]) g$colorNA <- gt$aes.colors["na"]
	if (g$colorNA=="#00000000") g$showNA <- FALSE
	
	if (!is.na(g$alpha) && !is.numeric(g$alpha)) stop("alpha argument in tm_lines is not a numeric", call. = FALSE)
	
	
		
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
		if (!all(is.numeric(xlwd))) stop("Line widths are neither numeric nor valid variable name(s)", call. = FALSE)
		for (i in 1:nx) data[[paste("lwd", i, sep="_")]] <- xlwd[i]
		xlwd <- paste("lwd", 1:nx, sep="_")
		gby$free.scales.line.lwd <- FALSE
		split.by <- FALSE
	} else split.by <- TRUE
	
	# check for direct color input
	is.colors <- all(valid_colors(xcol))
	if (!varycol) {
		if (!is.colors) stop("Invalid line colors", call. = FALSE)
		xcol <- do.call("process_color", c(list(col=col2hex(xcol), alpha=g$alpha), gt$pc))
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- xcol[i]
		xcol <- paste("COLOR", 1:nx, sep="_")
	}
	
	nx <- max(nx, nlevels(by))
	
	# update legend format from tm_layout
	g$legend.format <- process_legend_format(g$legend.format, gt$legend.format, nx)
	g$popup.format <- process_popup_format(g$popup.format, gt$legend.format, g$popup.vars)
	
	dtcol <- process_data(data[, xcol, drop=FALSE], by=by, free.scales=gby$free.scales.line.col, is.colors=is.colors)
	dtlwd <- process_data(data[, xlwd, drop=FALSE], by=by, free.scales=gby$free.scales.line.lwd, is.colors=FALSE, split.by=split.by)
	
	if (nlevels(by)>1) if (is.na(g$showNA)) g$showNA <- attr(dtcol, "anyNA")
	if (is.list(dtlwd)) {
		# multiple variables for lwd are defined
		gsl <- split_g(g, n=nx)
		if (!all(sapply(dtlwd, is.numeric))) stop("lwd argument of tm_lines contains a non-numeric variable", call. = FALSE)
		res <- mapply(process_line_lwd_vector, dtlwd, gsl, MoreArgs = list(rescale=varylwd), SIMPLIFY = FALSE)
		line.lwd <- sapply(res, function(r)r$line.lwd)
		line.legend.lwds <- lapply(res, function(r)r$line.legend.lwds)
		line.lwd.legend.labels <- lapply(res, function(r)r$line.lwd.legend.labels)
		line.lwd.legend.values <- lapply(res, function(r)r$line.lwd.legend.values)
	} else {
		if (!is.numeric(dtlwd)) stop("lwd argument of tm_lines is not a numeric variable", call. = FALSE)
		res <- process_line_lwd_vector(dtlwd, g, rescale=varylwd)
		line.lwd <- matrix(res$line.lwd, nrow=npol)
		if (varylwd) {
			line.legend.lwds <- res$line.legend.lwds
			line.lwd.legend.labels <- res$line.lwd.legend.labels
			line.lwd.legend.values <- res$line.lwd.legend.values
		} else {
			line.legend.lwds <- NA
			line.lwd.legend.labels <- NA
			line.lwd.legend.values <- NA
			xlwd <- rep(NA, nx)
			line.lwd.legend.title <- rep(NA, nx)
			
		}
	}
	
	# selection: which line widths are NA?
	sel <- if (is.list(dtlwd)) {
		lapply(dtlwd, function(i)!is.na(i))
	} else {
		if (is.list(dtcol)) {
			cnts <- vapply(dtcol, length, integer(1))
			cnts2 <- 1:length(dtcol)
			f <- factor(unlist(mapply(rep, cnts2, cnts, SIMPLIFY = FALSE)))
			split(dtlwd, f = f)
		} else !is.na(dtlwd)
	} 
	

	dcr <- process_dtcol(dtcol, sel, g, gt, nx, npol)
	if (dcr$is.constant) xcol <- rep(NA, nx)
	col <- dcr$col
	col.legend.labels <- dcr$legend.labels
	col.legend.values <- dcr$legend.values
	col.legend.palette <- dcr$legend.palette
	col.neutral <- dcr$col.neutral
	breaks <- dcr$breaks
	values <- dcr$values
	

	line.lwd.legend.palette <- col.neutral
		
	
	line.legend.lwd <- if (is.list(line.legend.lwds)) {
		sapply(line.legend.lwds, function(x)quantile(x, probs=.75, na.rm=TRUE))
	} else if (is.na(line.legend.lwds[1])) {
		apply(line.lwd, 2, function(bc) quantile(bc, probs=.75, na.rm=TRUE))
	} else {
		rep(quantile(line.legend.lwds, probs=.75, na.rm=TRUE), nx)
	}
	
	line.col.legend.title <- if (is.ena(g$title.col)[1]) xcol else g$title.col
	line.lwd.legend.title <- if (is.ena(g$title.lwd)[1]) xlwd else g$title.lwd
	line.col.legend.z <- if (is.na(g$legend.col.z)) z else g$legend.col.z
	line.lwd.legend.z <- if (is.na(g$legend.lwd.z)) z+.33 else g$legend.lwd.z
	line.col.legend.hist.z <- if (is.na(g$legend.hist.z)) z+.66 else g$legend.hist.z

	if (g$legend.hist && is.ena(g$legend.hist.title) && line.col.legend.z>line.col.legend.hist.z) {
		# histogram is drawn between title and legend enumeration
		line.col.legend.hist.title <- line.col.legend.title
		line.col.legend.title <- ""
	} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
		line.col.legend.hist.title <- g$legend.hist.title
	} else line.col.legend.hist.title <- ""
	
	if (!g$legend.lwd.show) line.lwd.legend.title <- NA
	if (!g$legend.col.show) line.col.legend.title <- NA
	
	
	list(line.col=col,
		 line.lwd=line.lwd,
		 line.lty=g$lty,
		 line.alpha=g$alpha,
		 line.col.legend.labels=col.legend.labels,
		 line.col.legend.values=col.legend.values,
		 line.col.legend.palette=col.legend.palette,
		 line.col.legend.misc=list(line.legend.lwd=line.legend.lwd, 
		 						  line.legend.lty=g$lty,
		 						  line.legend.alpha=g$alpha),
		 line.lwd.legend.labels=line.lwd.legend.labels,
		 line.lwd.legend.values=line.lwd.legend.values,
		 line.lwd.legend.palette=line.lwd.legend.palette,
		 line.lwd.legend.misc=list(legend.lwds=line.legend.lwds,
		 						  line.legend.lty=g$lty,
		 						  line.legend.alpha=g$alpha),
		 line.col.legend.hist.misc=list(values=values, breaks=breaks),
		 xline=xcol,
		 xlinelwd=xlwd,
		 line.col.legend.show=g$legend.col.show,
		 line.lwd.legend.show=g$legend.lwd.show,
		 line.col.legend.title=line.col.legend.title,
		 line.lwd.legend.title=line.lwd.legend.title,
		 line.col.legend.is.portrait=g$legend.col.is.portrait,
		 line.lwd.legend.is.portrait=g$legend.lwd.is.portrait,
		 line.col.legend.hist=g$legend.hist,
		 line.col.legend.hist.title=line.col.legend.hist.title,
		 line.col.legend.z=line.col.legend.z,
		 line.lwd.legend.z=line.lwd.legend.z,
		 line.col.legend.hist.z=line.col.legend.hist.z,
		 line.id=g$id,
		 line.popup.vars=g$popup.vars,
		 line.popup.format=g$popup.format
	)

}


