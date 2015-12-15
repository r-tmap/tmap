process_line_lwd_vector <- function(x, g, rescale) {
	
	if (is.null(g$lwd.legend)) {
		w_legend <- pretty(x, 7)
		w_legend <- w_legend[w_legend!=0]
		w_legend <- w_legend[-c(length(w_legend)-3,length(w_legend)-1)]
	} else {
		w_legend <- g$lwd.legend
	}
	
	
	
	maxW <- ifelse(rescale, max(x, na.rm=TRUE), 1)
	line.legend.lwds <-  g$scale * (w_legend/maxW)
	line.lwd.legend.labels <- format(w_legend, trim=TRUE)

	if (is.null(g$line.lwd.legend.labels)) {
		line.lwd.legend.labels <- do.call("fancy_breaks", c(list(vec=w_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$line.lwd.legend.labels) != length(w_legend)) stop("length of sizes.legend.labels is not equal to the number of bubbles in the legend", call. = FALSE)
		line.lwd.legend.labels <- g$line.lwd.legend.labels
	}
	
	
	
	line.lwd <- g$scale * (x/maxW)
	list(line.lwd=line.lwd,
		 line.legend.lwds=line.legend.lwds,
		 line.lwd.legend.labels=line.lwd.legend.labels)
}

process_line_col_vector <- function(x, g, gt) {
	line.col.is.numeric <- is.numeric(x)
	if (line.col.is.numeric) {
		is.diverging <-  use_diverging_palette(x, g$breaks)
		palette <- if (is.null(g$palette)) {
			gt$aes.palette[[ifelse(is.diverging, "div", "seq")]] 
		} else if (g$palette[1] %in% c("seq", "div", "cat")) {
			gt$aes.palette[[g$palette[1]]]
		} else g$palette
		colsLeg <- num2pal(x, g$n, style=g$style, breaks=g$breaks, 
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast, legend.labels=g$labels,
						   legend.NA.text=g$textNA,
						   process.colors=c(list(alpha=g$alpha), gt$pc),
						   legend.format=g$legend.format)
		line.breaks <- colsLeg[[4]]
	} else {
		palette <- if (is.null(g$palette)) {
			gt$aes.palette[[ifelse(is.ordered(x), "seq", "cat")]] 
		} else if (g$palette[1] %in% c("seq", "div", "cat")) {
			gt$aes.palette[[g$palette[1]]]
		} else g$palette
		#remove unused levels in legend
		colsLeg <- cat2pal(x,
						   palette = palette,
						   contrast = g$contrast,
						   colorNA = g$colorNA,
						   legend.labels=g$labels,
						   legend.NA.text=g$textNA,
						   max_levels=g$max.categories,
						   process.colors=c(list(alpha=g$alpha), gt$pc))
		line.breaks <- NA
	}
	line.col <- colsLeg[[1]]
	line.col.legend.labels <- colsLeg[[2]]
	line.col.legend.palette <- colsLeg[[3]]
	
	list(line.col=line.col,
		 line.col.legend.labels=line.col.legend.labels,
		 line.col.legend.palette=line.col.legend.palette,
		 line.col.is.numeric=line.col.is.numeric,
		 line.breaks=line.breaks)
	
}

process_lines <- function(data, g, gt, gby, z) {
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]

	# update legend format from tm_layout
	to_be_assigned <- setdiff(names(gt$legend.format), names(g$legend.format))
	g$legend.format[to_be_assigned] <- gt$legend.format[to_be_assigned]
	
	xcol <- g$col
	xlwd <- g$lwd
	
	if (is.na(xcol[1])) xcol <- gt$aes.colors["lines"]
	if (is.na(g$colorNA)[1]) g$colorNA <- gt$aes.colors["na"]
	
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
	}
	
	# check for direct color input
	is.colors <- all(valid_colors(xcol))
	if (!varycol) {
		if (!is.colors) stop("Invalid line colors", call. = FALSE)
		xcol <- do.call("process_color", c(list(col=col2hex(xcol), alpha=g$alpha), gt$pc))
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- xcol[i]
		xcol <- paste("COLOR", 1:nx, sep="_")
	}
	
	nx <- max(nx, nlevels(by))
	
	dtcol <- process_data(data[, xcol, drop=FALSE], by=by, free.scales=gby$free.scales.line.col, is.colors=is.colors)
	dtlwd <- process_data(data[, xlwd, drop=FALSE], by=by, free.scales=gby$free.scales.line.lwd, is.colors=FALSE)
	
	if (is.list(dtlwd)) {
		# multiple variables for lwd are defined
		gsl <- split_g(g, n=nx)
		if (!all(sapply(dtlwd, is.numeric))) stop("lwd argument of tm_lines contains a non-numeric variable", call. = FALSE)
		res <- mapply(process_line_lwd_vector, dtlwd, gsl, MoreArgs = list(rescale=varylwd), SIMPLIFY = FALSE)
		line.lwd <- sapply(res, function(r)r$line.lwd)
		line.legend.lwds <- lapply(res, function(r)r$line.legend.lwds)
		line.lwd.legend.labels <- lapply(res, function(r)r$line.lwd.legend.labels)
	} else {
		if (!is.numeric(dtlwd)) stop("lwd argument of tm_lines is not a numeric variable", call. = FALSE)
		res <- process_line_lwd_vector(dtlwd, g, rescale=varylwd)
		line.lwd <- matrix(res$line.lwd, nrow=npol)
		if (varylwd) {
			line.legend.lwds <- res$line.legend.lwds
			line.lwd.legend.labels <- res$line.lwd.legend.labels
		} else {
			line.legend.lwds <- NA
			line.lwd.legend.labels <- NA
			xlwd <- rep(NA, nx)
			line.lwd.legend.title <- rep(NA, nx)
			
		}
	}
	
	
	sel <- if (is.list(dtlwd)) {
		lapply(dtlwd, function(i)!is.na(i))
	} else !is.na(dtlwd)
	
	
	
	dcr <- process_dtcol(dtcol, sel, g, gt, nx, npol)
	if (dcr$is.constant) xcol <- rep(NA, nx)
	col <- dcr$col
	col.legend.labels <- dcr$legend.labels
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
	
	line.col.legend.title <- if (is.na(g$title.col)[1]) xcol else g$title.col
	line.lwd.legend.title <- if (is.na(g$title.lwd)[1]) xlwd else g$title.lwd
	line.col.legend.z <- if (is.na(g$legend.col.z)) z else g$legend.col.z
	line.lwd.legend.z <- if (is.na(g$legend.lwd.z)) z+.33 else g$legend.lwd.z
	line.col.legend.hist.z <- if (is.na(g$legend.hist.z)) z+.66 else g$legend.hist.z

	if (g$legend.hist && is.na(g$legend.hist.title) && line.col.legend.z>line.col.legend.hist.z) {
		# histogram is drawn between title and legend enumeration
		line.col.legend.hist.title <- line.col.legend.title
		line.col.legend.title <- ""
	} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
		line.col.legend.hist.title <- g$legend.hist.title
	} else line.col.legend.hist.title <- ""
	
	list(line.col=col,
		 line.lwd=line.lwd,
		 line.lty=g$lty,
		 line.alpha=g$alpha,
		 line.col.legend.labels=col.legend.labels,
		 line.col.legend.palette=col.legend.palette,
		 line.col.legend.misc=list(line.legend.lwd=line.legend.lwd, 
		 						  line.legend.lty=g$lty,
		 						  line.legend.alpha=g$alpha),
		 line.lwd.legend.labels=line.lwd.legend.labels,
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
		 line.col.legend.hist.z=line.col.legend.hist.z
	)

}


