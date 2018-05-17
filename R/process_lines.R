check_line_specials <- function(xcol, xlwd, g, gt, gby, xvary, data, nx) {
	if (!xvary["line.lwd"]) {
		if (!all(is.numeric(xlwd))) stop("Line widths are neither numeric nor valid variable name(s)", call. = FALSE)
		for (i in 1:nx) data[[paste("lwd", i, sep="_")]] <- xlwd
		xlwd <- paste("lwd", 1:nx, sep="_")
		gby$free.scales.line.lwd <- FALSE
		split.by <- FALSE
	} else split.by <- TRUE
	
	# check for direct color input
	if (xvary["line.col"]) {
		is.colors <- FALSE
	} else {
		# check for direct color input
		is.colors <- all(valid_colors(xcol))
		if (!is.colors) stop("Invalid line colors", call. = FALSE)
		xcol <- do.call("process_color", c(list(col=col2hex(xcol), alpha=g$alpha), gt$pc))
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- xcol[i]
		xcol <- paste("COLOR", 1:nx, sep="_")
	}
	list(xcol = xcol, xlwd = xlwd, gby = gby, data = data, is.colors = is.colors, split.by = split.by)			
}

assign_legend_line_widths <- function(line.legend.lwds, line.lwd, nx) {
	if (is.list(line.legend.lwds)) {
		vapply(line.legend.lwds, function(x)quantile(x, probs=.75, na.rm=TRUE), numeric(1))
	} else if (is.na(line.legend.lwds[1])) {
		apply(line.lwd, 2, function(bc) quantile(bc, probs=.75, na.rm=TRUE))
	} else {
		rep(quantile(line.legend.lwds, probs=.75, na.rm=TRUE), nx)
	}		
}


process_lines <- function(data, g, gt, gby, z, interactive) {

	# aesthetics
	xs <- list(line.col = g$col, line.lwd = g$lwd)
	process_aes(type = "line", xs, c("xline", "xlinelwd"), "lines", data, g, gt, gby, z, interactive)
}


