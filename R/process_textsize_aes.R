process_text_size_vector <- function(x, text, g, rescale, gt, reverse) {
	check_aes_args(g)
	
	if (!is.na(g$size.lim[1])) {
		x[x<g$size.lim[1]] <- NA
		x[x>g$size.lim[2]] <- g$size.lim[2]
	}
	
	if (is.null(g$sizes.legend)) {
		x_legend <- pretty(x, 5)
		x_legend <- x_legend[x_legend!=0]
		nxl <- length(x_legend)
		if (nxl>5) x_legend <- x_legend[-c(nxl-3, nxl-1)]
	} else {
		x_legend <- g$sizes.legend
	}
	
	size.legend.values <- x_legend
	
	if (is.null(g$sizes.legend.labels)) {
		size.legend.labels <- do.call("fancy_breaks", c(list(vec=x_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of texts in the legend", call. = FALSE)
		size.legend.labels <- g$sizes.legend.labels
		attr(size.legend.labels, "align") <- g$legend.format$text.align
	}
	
	root <- ifelse(rescale, g$root, 1)
	
	maxX <- ifelse(rescale, max(x, na.rm=TRUE), 1)
	size <- (x / maxX) ^ (1/root)
	
	max.size <- max(size, na.rm=TRUE)
	legend.sizes <- (x_legend/maxX) ^ (1/root)
	
	
	text_sel <- (size >= g$size.lowerbound)
	text_empty <- is.na(text) | is.na(size)
	
	if (g$print.tiny) {
		size[!text_sel & !text_empty] <- g$size.lowerbound
		text_sel <- !text_empty
	} else {
		text_sel <- text_sel & !text_empty
	}
	
	size <- size * g$scale
	max.size <- max.size * g$scale
	legend.sizes <- legend.sizes * g$scale
	
	if (reverse) {
		size.legend.labels <- rev(size.legend.labels)
		size.legend.values <- rev(size.legend.values)
		legend.sizes <- legend.sizes
	}
	
	list(size=size,
		 text_sel=text_sel,
		 size.legend.labels=size.legend.labels,
		 size.legend.values=size.legend.values,
		 legend.sizes=legend.sizes,
		 max.size=max.size)
}
