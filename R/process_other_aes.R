process_symbols_shape_vector <- function(x, sel, g, map_shapes, gt) {
	check_aes_args(g)
	
	if (map_shapes) {
		x[!sel] <- NA
		if (length(na.omit(unique(x)))==1 && g$style!="fixed") g$style <- "cat"
		
		if (is.factor(x) || g$style=="cat") {
			shapesLeg <- cat2shape(x,
								   shapes=g$shapes,
								   legend.labels=g$labels,
								   shapeNA = g$shapeNA,
								   legend.NA.text = g$shape.textNA,
								   showNA = g$showNA)
			symbol.shape <- shapesLeg$shps
			shape.legend.labels <- shapesLeg$legend.labels
			shape.legend.values <- shapesLeg$legend.values
			shape.legend.shapes <- shapesLeg$shapes
			shape.neutral <- shape.legend.shapes[1]
		} else {
			

			shapesLeg <- num2shape(x, 
								   n=g$shapes.n, 
								   style=g$shapes.style, 
								   breaks=g$shapes.breaks, 
								   interval.closure=g$shapes.interval.closure,
								   shapes=g$shapes,
								   legend.NA.text = g$shape.textNA,
								   shapeNA=g$shapeNA, 
								   showNA = g$showNA,
								   legend.format=g$legend.format)
			symbol.shape <- shapesLeg$shps
			shape.legend.labels <- shapesLeg$legend.labels
			shape.legend.values <- shapesLeg$legend.values
			shape.legend.shapes <- shapesLeg$shapes
			shape.neutral <- shape.legend.shapes[1]
		}
		
	} else {
		symbol.shape <- x
		shape.legend.labels <- NA
		shape.legend.values <- NA
		shape.legend.shapes <- NA
		shape.neutral <- x[1]
	}
	
	list(symbol.shape=symbol.shape,
		 shape.legend.labels=shape.legend.labels,
		 shape.legend.values=shape.legend.values,
		 shape.legend.shapes=shape.legend.shapes,
		 shape.neutral=shape.neutral)
	
}

process_symbols_size_vector <- function(x, g, rescale, gt) {
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
	symbol.size.legend.values <- x_legend
	
	if (is.null(g$sizes.legend.labels)) {
		symbol.size.legend.labels <- do.call("fancy_breaks", c(list(vec=x_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of symbols in the legend", call. = FALSE)
		symbol.size.legend.labels <- g$sizes.legend.labels
	}
	
	maxX <- ifelse(rescale, ifelse(is.na(g$size.max), max(x, na.rm=TRUE), g$size.max), 1)
	scaling <- ifelse(g$perceptual, 0.5716, 0.5)
	symbol.size <- g$scale*(x/maxX)^scaling
	symbol.max.size <- max(symbol.size, na.rm=TRUE)
	symbol.legend.sizes <- g$scale*(x_legend/maxX)^scaling
	list(symbol.size=symbol.size,
		 symbol.size.legend.labels=symbol.size.legend.labels,
		 symbol.size.legend.values=symbol.size.legend.values,
		 symbol.legend.sizes=symbol.legend.sizes,
		 symbol.max.size=symbol.max.size)
}

process_text_size_vector <- function(x, text, g, rescale, gt) {
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
	
	list(size=size,
		 text_sel=text_sel,
		 size.legend.labels=size.legend.labels,
		 size.legend.values=size.legend.values,
		 legend.sizes=legend.sizes,
		 max.size=max.size)
}

process_line_lwd_vector <- function(x, g, rescale) {
	check_aes_args(g)

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
	
	if (is.null(g$line.lwd.legend.labels)) {
		line.lwd.legend.labels <- do.call("fancy_breaks", c(list(vec=w_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$line.lwd.legend.labels) != length(w_legend)) stop("length of sizes.legend.labels is not equal to the number of lines in the legend", call. = FALSE)
		line.lwd.legend.labels <- g$line.lwd.legend.labels
	}
	
	
	
	line.lwd <- g$scale * (x/maxW)
	list(line.lwd=line.lwd,
		 line.legend.lwds=line.legend.lwds,
		 line.lwd.legend.labels=line.lwd.legend.labels,
		 line.lwd.legend.values=line.lwd.legend.values)
}


