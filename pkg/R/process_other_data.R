process_symbols_shape_vector <- function(x, sel, g, map_shapes, gt) {
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
			shape.legend.shapes <- shapesLeg$shapes
			shape.neutral <- shape.legend.shapes[1]
		}
		
	} else {
		symbol.shape <- x
		shape.legend.labels <- NA
		shape.legend.shapes <- NA
		shape.neutral <- x[1]
	}
	
	list(symbol.shape=symbol.shape,
		 shape.legend.labels=shape.legend.labels,
		 shape.legend.shapes=shape.legend.shapes,
		 shape.neutral=shape.neutral)
	
}

process_symbols_size_vector <- function(x, g, rescale, gt) {
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
	if (is.null(g$sizes.legend.labels)) {
		symbol.size.legend.labels <- do.call("fancy_breaks", c(list(vec=x_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of symbols in the legend", call. = FALSE)
		symbol.size.legend.labels <- g$sizes.legend.labels
	}
	
	maxX <- ifelse(rescale, max(x, na.rm=TRUE), 1)
	scaling <- ifelse(g$perceptual, 0.5716, 0.5)
	symbol.size <- g$scale*(x/maxX)^scaling
	symbol.max.size <- max(symbol.size, na.rm=TRUE)
	symbol.legend.sizes <- g$scale*(x_legend/maxX)^scaling
	list(symbol.size=symbol.size,
		 symbol.size.legend.labels=symbol.size.legend.labels,
		 symbol.legend.sizes=symbol.legend.sizes,
		 symbol.max.size=symbol.max.size)
}
