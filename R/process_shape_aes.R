process_dtshape <- function(dtshape, g, gt, sel, nx, npol, varyshape, col.neutral) {
	is.constant <- FALSE
	if (is.list(dtshape)) {
		sel2 <- if (is.na(sel[1])) rep(TRUE, nx) else sel
		
		sel2 <- split(sel2, f = rep(1L:nx, each = npol))
		
		# multiple variables for size are defined
		gsp <- split_g(g, n=nx)
		#if (!all(sapply(dtshape, is.numeric))) stop("size argument of tm_symbols/tm_dots contains a non-numeric variable", call. = FALSE)
		res <- mapply(process_symbols_shape_vector, dtshape, sel2, gsp, MoreArgs = list(map_shapes=varyshape, gt=gt, reverse=g$legend.shape.reverse), SIMPLIFY = FALSE)
		symbol.shape <- sapply(res, function(r)r$symbol.shape)
		shape.legend.labels <- lapply(res, function(r)r$shape.legend.labels)
		shape.legend.values <- lapply(res, function(r)r$shape.legend.values)
		shape.legend.shapes <- lapply(res, function(r)r$shape.legend.shapes)
		shape.neutral <- lapply(res, function(r)r$shape.neutral)
		#if (!varyshape) xshape <- rep(NA, nx)
		values <- dtshape
	} else {
		#if (!is.numeric(dtsize)) stop("size argument of tm_symbols/tm_dots is not a numeric variable", call. = FALSE)
		sel2 <- if (is.na(sel[1])) TRUE else sel
		res <- process_symbols_shape_vector(dtshape, sel2, g, map_shapes=varyshape, gt=gt, reverse=g$legend.shape.reverse)
		symbol.shape <- matrix(res$symbol.shape, nrow=npol)
		if (varyshape) {
			shape.legend.labels <- res$shape.legend.labels
			shape.legend.values <- res$shape.legend.values
			shape.legend.shapes <- res$shape.legend.shapes
			shape.neutral <- res$shape.neutral
			values <- split(dtshape, rep(1:nx, each=npol))
		} else {
			is.constant <- TRUE
			shape.legend.labels <- NA
			shape.legend.values <- NA
			shape.legend.shapes <- NA
			xshape <- rep(NA, nx)
			symbol.shape.legend.title <- rep(NA, nx)
			shape.neutral <- symbol.shape[which(!is.na(symbol.shape))[1]]
		}
	}

	nonemptyFacets <- if (is.constant) NULL else if(is.list(values)) vapply(values, function(v) !all(is.na(v)), logical(1)) else rep(TRUE, nx)
	
	list(is.constant=is.constant,
		 symbol.shape=symbol.shape,
		 legend.labels=shape.legend.labels,
		 legend.values=shape.legend.values,
		 legend.shapes=shape.legend.shapes,
		 legend.palette=col.neutral,
		 legend.misc= list(symbol.border.lwd=g$border.lwd, symbol.normal.size=g$legend.max.symbol.size, shape.neutral = shape.neutral), # symbol.border.col added later, shape.neutral needed for col and size
		 nonemptyFacets = nonemptyFacets,
		 title_append = "")	
	
}

process_symbols_shape_vector <- function(x, sel, g, map_shapes, gt, reverse) {
	check_aes_args(g)
	
	if (map_shapes) {
		x[!sel] <- NA
		if (length(na.omit(unique(x)))==1 && g$style!="fixed") g$style <- "cat"
		
		if (is.factor(x) || g$style=="cat") {
			shapesLeg <- cat2shape(x,
								   var = g$shape,
								   shapes=g$shapes,
								   legend.labels=g$labels,
								   shapeNA = g$shapeNA,
								   legend.NA.text = g$shape.textNA,
								   showNA = g$shape.showNA,
								   legend.format=g$legend.format,
								   reverse=reverse)
			symbol.shape <- shapesLeg$shps
			shape.legend.labels <- shapesLeg$legend.labels
			shape.legend.values <- shapesLeg$legend.values
			shape.legend.shapes <- shapesLeg$shapes
			shape.neutral <- shape.legend.shapes[1]
		} else {
			shapesLeg <- num2shape(x, 
								   var = g$shape,
								   n=g$shapes.n, 
								   style=g$shapes.style, 
								   breaks=g$shapes.breaks, 
								   interval.closure=g$shapes.interval.closure,
								   shapes=g$shapes,
								   legend.NA.text = g$shape.textNA,
								   shapeNA=g$shapeNA, 
								   showNA = g$shape.showNA,
								   legend.format=g$legend.format,
								   reverse=reverse)
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
		shape.neutral <- x[which(!is.na(x))[1]]
	}

	list(symbol.shape=symbol.shape,
		 shape.legend.labels=shape.legend.labels,
		 shape.legend.values=shape.legend.values,
		 shape.legend.shapes=shape.legend.shapes,
		 shape.neutral=shape.neutral)
	
}