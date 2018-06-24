cat2shape <- function(x, 
					  var,
					  shapes,
					  legend.labels = NULL,
					  shapeNA = NA,
					  legend.NA.text = "Missing",
					  showNA = NA,
					  legend.format=list(align="left"),
					  reverse = FALSE) {
	
	sel <- attr(x, "sel")
	if (is.null(sel)) sel <- rep(TRUE, length(x))
	
	x[!sel] <- NA
	
	
	if (!is.factor(x)) x <- factor(x, levels=sort(unique(x)))
	
	
	# quick&dirty
	nCol <- nlevels(x)
	max_levels <- length(shapes)
	
	named <- !is.null(names(shapes))
	

	if (named) {
		nms <- names(shapes)
		xs <- levels(x)
		
		if (!setequal(xs, nms)) {
			c1 <- setdiff(xs, nms)
			c2 <- setdiff(nms, xs)
			txt <- paste0("Names of shapes argument do not match with the values of the variable \"", var, "\".")
			if (length(c1)>0) txt <- paste0(txt, " Values not specified in shapes argument: \"", paste(c1, collapse="\", \""), "\".")
			if (length(c2)>0) txt <- paste0(txt, " Names in shapes argument for which no values exist: \"", paste(c2, collapse="\", \""), "\".")
			stop(txt, call. = FALSE)
		}
		shapes <- shapes[match(xs, nms)]
	} else {
		if (nCol > max_levels) {
			warning("Number of levels (unique values) is ", nCol, ", which is larger than number of symbol shapes (", max_levels, ").", call. = FALSE)
			mapping <- if (max_levels==1) {
				rep(1, nCol)
			} else as.numeric(cut(seq.int(nCol), breaks=max_levels))
			to <- c(which(mapping[-nCol] - mapping[-1]!=0), nCol)
			from <- c(0, to[-max_levels]) + 1
			
			lvls <- levels(x)
			new_lvls <- paste0(lvls[from], "...", lvls[to])
			
			x <- factor(mapping[as.integer(x)], levels=1:max_levels, labels=new_lvls)
		}
		nCol <- nlevels(x)		
	}
	
	

	
	# in case the number of shapes is more than the number of levels
	shapes <- rep(shapes, length.out=nCol) 
	
	if (is.null(legend.labels)) {
		legend.labels <- levels(x)	
	} else {
		legend.labels <- rep(legend.labels, length.out = nCol)
	}
	
	
	shps <- shapes[as.integer(x)]
	shpsNA <- is.na(shps)
	
	
	if (any(shpsNA)) {
		if (is.na(showNA)) showNA <- any(shpsNA & sel)
		shps[shpsNA] <- shapeNA
	} else {
		if (is.na(showNA)) showNA <- FALSE
	}
	
	legend.values <- legend.labels
	
	if (reverse) {
		legend.labels <- rev(legend.labels)
		shapes <- rev(shapes)
	}
	
	if (showNA) {
		legend.labels <- c(legend.labels, legend.NA.text)
		shapes <- c(shapes, shapeNA)
	}
	attr(legend.labels, "align") <- legend.format$text.align
	list(shps=shps, legend.labels=legend.labels, legend.values=legend.values, shapes=shapes)
}
