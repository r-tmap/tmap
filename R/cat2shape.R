cat2shape <- function(x, 
					  shapes,
					  legend.labels = NULL,
					  shapeNA = NA,
					  legend.NA.text = "Missing",
					  showNA = NA) {
	if (!is.factor(x)) x <- factor(x, levels=sort(unique(x)))
	
	
	# quick&dirty
	nCol <- nlevels(x)
	max_levels <- length(shapes)
	
	if (nCol > max_levels) {
		warning("Number of levels (unique values) larger than number of symbol shapes.")
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
		if (is.na(showNA)) showNA <- TRUE
		shps[shpsNA] <- shapeNA
	} else {
		if (is.na(showNA)) showNA <- FALSE
	}
	
	legend.values <- legend.labels
	
	if (showNA) {
		legend.labels <- c(legend.labels, legend.NA.text)
		shapes <- c(shapes, shapeNA)
	}
	
	list(shps=shps, legend.labels=legend.labels, legend.values=legend.values, shapes=shapes)
}
