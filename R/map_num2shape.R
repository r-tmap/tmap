num2shape <- function(x, 
					  var,
					  n = 5,
					  style = "pretty",
					  style.args = list(),
					  breaks = NULL,
					interval.closure="left",
					shapes = NULL,
					legend.labels = NULL,
					shapeNA = NA,
					legend.NA.text = "Missing",
					showNA=NA,
					legend.format=list(scientific=FALSE),
					reverse=FALSE) {
	breaks.specified <- !is.null(breaks)
	q <- num2breaks(x=x, n=n, style=style, breaks=breaks, interval.closure=interval.closure, var=var, args = style.args)

	breaks <- q$brks
	nbrks <- length(breaks)
	

	show.warnings = get("tmapOptions", envir = .TMAP_CACHE)$show.warnings
	
	if (length(shapes) < (nbrks-1) && show.warnings) {
		warning("Not enough symbol shapes available. Shapes will be re-used.", call.=FALSE)
	}
	shapes <- rep(shapes, length.out=nbrks-1)
	
	int.closure <- attr(q, "intervalClosure")
	
	ids <- findCols(q)
	shps <- shapes[ids]
	anyNA <- any(is.na(shps))
	if (anyNA) {
		if (is.na(showNA)) showNA <- TRUE
		shps[is.na(shps)] <- shapeNA
	} else {
		if (is.na(showNA)) showNA <- FALSE
	}
	
	if (reverse) {
		legend.labels <- rev(legend.labels)
		shapes <- rev(shapes)
	}
	
	legend.values <- breaks
	
	# create legend labels for discrete cases
	if (is.null(legend.labels)) {
		legend.labels <- do.call("fancy_breaks", c(list(vec=breaks, intervals=TRUE, interval.closure=int.closure), legend.format)) 
	} else {
		if (length(legend.labels)!=nbrks-1 && show.warnings) warning("number of legend labels should be ", nbrks-1, call. = FALSE)
		legend.labels <- rep(legend.labels, length.out=nbrks-1)
		attr(legend.labels, "align") < legend.format$align
	}
	
	if (showNA) {
		legend.labels.align <- attr(legend.labels, "align")
		legend.labels <- c(legend.labels, legend.NA.text)
		attr(legend.labels, "align") <- legend.labels.align
		shapes <- c(shapes, shapeNA)
	}
	

	list(shps=shps, legend.labels=legend.labels, legend.values=legend.values, shapes=shapes)
}

