legend_prepare <- function(gp, gt, scaleFactor) {
	choroID <- which(sapply(gp, function(x)!is.na(x$varnames$choro.fill[1])))[1]
	bubbleSizeID <- which(sapply(gp, function(x)!is.na(x$varnames$bubble.size[1])))[1]
	bubbleColID <- which(sapply(gp, function(x)!is.na(x$varnames$bubble.col[1])))[1]
	
	
	# create input lists for legend
	choro <- if (!is.na(choroID) && "choro" %in% gt$legend.config) {
		gc <- gp[[choroID]]
		list(choro=list(legend.labels=gc$choro.legend.labels,
						legend.palette=gc$choro.legend.palette,
						legend.is.bubbles=FALSE))
	} else NULL
	hist <- if (!is.na(choroID) && "hist" %in% gt$legend.config) {
		gc <- gp[[choroID]]
		list(hist=list(choro.legend.palette=gc$choro.legend.palette,
					   choro.values=gc$choro.values,
					   choro.breaks=gc$choro.breaks))
	} else NULL
	bubble.col <- if (!is.na(bubbleColID) && "bubble.col" %in% gt$legend.config) {
		gb <- gp[[bubbleColID]]
		list(bubble.col=list(legend.palette=gb$bubble.legend.palette,
							 legend.labels=gb$bubble.legend.labels,
							 legend.is.bubbles=TRUE,
							 bubble.legend.border=gb$bubble.border,
							 bubble.max.size=max(gb$bubble.size, na.rm=TRUE)))
	} else NULL
	bubble.size <- if (!is.na(bubbleSizeID) && "bubble.size" %in% gt$legend.config) {
		gb <- gp[[bubbleSizeID]]
		col <- ifelse(gb$bubble.col.is.numeric, gb$bubble.legend.palette[length(gb$bubble.legend.palette)],
					  ifelse(is.na(gb$bubble.legend.palette), gb$bubble.col[1], gb$bubble.legend.palette[1]))
		
		list(bubble.size=list(bubble.legend.col=col,
							  bubble.legend.border=gb$bubble.border,
							  bubble.legend.sizes=gb$bubble.legend.sizes * scaleFactor, 
							  bubble.legend.size_labels=gb$bubble.legend.size_labels))
	} else NULL
	
	if (is.null(choro) && is.null(hist) && is.null(bubble.col) && is.null(bubble.size) && is.na(gt$title)) {
		return()
	} else {
		return(c(choro, hist, bubble.col, bubble.size))
	}
}