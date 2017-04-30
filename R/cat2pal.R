cat2pal <- function(x, 
					palette = "Set3",
					auto.palette.mapping = TRUE,
					contrast = 1, 
					colorNA = "#FF1414",
					legend.labels = NULL,
					max_levels = 40,
					legend.NA.text = "Missing",
					showNA = NA,
					process.colors) {
  if (!is.factor(x)) x <- factor(x, levels=sort(unique(x)))
	
	
	# quick&dirty
	nCol <- nlevels(x)
	if (nCol > max_levels && !auto.palette.mapping) {

		mapping <- as.numeric(cut(seq.int(nCol), breaks=max_levels))
		to <- c(which(mapping[-nCol] - mapping[-1]!=0), nCol)
		from <- c(0, to[-max_levels]) + 1

		lvls <- levels(x)
		new_lvls <- paste0(lvls[from], "...", lvls[to])
		
		x <- factor(mapping[as.integer(x)], levels=1:max_levels, labels=new_lvls)
	}
	nCol <- nlevels(x)
	
	# reverse palette
	if (length(palette)==1 && substr(palette[1], 1, 1)=="-") {
		revPal <- function(p)rev(p)
		palette <- substr(palette, 2, nchar(palette))
	} else revPal <- function(p)p
	

	legend.palette <- if (palette[1] %in% rownames(brewer.pal.info)) {
		revPal(suppressWarnings(get_brewer_pal(palette, nCol, contrast, stretch = auto.palette.mapping, plot = FALSE)))
	} else {
		if (auto.palette.mapping && (length(palette) < nCol)) {
			colorRampPalette(palette)(nCol)	
		} else rep(palette, length.out=nCol)
	}
    
# 	if (!is.null(process.colors)) {
# 		legend.palette <- process.colors(legend.palette)
# 		colorNA <- process.colors(colorNA)
# 	}
	
	legend.palette <- do.call("process_color", c(list(col=legend.palette), process.colors))
	colorNA <- do.call("process_color", c(list(col=colorNA), process.colors))

	cols <- legend.palette[as.integer(x)]
	colsNA <- is.na(cols)

	if (is.null(legend.labels)) {
		legend.labels <- levels(x)	
	} else {
		legend.labels <- rep(legend.labels, length.out = nCol)
	}
	if (any(colsNA)) {
		if (is.na(showNA)) showNA <- TRUE
		cols[colsNA] <- colorNA
	} else {
		if (is.na(showNA)) showNA <- FALSE
	}

	legend.values <- legend.labels
	
	if (showNA) {
		legend.labels <- c(legend.labels, legend.NA.text)
		legend.palette <- c(legend.palette, colorNA)
	}
	
	list(cols=cols, legend.labels=legend.labels, legend.values=legend.values, legend.palette=legend.palette)
}