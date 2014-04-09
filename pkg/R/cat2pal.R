cat2pal <- function(x, 
					palette = "Set3",
					colorNA = "#FF1414",
					legend.labels = NULL,
					max_levels = 12) {
	if (!is.factor(x)) x <- factor(x, levels=sort(unique(x)))
	
	# quick&dirty
	nCol <- nlevels(x)
	if (nCol > max_levels) {

		mapping <- as.numeric(cut(seq.int(nCol), breaks=max_levels))
		to <- c(which(mapping[-nCol] - mapping[-1]!=0), nCol)
		from <- c(0, to[-max_levels]) + 1

		lvls <- levels(x)
		new_lvls <- paste0(lvls[from], "...", lvls[to])
		
		x <- factor(mapping[as.integer(x)], levels=1:max_levels, labels=new_lvls)
	}
	
	# reverse palette
	if (substr(palette, 1, 1)=="-") {
		revPal <- function(p)rev(p)
		palette <- substr(palette, 2, nchar(palette))
	} else revPal <- function(p)p
	

	legend.palette <- if (palette[1] %in% rownames(brewer.pal.info)) {
		revPal(brewer.pal(name=palette, nlevels(x)))
	} else {
        rep(palette, length.out=nlevels(x))
	}
    
	   
	cols <- legend.palette[as.integer(x)]
	colsNA <- is.na(cols)

	legend.labels <- levels(x)
	if (any(colsNA)) {
		cols[is.na(cols)] <- colorNA
		legend.labels <- c(legend.labels, "Missing")
		legend.palette <- c(legend.palette, colorNA)
	}
	
	list(cols=cols, legend.labels=legend.labels, legend.palette=legend.palette)
}