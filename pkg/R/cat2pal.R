cat2pal <- function(x, 
					palette = "Set3",
					contrast = 1, 
					colorNA = "#FF1414",
					legend.labels = NULL,
					max_levels = 40,
					legend.NA.text = "Missing",
					alpha=1) {
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
	if (length(palette)==1 && substr(palette[1], 1, 1)=="-") {
		revPal <- function(p)rev(p)
		palette <- substr(palette, 2, nchar(palette))
	} else revPal <- function(p)p
	

	n <- nlevels(x)
	legend.palette <- if (palette[1] %in% rownames(brewer.pal.info)) {
		brewerpal <- brewer.pal(min(brewer.pal.info[palette, "maxcolors"], max(n, 3)), name=palette)
		if (brewer.pal.info[palette, "category"]=="qual") {
			p <- rep(brewerpal, length.out=nlevels(x))
		} else {
			if (length(contrast)==1) contrast <- c(0, contrast)
			crange <- contrast[2] - contrast[1]
			ext <- nlevels(x)/crange
			from <- floor(contrast[1] * ext)
			to <- from + nlevels(x)
			p <- colorRampPalette(brewerpal)(ext)[from:to]
		}
		revPal(p)
	} else {
        rep(palette, length.out=nlevels(x))
	}
    
	legend.palette <- get_alpha_col(legend.palette, alpha)
	colorNA <- get_alpha_col(colorNA, alpha)
	
	cols <- legend.palette[as.integer(x)]
	colsNA <- is.na(cols)

	if (is.null(legend.labels)) {
		legend.labels <- levels(x)	
	} else {
		legend.labels <- rep(legend.labels, length.out = n)
	}
	if (any(colsNA)) {
		cols[is.na(cols)] <- colorNA
		if (!is.na(legend.NA.text)) {
			legend.labels <- c(legend.labels, legend.NA.text)
			legend.palette <- c(legend.palette, colorNA)
		}
	}
	
	list(cols=cols, legend.labels=legend.labels, legend.palette=legend.palette)
}