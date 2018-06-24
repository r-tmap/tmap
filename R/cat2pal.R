cat2pal <- function(x, 
					var,
					palette = "Set3",
					stretch.palette = TRUE,
					#auto.palette.mapping = TRUE,
					contrast = 1, 
					colorNA = "#FF1414",
					colorNULL = "#FFFFFF",
					legend.labels = NULL,
					max_levels = 40,
					legend.NA.text = "Missing",
					showNA = NA,
					process.colors,
					legend.format=list(align="left"),
					reverse=FALSE) {
	
	sel <- attr(x, "sel")
	if (is.null(sel)) sel <- rep(TRUE, length(x))
	
	x[!sel] <- NA
	
	if (!is.factor(x)) {
		su <- sort(unique(x))
		if (is.numeric(su) && length(su) > max_levels) stop("Number of unique values of the variable \"", var, "\" is ", length(su), ", which is more than max.categories (which is ", max_levels, "), so style = \"cat\" cannot be used. Please use numeric intervals instead, e.g. with style =  \"pretty\".")
		x <- factor(x, levels=su)
		if (is.numeric(su)) levels(x) <- do.call("fancy_breaks", c(list(vec=su, intervals=FALSE), legend.format)) 	
	}
	
	
	# quick&dirty
	nCol <- nlevels(x)
	if (nCol > max_levels) {
		warning("Number of levels of the variable \"", var ,"\" is ", nCol, ", which is larger than max.categories (which is ", max_levels, "), so levels are combined. Set tmap_options(max.categories = ", nCol, ") in the layer function to show all levels.", call. = FALSE)
	
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
	
	
	legend.palette <- if (palette[1] %in% rownames(tmap.pal.info)) {
		if (tmap.pal.info[palette[1], "origin"] == "brewer") {
			revPal(suppressWarnings(get_brewer_pal(palette[1], nCol, contrast, stretch = stretch.palette, plot = FALSE)))			
		} else {
			# viridis palette
			revPal(viridis(nCol, option = palette[1]))
		}
	} else {
		if (stretch.palette && (length(palette) < nCol)) {
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
		if (is.na(showNA)) showNA <- any(colsNA & sel)
		cols[colsNA] <- colorNA
	} else {
		if (is.na(showNA)) showNA <- FALSE
	}
	cols[!sel] <- colorNULL
	
	legend.values <- legend.labels
	
	
	if (reverse) {
		legend.labels <- rev(legend.labels)
		legend.palette <- rev(legend.palette)
	}
	
	if (showNA) {
		legend.labels <- c(legend.labels, legend.NA.text)
		legend.palette <- c(legend.palette, colorNA)
	}
	attr(legend.labels, "align") <- legend.format$text.align
	
	list(cols=cols, legend.labels=legend.labels, legend.values=legend.values, legend.palette=legend.palette)
}