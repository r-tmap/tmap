num2pal <- function(x, n = 5,
					   style = "pretty",
                       breaks = NULL,
					   palette = NULL,
					   auto.palette.mapping = TRUE,
					   contrast = 1,
					   legend.labels = NULL,
					   colorNA = "#FF1414",
					   legend.NA.text = "Missing",
					   process.colors=NULL,
					   legend.format=list(scientific=FALSE)) {
	
	if (length(x)==1) stop("Statistical numerical variable only contains one value. Please use a constant value instead.")
	# create intervals and assign colors
	q <- suppressWarnings(if (style=="fixed") {
        classIntervals(x, n, style= style, fixedBreaks=breaks) 
    } else {
        classIntervals(x, n, style= style)
    })
    breaks <- q$brks
	nbrks <- length(breaks)

	#if (breaks[1] > xrealmin) breaks[1] <- xrealmin
	#if (breaks[nbrks] < xrealmax) breaks[nbrks] <- xrealmax
	
	
	# reverse palette
	if (length(palette)==1 && substr(palette[1], 1, 1)=="-") {
		revPal <- function(p)rev(p)
		palette <- substr(palette, 2, nchar(palette))
	} else revPal <- function(p)p
	
	
	# map palette
	if (palette[1] %in% rownames(brewer.pal.info)) {
		mc <- brewer.pal.info[palette, "maxcolors"]
		if (auto.palette.mapping) {
			pal.div <- (brewer.pal.info[palette, "category"]=="div")
			
			colpal <- colorRampPalette(revPal(brewer.pal(mc, palette)))(101)
			
			ids <- if (pal.div) {
				map2divscaleID(breaks, n=101, contrast=contrast)
			} else {
				map2seqscaleID(breaks, n=101, contrast=contrast)
			}
			
			legend.palette <- colpal[ids]
			if (any(ids<51) && any(ids>51)) {
				ids.neutral <- min(ids[ids>=51]-51) + 51
				legend.neutral.col <- colpal[ids.neutral]
			} else {
				legend.neutral.col <- colpal[ids[round(((length(ids)-1)/2)+1)]]
			}
			
		} else {
			if (nbrks-1 > mc) {
				legend.palette <- colorRampPalette(revPal(brewer.pal(mc, palette)))(nbrks-1)
			} else legend.palette <- revPal(brewer.pal(nbrks-1, palette))
			legend.neutral.col <- legend.palette[round(((length(legend.palette)-1)/2)+1)]
		}
	} else {
		legend.palette <- rep(palette, length.out=nbrks-1)
		legend.neutral.col <- legend.palette[1]
	}
	
	legend.palette <- do.call("process_color", c(list(col=legend.palette), process.colors))
	legend.neutral.col <- do.call("process_color", c(list(col=legend.neutral.col), process.colors))
	colorNA <- do.call("process_color", c(list(col=colorNA), process.colors))
	
# 	if (!is.null(process.colors)) {
# 		legend.palette <- process.colors(legend.palette)
# 		legend.neutral.col <- process.colors(legend.neutral.col)
# 		colorNA <- process.colors(colorNA)
# 	}
# 	
# 	legend.palette <- get_alpha_col(legend.palette, alpha)
# 	legend.neutral.col <- get_alpha_col(legend.neutral.col, alpha)
# 	colorNA <- get_alpha_col(colorNA, alpha)
	
	
	ids <- findCols(q)
	cols <- legend.palette[ids]
	anyNA <- any(is.na(cols))
	if (anyNA) {
		cols[is.na(cols)] <- colorNA
		if (!is.na(legend.NA.text)) legend.palette <- c(legend.palette, colorNA)
	}
	# create legend labels
	if (is.null(legend.labels)) {
		legend.labels <- do.call("fancy_breaks", c(list(vec=breaks, intervals=TRUE), legend.format)) 
	} else {
		if (length(legend.labels)!=nbrks-1) warning(paste("number of legend labels should be", nbrks-1))
		legend.labels <- rep(legend.labels, length.out=nbrks-1)
	}
	
	if (anyNA && !is.na(legend.NA.text)) {
		legend.labels <- c(legend.labels, legend.NA.text)
	}
	
	list(cols=cols, legend.labels=legend.labels, legend.palette=legend.palette, breaks=breaks, legend.neutral.col = legend.neutral.col)
}


fancy_breaks <- function(vec, intervals=FALSE, scientific=FALSE, ...) {
	args <- list(...)

	text.separator <- args$text.separator
	text.less.than <- args$text.less.than
	text.or.more <- args$text.or.more
	
	args[c("text.separator", "text.less.than", "text.or.more")] <- NULL
	
	### analyse the numeric vector
	n <- length(vec)
	frm <- gsub(" ", "", sprintf("%20.10f", abs(vec)))
	
	# get width before decimal point
	mag <- max(nchar(frm)-11)
	
	# get number of decimals (which is number of decimals in vec, which is reduced when mag is large)
	ndec <- max(10 - nchar(frm) + nchar(sub("0+$","",frm)))
	if (is.na(args$digits)) args$digits <- max(min(ndec, 4-mag), 0)
	
	
	if (!scientific) {
		if (mag>11 || (mag > 9 && all(vec - floor(vec/1e9)*1e9 < 1))) {
			vec <- vec / 1e9
			ext <- " bln"
		} else if (mag > 8 || (mag > 6 && all(vec - floor(vec/1e6)*1e6 < 1))) {
			vec <- vec / 1e6
			ext <- " mln"
		} else {
			ext <- ""
		}
		
		# set default values
		if (!("big.mark" %in% names(args))) args$big.mark <- ","
		if (!("format" %in% names(args))) args$format <- "f"
		if (!("preserve.width" %in% names(args))) args$preserve.width <- "none"
		
		x <- paste(do.call("formatC", c(list(x=vec), args)), ext, sep="")

		if (intervals) {
			x[vec==-Inf] <- ""
			lbls <- paste(x[-n], x[-1], sep = paste0(" ", text.separator, " "))
			if (vec[1]==-Inf) lbls[1] <- paste(text.less.than, x[2])
			if (vec[n]==Inf) lbls[n-1] <- paste(x[n-1], text.or.more)
		}
		
	} else {
		if (!("format" %in% names(args))) args$format <- "g"
		
		x <- do.call("formatC", c(list(x=vec), args))
		if (intervals) {
			lbls <- paste("[", x[-n], ", ", x[-1], ")", sep="")
			lbls[n-1] <- paste(substr(lbls[n-1], 1, nchar(lbls[n-1])-1), "]", sep="")
		}
	}
	
	if (intervals) lbls else x
}

