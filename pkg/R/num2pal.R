num2pal <- function(x, n = 5,
					   style = "pretty",
                       breaks = NULL,
					   palette = NULL,
					   auto.palette.mapping = TRUE,
					   contrast = 1,
					   legend.labels = NULL,
					   legend.digits = 2,
					   colorNA = "#FF1414",
					   legend.NA.text = "Missing") {
	
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
	mc <- brewer.pal.info[palette, "maxcolors"]
	if (auto.palette.mapping) {
		pal.div <- (brewer.pal.info[palette, "category"]=="div")

		colpal <- colorRampPalette(revPal(brewer.pal(mc, palette)))(101)

		ids <- if (pal.div) {
			map2divscaleID(breaks, contrast=contrast)
		} else {
			map2seqscaleID(breaks, contrast=contrast)
		}
		
		legend.palette <- colpal[ids]
	} else {
		if (nbrks-1 > mc) {
			legend.palette <- colorRampPalette(revPal(brewer.pal(mc, palette)))(nbrks-1)
		} else legend.palette <- revPal(brewer.pal(nbrks-1, palette))
	}
	
	cols <- findColours(q, legend.palette)
	anyNA <- any(is.na(cols))
	if (anyNA) {
		cols[is.na(cols)] <- colorNA
		if (!is.na(legend.NA.text)) legend.palette <- c(legend.palette, colorNA)
	}
	# create legend labels
	if (is.null(legend.labels)) {
		
		#breaks.printed <- sprintf(paste("%.", legend.digits, "f", sep=""), breaks) 
		breaks.printed <- pretty_breaks(breaks, dp=legend.digits) 
		legend.labels <- paste(breaks.printed[-nbrks], breaks.printed[-1], sep=" to ")
		#legend.labels[length(legend.labels)] <- paste(substr(legend.labels[length(legend.labels)], 
		#													 1, nchar(legend.labels[length(legend.labels)])-1), "]", sep="")
	} else {
		if (length(legend.labels)!=nbrks-1) warning(paste("number of legend labels should be", nbrks-1))
		legend.labels <- rep(legend.labels, length.out=nbrks-1)
	}
	
	if (anyNA && !is.na(legend.NA.text)) {
		legend.labels <- c(legend.labels, legend.NA.text)
	}
	
	list(cols=cols, legend.labels=legend.labels, legend.palette=legend.palette, breaks=breaks)
}


pretty_breaks <- function(vec, dp=NULL) {
	# get correct number of significant figures
	#vec = signif(vec, digits)
	frm <- gsub(" ", "", sprintf("%20.10f", abs(vec)))
	mag <- max(nchar(frm)-11)
	ndec <- max(10 - nchar(frm) + nchar(sub("0+$","",frm)))

	if (mag>11) {
		vec <- vec / 1e9
		ext <- "billion"
	} else if (mag > 8) {
		vec <- vec / 1e6
		ext <- "million"
	} else {
		ext <- ""
	}
	
	if (is.null(dp)) dp <- max(min(ndec, 4-mag), 0)
	
	paste(prettyNum(vec, big.mark=",", scientific=FALSE, preserve.width="none", digits=dp), ext)
}

