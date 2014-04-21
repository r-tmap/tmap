num2pal <- function(x, n = 5,
					   style = "pretty",
                       breaks = NULL,
					   palette = NULL,
					   auto.palette.mapping = TRUE,
					   contrast = 1,
					   legend.labels = NULL,
					   legend.digits = 2,
					   colorNA = "#FF1414") {
	
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
	if (substr(palette, 1, 1)=="-") {
		revPal <- function(p)rev(p)
		palette <- substr(palette, 2, nchar(palette))
	} else revPal <- function(p)p
	
	
	# map palette
	mc <- brewer.pal.info[palette, "maxcolors"]
	if (auto.palette.mapping) {
		is.div <- (brewer.pal.info[palette, "category"]=="div")

		colpal <- colorRampPalette(revPal(brewer.pal(mc, palette)))(101)

		mx <- max(abs(breaks))
		
		# define neutral category
		brk0 <- which(breaks[1:(nbrks-1)] < 0 & breaks[2:nbrks] > 0)
		if (length(brk0)==0) brk0 <- ifelse(breaks[1]>=0, 1, nbrks-1)
		
		if (is.div) {
			# diverging
			ids <- rep(51, nbrks-1)
			if (brk0 < nbrks-1) ids[brk0:(nbrks-1)] <- 51 + seq(0, 50/mx*breaks[nbrks]*contrast, 
												length.out=nbrks-brk0)
			if (brk0 > 1) ids[1:brk0] <- seq(51-(50/mx*-breaks[1]*contrast), 51, 
												length.out=brk0)
		} else ids <- seq(1, 1+100*contrast, length.out=nbrks-1)
		
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
		legend.palette <- c(legend.palette, colorNA)
	}
	# create legend labels
	if (is.null(legend.labels)) {
		breaks.printed <- sprintf(paste("%.", legend.digits, "f", sep=""), breaks) 
		legend.labels <- paste("[", paste(breaks.printed[-nbrks], breaks.printed[-1], sep=", "), ")", sep="")
		legend.labels[length(legend.labels)] <- paste(substr(legend.labels[length(legend.labels)], 
															 1, nchar(legend.labels[length(legend.labels)])-1), "]", sep="")
	} else {
		if (length(legend.labels)!=nbrks-1) warning(paste("number of legend labels should be", nbrks-1))
		legend.labels <- rep(legend.labels, length.out=nbrks-1)
	}
	
	if (anyNA) {
		legend.labels <- c(legend.labels, "Missing")
	}
	
	list(cols=cols, legend.labels=legend.labels, legend.palette=legend.palette, breaks=breaks)
}