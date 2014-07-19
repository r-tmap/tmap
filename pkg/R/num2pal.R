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
			map2divscale(breaks, contrast=contrast)
		} else seq(1, 1+100*contrast, length.out=nbrks-1)
		
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
		breaks.printed <- sprintf(paste("%.", legend.digits, "f", sep=""), breaks) 
		legend.labels <- paste("[", paste(breaks.printed[-nbrks], breaks.printed[-1], sep=", "), ")", sep="")
		legend.labels[length(legend.labels)] <- paste(substr(legend.labels[length(legend.labels)], 
															 1, nchar(legend.labels[length(legend.labels)])-1), "]", sep="")
	} else {
		if (length(legend.labels)!=nbrks-1) warning(paste("number of legend labels should be", nbrks-1))
		legend.labels <- rep(legend.labels, length.out=nbrks-1)
	}
	
	if (anyNA && !is.na(legend.NA.text)) {
		legend.labels <- c(legend.labels, legend.NA.text)
	}
	
	list(cols=cols, legend.labels=legend.labels, legend.palette=legend.palette, breaks=breaks)
}

# breaksList <- list(c(0, 10, 20, 30, 40, 50),
# 				   c(10, 20, 30, 40, 50),
# 				   c(-50, -40, -30, -20, -10, 0),
# 				   c(-50, -40, -30, -20, -10),
# 				   c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50),
# 				   c(-50, -40, -30, -20, -10, -1, 10, 20, 30, 40, 50),
# 				   c(-30, -10, 10, 30),
# 				   c(-1, 10, 20, 30, 40, 50),
# 				   c(-1, 0, 10, 20, 30, 40, 50),
# 				   c(-50, -40, -30, -20, -10, 1))
# lapply(breaksList, FUN=map2divscale)
map2divscale <- function(breaks, n=101, contrast=1) {
	nbrks <- length(breaks)
	mx <- max(abs(breaks))
	
	is.div <- any(breaks<0) && any(breaks>0)
	
	cat0 <- !any(breaks==0)
	
	h <- ((n-1)/2)+1
	
	if (is.div && !cat0) {
		npos <- sum(breaks>0)
		nneg <- sum(breaks<0)
		step <- round((h-1)*contrast/((max(npos, nneg)-.5)*2))
	} else {
		npos <- sum(breaks>=0) - !is.div
		nneg <- sum(breaks<=0) - !is.div
		step <- 0
	}
	
	pid <- h + step
	nid <- h - step
	
	ids <- rep(h, nbrks-1)
	if (npos>0) ids[(nbrks-npos):(nbrks-1)] <- pid + seq(0, (n-pid)/mx*breaks[nbrks]*contrast, 
														 length.out=npos)
	if (nneg>0) ids[1:nneg] <- seq(nid-((nid-1)/mx*-breaks[1]*contrast), nid, 
								   length.out=nneg)
	
	ids
}
