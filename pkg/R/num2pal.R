num2pal <- function(x, n = 5,
					   style = "pretty",
                       breaks = NULL,
					   palette = NULL,
					   auto.palette.mapping = TRUE,
					   contrast = 1,
					   legend.labels = NULL,
					   legend.scientific = FALSE,
					   legend.digits = NA,
					   colorNA = "#FF1414",
					   legend.NA.text = "Missing",
					   alpha=1,
					   text_separator = "to",
					   text_less_than = "Less than",
					   text_or_more = "or more") {
	
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
	
	legend.palette <- get_alpha_col(legend.palette, alpha)
	legend.neutral.col <- get_alpha_col(legend.neutral.col, alpha)
	colorNA <- get_alpha_col(colorNA, alpha)
	
	
	
	cols <- findColours(q, legend.palette)
	anyNA <- any(is.na(cols))
	if (anyNA) {
		cols[is.na(cols)] <- colorNA
		if (!is.na(legend.NA.text)) legend.palette <- c(legend.palette, colorNA)
	}
	# create legend labels
	if (is.null(legend.labels)) {
		#breaks.printed <- sprintf(paste("%.", legend.digits, "f", sep=""), breaks) 
		if (legend.scientific) {
			if (is.na(legend.digits)) {
				breaks.printed <- formatC(breaks, flag="#")
			} else {
				breaks.printed <- formatC(breaks, digits=legend.digits, flag="#")
			}
			legend.labels <- paste("[", breaks.printed[-nbrks], ", ", breaks.printed[-1], ")", sep="")
			legend.labels[length(legend.labels)] <- paste(substr(legend.labels[length(legend.labels)], 
																 1, nchar(legend.labels[length(legend.labels)])-1), "]", sep="")
			
		} else {
			breaks.printed <- fancy_breaks(breaks, dp=legend.digits) 
			breaks.printed[breaks==-Inf] <- ""
			legend.labels <- paste(breaks.printed[-nbrks], breaks.printed[-1], sep = paste0(" ", text_separator, " "))
			if (breaks[1]==-Inf) legend.labels[1] <- paste(text_less_than, breaks.printed[2])
			if (breaks[nbrks]==Inf) legend.labels[nbrks-1] <- paste(breaks.printed[nbrks-1], text_or_more)
		}
		
	} else {
		if (length(legend.labels)!=nbrks-1) warning(paste("number of legend labels should be", nbrks-1))
		legend.labels <- rep(legend.labels, length.out=nbrks-1)
	}
	
	if (anyNA && !is.na(legend.NA.text)) {
		legend.labels <- c(legend.labels, legend.NA.text)
	}
	
	list(cols=cols, legend.labels=legend.labels, legend.palette=legend.palette, breaks=breaks, legend.neutral.col = legend.neutral.col)
}


fancy_breaks <- function(vec, dp=NA) {
	# get correct number of significant figures
	#vec = signif(vec, digits)
	frm <- gsub(" ", "", sprintf("%20.10f", abs(vec)))
	mag <- max(nchar(frm)-11)
	ndec <- max(10 - nchar(frm) + nchar(sub("0+$","",frm)))
	nsig <- 

	if (mag>11 || (mag > 9 && all(vec - floor(vec/1e9)*1e9 < 1))) {
		vec <- vec / 1e9
		ext <- " bln"
	} else if (mag > 8 || (mag > 6 && all(vec - floor(vec/1e6)*1e6 < 1))) {
		vec <- vec / 1e6
		ext <- " mln"
	} else {
		ext <- ""
	}
	
	if (is.na(dp)) dp <- max(min(ndec, 4-mag), 0)
	
	paste(prettyNum(vec, big.mark=",", scientific=FALSE, preserve.width="none", digits=dp), ext, sep="")
}

