num2breaks <- function(x, n, style, breaks, approx=FALSE) {
	# create intervals and assign colors
	if (style=="fixed") {
		q <- list(var=x,
				  brks=breaks)
		attr(q, "style") <- "fixed"
		attr(q, "nobs") <- sum(!is.na(x))
		attr(q, "intervalClosure") <- "left"
		class(q) <- "classIntervals"
	} else {
		if (length(x)==1) stop("Statistical numerical variable only contains one value. Please use a constant value instead, or specify breaks", call. = FALSE)
		q <- suppressWarnings(classIntervals(x, n, style= style))
	}
	
	if (approx && style != "fixed") {
	  if (n >= length(unique(x)) && style=="equal") {
	    # to prevent classIntervals to set style to "unique"
      q <- list(var=x, brks=seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=n))
      attr(q, "intervalClosure") <- "left"
      class(q) <- "classIntervals"
	  } else {
	    brks <- q$brks

	    # to prevent ugly rounded breaks such as -.5, .5, ..., 100.5 for n=101
	    qm1 <- suppressWarnings(classIntervals(x, n-1, style= style))
	    brksm1 <- qm1$brks
	    qp1 <- suppressWarnings(classIntervals(x, n+1, style= style))
	    brksp1 <- qp1$brks
	    if (min(brksm1) > min(brks) && max(brksm1) < max(brks)) {
	      q <- qm1
	    } else if (min(brksp1) > min(brks) && max(brksp1) < max(brks)) {
	      q <- qp1
	    }
	  }
	}
	q
}

num2pal <- function(x, n = 5,
					   style = "pretty",
                       breaks = NULL,
					   palette = NULL,
					   auto.palette.mapping = TRUE,
					   contrast = 1,
					   legend.labels = NULL,
					   colorNA = "#FF1414",
					   legend.NA.text = "Missing",
					   showNA=NA,
					   process.colors=NULL,
					   legend.format=list(scientific=FALSE)) {
	breaks.specified <- !is.null(breaks)
	is.cont <- (style=="cont" || style=="order")
	if (is.cont) {
		style <- ifelse(style=="order", "quantile", "equal")
		if (is.null(legend.labels)) {
			ncont <- n
		} else {
			ncont <- length(legend.labels)
		}
		q <- num2breaks(x=x, n=101, style=style, breaks=breaks, approx=TRUE)
		n <- length(q$brks) - 1
	} else {
		q <- num2breaks(x=x, n=n, style=style, breaks=breaks)
	}

	breaks <- q$brks
	nbrks <- length(breaks)

	
	# reverse palette
	if (length(palette)==1 && substr(palette[1], 1, 1)=="-") {
		revPal <- function(p)rev(p)
		palette <- substr(palette, 2, nchar(palette))
	} else revPal <- function(p)p
	
	
	# map palette
	is.brewer <- palette[1] %in% rownames(brewer.pal.info)
	
	if (is.brewer) {
		mc <- brewer.pal.info[palette, "maxcolors"]
		pal.div <- (brewer.pal.info[palette, "category"]=="div")
	} else {
		palette.type <- palette_type(palette)
		
		if (auto.palette.mapping && palette.type=="cat") {
			warning("could not determine whether palette is sequential or diverging. auto.palette.mapping will be set to FALSE.", call. = FALSE)
			auto.palette.mapping <- FALSE
		}
		pal.div <- palette.type=="div"
			
		
		colpal_light <- get_light(palette[c(1, length(palette)/2, length(palette))])
		# figure out whether palette is diverging
		pal.div <- ((colpal_light[2]>colpal_light[1] && colpal_light[2]>colpal_light[3]) || (colpal_light[2]<colpal_light[1] && colpal_light[2]<colpal_light[3]))
	}

	if (auto.palette.mapping) {
		if (is.brewer) {
			colpal <- colorRampPalette(revPal(brewer.pal(mc, palette)), space="rgb")(101)
		} else {
			colpal <- colorRampPalette(revPal(palette), space="rgb")(101)
		}
		
		ids <- if (pal.div) {
			if (is.na(contrast[1])) contrast <- if (is.brewer) default_contrast_div(n) else c(0, 1)
			map2divscaleID(breaks, n=101, contrast=contrast)
		} else {
			if (is.na(contrast[1])) contrast <- if (is.brewer) default_contrast_seq(n) else c(0, 1)
			map2seqscaleID(breaks, n=101, contrast=contrast, breaks.specified=breaks.specified)
		}
		
		legend.palette <- colpal[ids]
		if (any(ids<51) && any(ids>51)) {
			ids.neutral <- min(ids[ids>=51]-51) + 51
			legend.neutral.col <- colpal[ids.neutral]
		} else {
			legend.neutral.col <- colpal[ids[round(((length(ids)-1)/2)+1)]]
		}
		
	} else {
		if (is.brewer) {
			if (nbrks-1 > mc) {
				legend.palette <- colorRampPalette(revPal(brewer.pal(mc, palette)), space="rgb")(nbrks-1)
			} else legend.palette <- revPal(brewer.pal(nbrks-1, palette))
		} else {
			legend.palette <- colorRampPalette(revPal(palette), space="rgb")(nbrks-1) #rep(palette, length.out=nbrks-1)
		}
		neutralID <- if (pal.div) round(((length(legend.palette)-1)/2)+1) else 1
		legend.neutral.col <- legend.palette[neutralID]
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
	breaks.palette <- legend.palette
	if (anyNA) {
		if (is.na(showNA)) showNA <- TRUE
		cols[is.na(cols)] <- colorNA
	} else {
		if (is.na(showNA)) showNA <- FALSE
	}

	if (showNA && !is.cont) legend.palette <- c(legend.palette, colorNA)
	
	
	if (is.cont) {
		# recreate legend palette for continuous cases
		if (style=="quantile") {
			id <- seq(1, n+1, length.out=ncont)
			b <- breaks[id]
			nbrks_cont <- length(b)
		} else {
			b <- pretty(breaks, n=ncont)
			b <- b[b>=breaks[1] & b<=breaks[length(breaks)]]
			nbrks_cont <- length(b)
			id <- as.integer(cut(b, breaks=breaks))
		}

		id_step <- id[2] - id[1]
		id_lst <- lapply(id, function(i){
			res <- round(seq(i-floor(id_step/2), i+ceiling(id_step/2), length.out=11))[1:10]
			res[res<1 | res>101] <- NA
			res
		})
		legend.palette <- lapply(id_lst, function(i) legend.palette[i])
		if (showNA) legend.palette <- c(legend.palette, colorNA)
		
		# temporarily stack gradient colors
		legend.palette <- sapply(legend.palette, paste, collapse="-")
		
		# create legend labels for continuous cases
		if (is.null(legend.labels)) {
			legend.labels <- do.call("fancy_breaks", c(list(vec=b, intervals=FALSE), legend.format)) 	
		} else {
			legend.labels <- rep(legend.labels, length.out=nbrks_cont)
		}
		if (showNA) {
			legend.labels <- c(legend.labels, legend.NA.text)
		}		
		attr(legend.palette, "style") <- style
	} else {
		# create legend labels for discrete cases
		if (is.null(legend.labels)) {
			legend.labels <- do.call("fancy_breaks", c(list(vec=breaks, intervals=TRUE), legend.format)) 
		} else {
			if (length(legend.labels)!=nbrks-1) warning("number of legend labels should be ", nbrks-1, call. = FALSE)
			legend.labels <- rep(legend.labels, length.out=nbrks-1)
		}
		
		if (showNA) legend.labels <- c(legend.labels, legend.NA.text)
	}
	list(cols=cols, legend.labels=legend.labels, legend.palette=legend.palette, breaks=breaks, breaks.palette=breaks.palette, legend.neutral.col = legend.neutral.col)
}


fancy_breaks <- function(vec, intervals=FALSE, scientific=FALSE, text.separator="to", text.less.than="less than", text.or.more="or more", digits=NA, ...) {
	args <- list(...)

	### analyse the numeric vector
	n <- length(vec)
	frm <- gsub(" ", "", sprintf("%20.10f", abs(vec[!is.infinite(vec)])))
	
	# get width before decimal point
	mag <- max(nchar(frm)-11)
	
	# get number of decimals (which is number of decimals in vec, which is reduced when mag is large)
	ndec <- max(10 - nchar(frm) + nchar(sub("0+$","",frm)))
	if (is.na(digits)) digits <- max(min(ndec, 4-mag), 0)
	
	
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
		
		x <- paste(do.call("formatC", c(list(x=vec, digits=digits), args)), ext, sep="")

		if (intervals) {
			x[vec==-Inf] <- ""
			lbls <- paste(x[-n], x[-1], sep = paste0(" ", text.separator, " "))
			if (vec[1]==-Inf) lbls[1] <- paste(text.less.than, x[2])
			if (vec[n]==Inf) lbls[n-1] <- paste(x[n-1], text.or.more)
		}
		
	} else {
		if (!("format" %in% names(args))) args$format <- "g"
		
		x <- do.call("formatC", c(list(x=vec, digits=digits), args))
		if (intervals) {
			lbls <- paste("[", x[-n], ", ", x[-1], ")", sep="")
			lbls[n-1] <- paste(substr(lbls[n-1], 1, nchar(lbls[n-1])-1), "]", sep="")
		}
	}
	
	if (intervals) lbls else x
}

