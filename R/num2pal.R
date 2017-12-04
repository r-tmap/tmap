num2pal <- function(x, n = 5,
					   style = "pretty",
                       breaks = NULL,
					   interval.closure="left",
					   palette = NULL,
					   auto.palette.mapping = TRUE,
					   contrast = 1,
					   legend.labels = NULL,
					   colorNA = "#FF1414",
					   legend.NA.text = "Missing",
					   showNA=NA,
					   process.colors=NULL,
					   legend.format=list(scientific=FALSE),
					reverse=FALSE
					) {
	breaks.specified <- !is.null(breaks)
	is.cont <- (style=="cont" || style=="order")
	
	if (is.cont) {
		style <- ifelse(style=="order", "quantile", "fixed")

		if (style=="fixed") {
			custom_breaks <- breaks
			
			if (!is.null(custom_breaks)) {
				n <- length(breaks) - 1
			} else {
				breaks <- range(x, na.rm = TRUE)
			}
			breaks <- cont_breaks(breaks, n=101)
		}
		
		if (is.null(legend.labels)) {
			ncont <- n
		} else {
			if (!is.null(custom_breaks) && length(legend.labels) != n+1) {
				warning("legend.labels not the same length as breaks", call.=FALSE)
				legend.labels <- NULL
			} else {
				ncont <- length(legend.labels)	
			}
		}
		
		q <- num2breaks(x=x, n=101, style=style, breaks=breaks, approx=TRUE, interval.closure=interval.closure)
		n <- length(q$brks) - 1
	} else {
		q <- num2breaks(x=x, n=n, style=style, breaks=breaks, interval.closure=interval.closure)
	}

	breaks <- q$brks
	nbrks <- length(breaks)
	int.closure <- attr(q, "intervalClosure")
	
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

	if (is.cont) {
		# recreate legend palette for continuous cases
		if (style=="quantile") {
			id <- seq(1, n+1, length.out=ncont)
			b <- breaks[id]
			nbrks_cont <- length(b)
		} else {
			if (!is.null(custom_breaks)) {
				b <- custom_breaks
			} else {
				b <- pretty(breaks, n=ncont)
				b <- b[b>=breaks[1] & b<=breaks[length(breaks)]]
			}
			nbrks_cont <- length(b)
			id <- as.integer(cut(b, breaks=breaks, include.lowest = TRUE))
		}

		id_step <- id[2] - id[1]
		id_lst <- lapply(id, function(i){
			res <- round(seq(i-floor(id_step/2), i+ceiling(id_step/2), length.out=11))[1:10]
			res[res<1 | res>101] <- NA
			res
		})
		legend.palette <- lapply(id_lst, function(i) {
			if (reverse) rev(legend.palette[i]) else legend.palette[i]
		})
		
		if (reverse) legend.palette <- rev(legend.palette)
		
		if (showNA) legend.palette <- c(legend.palette, colorNA)
		
		# temporarily stack gradient colors
		legend.palette <- sapply(legend.palette, paste, collapse="-")
		
		# create legend values
		legend.values <- b
		
		# create legend labels for continuous cases
		if (is.null(legend.labels)) {
			legend.labels <- do.call("fancy_breaks", c(list(vec=b, intervals=FALSE, interval.closure=int.closure), legend.format)) 	
		} else {
			legend.labels <- rep(legend.labels, length.out=nbrks_cont)
			attr(legend.labels, "align") <- legend.format$text.align
		}
		
		if (reverse) {
			legend.labels.align <- attr(legend.labels, "align")
			legend.labels <- rev(legend.labels)
			attr(legend.labels, "align") <- legend.labels.align
		} 
		if (showNA) {
			legend.labels.align <- attr(legend.labels, "align")
			legend.labels <- c(legend.labels, legend.NA.text)
			attr(legend.labels, "align") <- legend.labels.align
		}
		attr(legend.palette, "style") <- style
	} else {
		

		# create legend values
		legend.values <- breaks[-nbrks]
		
		# create legend labels for discrete cases
		if (is.null(legend.labels)) {
			legend.labels <- do.call("fancy_breaks", c(list(vec=breaks, intervals=TRUE, interval.closure=int.closure), legend.format)) 
		} else {
			if (length(legend.labels)!=nbrks-1) warning("number of legend labels should be ", nbrks-1, call. = FALSE)
			legend.labels <- rep(legend.labels, length.out=nbrks-1)
			attr(legend.labels, "align") <- legend.format$text.align
		}
		
		if (reverse) {
			legend.labels.brks <- attr(legend.labels, "brks")
			legend.labels.align <- attr(legend.labels, "align")
			legend.labels <- rev(legend.labels)
			if (!is.null(legend.labels.brks)) {
				attr(legend.labels, "brks") <- legend.labels.brks[length(legend.labels):1L,]
			}
			attr(legend.labels, "align") <- legend.labels.align
			legend.palette <- rev(legend.palette)
		}		
		if (showNA) {
			legend.labels.brks <- attr(legend.labels, "brks")
			legend.labels.align <- attr(legend.labels, "align")
			#legend.labels <- c(legend.labels, legend.NA.text)
			if (!is.null(legend.labels.brks)) {
				legend.labels <- c(legend.labels, paste(legend.NA.text, " ", sep = ""))
				attr(legend.labels, "brks") <- rbind(legend.labels.brks, rep(nchar(legend.NA.text) + 2, 2))
			} else {
				legend.labels <- c(legend.labels, legend.NA.text)
			}
			attr(legend.labels, "align") <- legend.labels.align
			legend.palette <- c(legend.palette, colorNA)
		}
	}
		
	list(cols=cols, legend.labels=legend.labels, legend.values=legend.values, legend.palette=legend.palette, breaks=breaks, breaks.palette=breaks.palette, legend.neutral.col = legend.neutral.col)
}


cont_breaks <- function(breaks, n=101) {
	x <- round(seq(1, 101, length.out=length(breaks)))
	
	
	unlist(lapply(1L:(length(breaks)-1L), function(i) {
		y <- seq(breaks[i], breaks[i+1], length.out=x[i+1]-x[i]+1)	
		if (i!=1) y[-1] else y
	}))
}

