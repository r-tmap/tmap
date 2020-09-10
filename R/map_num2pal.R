prettyCount <- function(x, n, ...) {
	x <- na.omit(x)
	if (!length(x)) return(x)
	
	if (!is.integer(x)) x <- as.integer(x)

	
	mn <- min(x)
	mx <- max(x)
	
	any0 <- any(x==0)
	
	if (mn < 0) {
		n <- floor(n / 2)
		pneg <- -rev(prettyCount(-x[x<0], n = n, ...)) + 1L
		pneg <- pneg[pneg != 0L]
		x <- x[x>0]
		any0 <- TRUE
	} else {
		pneg <- integer()
	}
	
	if (any0) x <- x[x!=0L]
	
	p <- pretty(x - 1L, n = n, ...) + 1L
	
	p <- p[(p %% 1) == 0]
	p <- p[p!=0L]
	
	if (length(x) < 2) {
		if (any0) return(c(0L, p)) else return(p)
	}
	
	
	step <- p[2] - p[1]
	if (p[length(p)] == mx) p <- c(p, mx+step)
	
	if (any0) {
		c(pneg, 0L, p)
	} else {
		c(pneg, p)
	}
}

num2pal <- function(x, 
					var,
					call,
					n = 5,
					style = "pretty",
					style.args = list(),
					as.count = NA,
					breaks = NULL,
					interval.closure="left",
					palette = NULL,
					midpoint = NA, #auto.palette.mapping = TRUE,
					contrast = 1,
					legend.labels = NULL,
					colorNA = "#FF1414",
					colorNULL = "#FFFFFF",
					legend.NA.text = "Missing",
					showNA=NA,
					process.colors=NULL,
					legend.format=list(scientific=FALSE),
					reverse=FALSE
					) {
	sel <- attr(x, "sel")
	if (is.null(sel)) sel <- rep(TRUE, length(x))
	
	x[!sel] <- NA
	
	tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)
	show.messages <- tmapOptions$show.messages
	show.warnings <- tmapOptions$show.warnings
	
	if (!any(style == c("pretty", "log10_pretty", "fixed"))) {
		if (identical(as.count, TRUE) && show.warnings) warning("as.count not implemented for styles other than \"pretty\", \"log10_pretty\" and \"fixed\"", call. = FALSE)
		as.count <- FALSE
	}
	if (is.na(as.count)) {
		as.count <- is.integer(x) && !length(which(x < 0))
	}
	

	
	if (as.count) {
		if (interval.closure != "left" && show.warnings) warning("For as.count = TRUE, interval.closure will be set to \"left\"", call. = FALSE)
		interval.closure <- "left"
	}
	
	
	breaks.specified <- !is.null(breaks)
	is.cont <- (style=="cont" || style=="order" || style=="log10")
	is.log <- (style %in% c("log10", "log10_pretty"))
	
	if (is.log && !attr(legend.format, "big.num.abbr.set")) legend.format$big.num.abbr <- NA
	
	
	if (is.cont) {
		if (style == "log10") {
			x <- log10(x)
		}
		
		#l10 <- (style=="log10")
		style <- ifelse(style=="order", "quantile", "fixed")

		
		
		if (style=="fixed") {
			if ((!is.null(breaks) || !is.null(legend.labels)) && ("n" %in% call) && show.warnings) {
				warning("n will not be used since breaks and/or labels are specified. Therefore, n will be set to the number of breaks/labels.", call. = FALSE)
			}

			custom_breaks <- breaks
			
			if (!is.null(custom_breaks)) {
				n <- length(breaks) - 1
			} else {
				breaks <- range(x, na.rm = TRUE)
			}
			breaks <- cont_breaks(breaks, n=101)
		} else {
			if ("breaks" %in% call && show.warnings) warning("breaks cannot be set for style = \"order\".", 
											ifelse("labels" %in% call, "", " Breaks labels can be set with the argument labels."), ifelse(any(c("labels", "n") %in% call), "", " The number of breaks can be specified with the argument n."),  call. = FALSE)
			custom_breaks <- breaks
		}
		
		if (is.null(legend.labels)) {
			ncont <- n
		} else {
			if (!is.null(custom_breaks) && length(legend.labels) != n+1) {
				if (show.warnings) warning("The length of legend.labels is ", length(legend.labels), ", which differs from the length of the breaks (", (n+1), "). Therefore, legend.labels will be ignored", call.=FALSE)
				legend.labels <- NULL
			} else {
				if (style == "quantile" && ("n" %in% call) && show.warnings) {
					warning("n will not be used since labels are specified. Therefore, n will be set to the number of labels.", call. = FALSE)
				}
				ncont <- length(legend.labels)	
			}
		}
		q <- num2breaks(x=x, n=101, style=style, breaks=breaks, approx=TRUE, interval.closure=interval.closure, var=var, args = style.args)
	} else {
		if (style == "log10_pretty") {
			x <- log10(x)
			style <- "pretty"
		} else if (as.count && style == "pretty") {
			breaks <- prettyCount(x, n=n)
			style <- "fixed"
		} else if (as.count && style == "fixed") {
			breaks[length(breaks)] <- breaks[length(breaks)] + 1L
		}	

		
		q <- num2breaks(x=x, n=n, style=style, breaks=breaks, interval.closure=interval.closure, var=var, as.count = as.count && !is.log, args = style.args)
	}
	
	
	breaks <- q$brks
	nbrks <- length(breaks)
	n <- nbrks - 1
	
	int.closure <- attr(q, "intervalClosure")
	
	# reverse palette
	if (length(palette)==1 && substr(palette[1], 1, 1)=="-") {
		revPal <- function(p)rev(p)
		palette <- substr(palette, 2, nchar(palette))
	} else revPal <- function(p)p
	
	
	# map palette
	is.palette.name <- palette[1] %in% rownames(tmap.pal.info)
	
	is.brewer <- is.palette.name && tmap.pal.info[palette[1], "origin"] == "brewer"
	is.viridis <- is.palette.name &&  tmap.pal.info[palette[1], "origin"] == "viridis"
	
	if (is.palette.name) {
		mc <- tmap.pal.info[palette, "maxcolors"]
		pal.div <- (!is.null(midpoint)) || (tmap.pal.info[palette, "category"]=="div")
	} else {
		palette.type <- if (!is.null(midpoint)) "div" else palette_type(palette)
		pal.div <- palette.type=="div"
	}
	
	if ((is.null(midpoint) || is.na(midpoint)) && pal.div) {
		rng <- range(x, na.rm = TRUE)
		if (rng[1] < 0 && rng[2] > 0 && is.null(midpoint)) {
			
			if (show.messages) message("Variable(s) \"", paste(var, collapse = "\", \""), "\" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.")
			midpoint <- 0
		} else {
			if ((n %% 2) == 1) {
				# number of classes is odd, so take middle class (average of those breaks)
				midpoint <- mean.default(breaks[c((n+1) / 2, (n+3) / 2)])
			} else {
				midpoint <- breaks[(n+2) / 2]
			}
		}
	}

	## palette is created with a 101 colorramp, unless the defined palette is of the same length as n
	if (is.brewer) {
		snap <- FALSE
		colpal <- colorRampPalette(revPal(brewer.pal(mc, palette)), space="rgb")(101)
	} else if (is.viridis) {
		snap <- FALSE
		colpal <- revPal(viridis(101, option = palette))
	} else {
		snap <- length(palette) == n
		if (!snap) colpal <- colorRampPalette(revPal(palette), space="rgb")(101)
	}
	if (!snap) {
		ids <- if (pal.div) {
			if (is.na(contrast[1])) contrast <- if (is.brewer) default_contrast_div(n) else c(0, 1)
			map2divscaleID(breaks - midpoint, n=101, contrast=contrast)
		} else {
			if (is.na(contrast[1])) contrast <- if (is.brewer) default_contrast_seq(n) else c(0, 1)
			map2seqscaleID(breaks, n=101, contrast=contrast, breaks.specified=breaks.specified, show.warnings = show.warnings)
		}
		
		legend.palette <- colpal[ids]
		if (any(ids<51) && any(ids>51)) {
			ids.neutral <- min(ids[ids>=51]-51) + 51
			legend.neutral.col <- colpal[ids.neutral]
		} else {
			legend.neutral.col <- colpal[ids[round(((length(ids)-1)/2)+1)]]
		}
	} else {
		legend.palette <- palette
		legend.neutral.col <- palette[round((n+1)/2)]
	}
	
		

	legend.palette <- do.call("process_color", c(list(col=legend.palette), process.colors))
	legend.neutral.col <- do.call("process_color", c(list(col=legend.neutral.col), process.colors))
	colorNA <- do.call("process_color", c(list(col=colorNA), process.colors))
	

	
	
	ids <- findCols(q)
	cols <- legend.palette[ids]
	anyNA <- any(is.na(cols) & sel)
	breaks.palette <- legend.palette
	if (anyNA) {
		if (is.na(showNA)) showNA <- any(is.na(cols) & sel)
		cols[is.na(cols)] <- colorNA
	} else {
		if (is.na(showNA)) showNA <- FALSE
	}
	cols[!sel] <- colorNULL

	
	
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
		
		
		# detransform log 
		if (is.log) {
			breaks <- 10^breaks
			b <- 10^b
		}
		
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
		# detransform log 
		if (is.log) {
			if (any((breaks %% 1) != 0)) warning("non-rounded breaks occur, because style = \"log10_pretty\" is designed for large values", call. = FALSE)
			breaks <- 10^breaks
		}
		

		# create legend values
		legend.values <- breaks[-nbrks]
		
		if (is.null(legend.labels)) {
			legend.labels <- do.call("fancy_breaks", c(list(vec=breaks, as.count = as.count, intervals=TRUE, interval.closure=int.closure), legend.format)) 
		} else {
			if (length(legend.labels)!=nbrks-1 && show.warnings) warning("number of legend labels should be ", nbrks-1, call. = FALSE)
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
	}), use.names = FALSE)
}

