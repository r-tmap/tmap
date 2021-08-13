tmapScaleIntervals = function(x1, scale, legend, opt, aes, layer) {
	cls = data_class(x1)
	
	if (cls[1] == "na") stop("data contain only NAs, so tm_scale_class_int cannot be applied", call. = FALSE)
	if (cls[1] != "num") stop("tm_scale_intervals can only be used for numeric data", call. = FALSE)
	
	values = if (is.na(scale$values[1])) getAesOption("values.var", opt, aes, layer, cls = cls) else scale$values
	value.na = if (is.na(scale$value.na)) getAesOption("value.na", opt, aes, layer, cls = cls) else scale$value.na
	value.null = if (is.na(scale$value.null)) getAesOption("value.null", opt, aes, layer, cls = cls) else scale$value.null
	value.neutral = if (is.na(scale$value.neutral)) getAesOption("value.neutral", opt, aes, layer, cls = cls) else scale$value.neutral
	
	udiv = use_div(scale$breaks, scale$midpoint)
	if (identical(udiv, TRUE)) type = "div"
	
	# colsLeg <- num2pal(x1, 
	# 				   var = "g$col",
	# 				   call = NULL,
	# 				   n = scale$n, 
	# 				   style=scale$style, 
	# 				   style.args=scale$style.args,
	# 				   as.count = FALSE, #scale$as.count,
	# 				   breaks=scale$breaks, 
	# 				   interval.closure=scale$interval.closure,
	# 				   palette = scale$values,
	# 				   neutral = scale$value.neutral,
	# 				   midpoint = scale$midpoint, #auto.palette.mapping = scale$auto.palette.mapping,
	# 				   contrast = scale$values.contrast, legend.labels=scale$labels,
	# 				   colorNA=scale$value.na, 
	# 				   colorNULL=scale$value.null,
	# 				   legend.NA.text = scale$label.na,
	# 				   showNA = !is.na(scale$label.na),
	# 				   process.colors=c(list(alpha=opt$alpha), opt$pc),
	# 				   legend.format=legend$format,
	# 				   reverse=legend$reverse)
	
	
	
	show.messages <- opt$show.messages
	show.warnings <- opt$show.warnings
	
	breaks = scale$breaks
	style = scale$style
	
	labels = scale$labels
	label.na = scale$label.na
	
	breaks.specified <- !is.null(breaks)
	is.log = (style == "log10_pretty")
	
	if (is.log && !attr(legend$format, "big.num.abbr.set")) legend$format$big.num.abbr = NA
	
	if (style == "log10_pretty") {
		x1 = log10(x1)
		style = "fixed"
		breaks = seq(floor(min(x1, na.rm = TRUE)), ceiling(max(x1, na.rm=TRUE)))
	}	
	
	q <- num2breaks(x=x1, n=scale$n, style=style, breaks=breaks, interval.closure=scale$interval.closure, var=paste(layer, aes, sep = "-"), as.count = FALSE, args = scale$style.args)
	
	
	breaks = q$brks
	nbrks = length(breaks)
	n = nbrks - 1
	
	int.closure <- attr(q, "intervalClosure")
	
	# reverse palette
	# if (length(palette)==1 && substr(palette[1], 1, 1)=="-") {
	# 	revPal <- function(p)rev(p)
	# 	palette <- substr(palette, 2, nchar(palette))
	# } else revPal <- function(p)p
	
	
	# map palette
	
	palid = tmapPalId(values[1])
	
	arecolors = if (is.na(palid)) {
		valid_colors(values[1])
	} else TRUE
	
	arenumbers = !arecolors && is.numeric(values)
	
	
	midpoint = scale$midpoint
	
	if (arecolors) {
		if (!is.na(palid)) {
			pal.div = .tmap_pals$type[palid] == "div"
		} else {
			pal.div = (!is.null(midpoint)) || (palette_type(values) == "div")
		}
	} else if (arenumbers) {
		pal.div = any(values < 0) && any(values > 0)
	} else {
		pal.div = FALSE
	}
	
	
	if ((is.null(midpoint) || is.na(midpoint)) && pal.div) {
		rng <- range(x1, na.rm = TRUE)
		if (rng[1] < 0 && rng[2] > 0 && is.null(midpoint)) {
			
			if (show.messages) message("Variable(s) \"", paste(aes, collapse = "\", \""), "\" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.")
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
	
	if (arecolors) {
		if (!is.na(palid) && pal.div) {
			colpal = tmap_get_palette(values, n = 101)
			snap = FALSE
		} else if (!is.na(palid) && !pal.div) {
			colpal = tmap_get_palette(values, n = n)
			snap = TRUE
		} else if (length(values) != n) {
			colpal = grDevices::colorRampPalette(values, n = 101)
			snap = FALSE
		} else {
			colpal = values
			snap = TRUE
		}
	} else if (arenumbers) {
		if (pal.div) {
			colpal =  seq(values[1], values[2], length.out = 1001)[map2divscaleID(breaks - midpoint, n=1001, contrast=scale$values.contrast)]
			snap = TRUE
		} else {
			colpal =  seq(values[1], values[2], length.out = n) #seq(palette[1], palette[2], length.out = 1001)[map2seqscaleID(breaks, n=1001, contrast=contrast, breaks.specified=breaks.specified, show.warnings = show.warnings)]
			snap = TRUE
		}
	} else {
		colpal = rep(values, length.out = n)
		snap = TRUE
		#stop("values are not colors nor numbers")
	}
	
	
	if (!snap) {
		ids = if (pal.div) {
			map2divscaleID(breaks - midpoint, n=101, contrast=scale$values.contrast)
		} else {
			map2seqscaleID(breaks, n=101, contrast=scale$values.contrast, breaks.specified=breaks.specified, show.warnings = show.warnings)
		}
		
		vvalues = colpal[ids]
		if (is.na(value.neutral)) {
			if (any(ids<51) && any(ids>51)) {
				ids.neutral <- min(ids[ids>=51]-51) + 51
				value.neutral = colpal[ids.neutral]
			} else {
				value.neutral = colpal[ids[round(((length(ids)-1)/2)+1)]]
			}
		}
	} else {
		vvalues = colpal
		if (is.na(value.neutral)) value.neutral = colpal[round((n+1)/2)]
	}
	
	showNA = (label.na != "")
	
	
	ids <- classInt::findCols(q)
	cols <- vvalues[ids]
	anyNA <- any(is.na(cols))
	breaks.palette <- vvalues
	if (anyNA) {
		if (is.na(showNA)) showNA <- any(is.na(cols) & sel)
		cols[is.na(cols)] <- value.na
	} else {
		if (is.na(showNA)) showNA <- FALSE
	}
	
	
	
	# detransform log 
	if (is.log) {
		if (any((breaks %% 1) != 0)) warning("non-rounded breaks occur, because style = \"log10_pretty\" is designed for large values", call. = FALSE)
		breaks <- 10^breaks
	}
	
	
	# create legend values
	values = breaks[-nbrks]
	
	if (is.null(labels)) {
		labels = do.call("fancy_breaks", c(list(vec=breaks, as.count = FALSE, intervals=TRUE, interval.closure=int.closure), legend$format)) 
	} else {
		if (length(labels)!=nbrks-1 && show.warnings) warning("number of legend labels should be ", nbrks-1, call. = FALSE)
		labels = rep(labels, length.out=nbrks-1)
		attr(labels, "align") <- legend$format$text.align
	}
	
	if (legend$reverse) {
		labels.brks <- attr(labels, "brks")
		labels.align <- attr(labels, "align")
		labels <- rev(labels)
		if (!is.null(labels.brks)) {
			attr(labels, "brks") = labels.brks[length(labels):1L,]
		}
		attr(labels, "align") = labels.align
		vvalues = rev(vvalues)
	}		
	
	
	
	if (showNA) {
		labels.brks = attr(labels, "brks")
		labels.align = attr(labels, "align")
		#labels <- c(labels, legend.NA.text)
		if (!is.null(labels.brks)) {
			labels <- c(labels, paste(scale$label.na, " ", sep = ""))
			attr(labels, "brks") = rbind(labels.brks, rep(nchar(scale$label.na) + 2, 2))
		} else {
			labels = c(labels, scale$label.na)
		}
		attr(labels, "align") = labels.align
		vvalues = c(vvalues, value.na)
	}
	
	# temp solution to include NA, to do update num2pal
	# if (length(labels) == length(vvalues)) {
	# 	la = attributes(labels)
	# 	labels = c(labels, label.na)
	# 	attributes(labels) = la
	# 	values = c(values, NA)
	# }
	# 
	
	type = ifelse(nchar(vvalues[1]) > 50, "color_cont", "color_cls")
	
	legend = list(title = legend$title, 
				  nitems = length(labels),
				  labels = labels, 
				  dvalues = values, 
				  vvalues = vvalues,
				  vneutral = value.neutral,
				  setup = legend,
				  breaks=scale$breaks, type = type)
	
	
	format_aes_results(cols, legend)
}
