tmapValuesCheck_fill = function(x) {
	palid = tmapPalId(x[1])
	if (!is.na(palid)) TRUE else valid_colors(x[1])
}

tmapValuesCheck_col = function(x) {
	tmapValuesCheck_fill(x)
}


tmapValuesCheck_size = function(x) {
	inherits(x, "tmapSeq") || (is.numeric(x) && (all(x>=0) || all(x<=0)))
}

tmapValuesCheck_lwd = function(x) {
	tmapValuesCheck_size(x)
}




tmapValuesIsDiv_fill = function(x) {
	palid = tmapPalId(x[1])
	if (!is.na(palid)) {
		.tmap_pals$type[palid] == "div"
	} else {
		(palette_type(values) == "div")
	}
}

tmapValuesIsDiv_col = function(x) {
	tmapValuesIsDiv_fill(x)
}

tmapValuesIsDiv_size = function(x) {
	inherits(x, "tmapSeq") && (x$from < 0) && (x$to > 1) || (is.numeric(x) && (any(x < 0) && any(x> 0)))
}

tmapValuesIsDiv_lwd = function(x) {
	tmapValuesIsDiv_size(x)
}


tmapValuesVV_fill = function(x, isdiv, n, breaks, midpoint, contrast) {
	palid = tmapPalId(x[1])
	
	if (!is.na(palid) && isdiv) {
		colpal = tmap_get_palette(x, n = 101)
		snap = FALSE
	} else if (!is.na(palid) && !isdiv) {
		colpal = tmap_get_palette(x, n = n)
		snap = TRUE
	} else if (length(x) != n) {
		colpal = grDevices::colorRampPalette(x, n = 101)
		snap = FALSE
	} else {
		colpal = x
		snap = TRUE
	}
	
	if (!snap) {
		ids = if (isdiv) {
			map2divscaleID(breaks - midpoint, n=101, contrast=contrast)
		} else {
			map2seqscaleID(breaks, n=101, contrast=contrast, breaks.specified=FALSE, show.warnings = show.warnings)
		}
		
		vvalues = colpal[ids]
		#if (is.na(value.neutral)) {
		if (any(ids<51) && any(ids>51)) {
			ids.neutral <- min(ids[ids>=51]-51) + 51
			value.neutral = colpal[ids.neutral]
		} else {
			value.neutral = colpal[ids[round(((length(ids)-1)/2)+1)]]
		}
		#}
		
	} else {
		vvalues = colpal
		#if (is.na(value.neutral)) value.neutral = colpal[round((n+1)/2)]
		value.neutral = colpal[round((n+1)/2)]
	}
	list(vvalues = vvalues, value.neutral = value.neutral)
	
}

tmapValuesVV_col = function(...) {
	do.call(tmapValuesVV_fill, args = list(...))
}


tmapValuesVV_size = function(x, isdiv, n, breaks, midpoint, contrast) {
	#break_mids = breaks[-(n+1)] + (breaks[-1] - breaks[-(n+1)]) / 2
	
	
	#vvalues = seq(x[1], x[2])
	vvalues = if (is.numeric(x) && length(x) == n) {
		if (contrast[1] !=0 || contrast[2] != 1) {
			warning("values.contrast not used because the individual values have been specified (instead of a sequence)", call. = FALSE)
		}
		x
	} else {
		if (is.numeric(x)) {
			x = tmap_seq(x[1], x[length(x)], curve = "lin")
		}

		if (contrast[1] !=0 || contrast[2] != 1) {
			contrast_curved = tmapSeq(tmap_seq(from = contrast[1], to = contrast[2], curve = x$curve), n = 2, rescale = FALSE)
			x$from = x$from + contrast_curved[1] * (x$to - x$from)
			x$to = x$from + contrast_curved[2] * (x$to - x$from)
		}
		tmapSeq(x, n)
		
	}
	
	# (inherits(x, "tmapSeq")) {
	# 	tmapSeq(x, n)
	# } else if (length(x) == n) {
	# 	x 
	# } else {
	# 	seq(x[1], x[n], length.out = n)
	# }
	# 
	# if (isdiv) {
	# 	colpal =  seq(x[1], x[2], length.out = 1001)[map2divscaleID(breaks - midpoint, n=1001, contrast=contrast)]
	# } else {
	# 	#colpal =  seq(values[1], values[2], length.out = n) #seq(palette[1], palette[2], length.out = 1001)[map2seqscaleID(breaks, n=1001, contrast=contrast, breaks.specified=breaks.specified, show.warnings = show.warnings)]
	# 	colpal = seq(x[1], x[2], length.out = 1001)[map2seqscaleID(breaks, n = 1001, contrast = contrast)]
	# }
	# vvalues = colpal
	value.neutral = vvalues[round((n+1)/2)]
	
	
	list(vvalues = vvalues, value.neutral = value.neutral)
}

tmapValuesVV_lwd = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}



tmapValuesVV_shape = function(x, isdiv, n, breaks, contrast) {
	colpal = rep(x, length.out = n)
	snap = TRUE
	
}


tmap_seq = function(from = 0, to = 1, curve = c("lin", "sqrt", "sqrt_perceptual", "quadratic")) {
	curve = match.arg(curve)
	structure(as.list(environment()), class = "tmapSeq")
}

tmapSeq = function(s, n, rescale = TRUE) {
	with(s, {
		power = switch(curve, lin = 1, sqrt = 0.5, sqrt_perceptual = 0.5716, squadratic = 2)
		r = seq(from = from, to = to, length.out = n) ^ power
		if (rescale) {
			(r - (r[1])) / (r[n] - r[1]) * (to - from) + from	
		} else {
			r
		}
	})
}



tmapScaleIntervals = function(x1, scale, legend, opt, aes, layer, gp) {
	cls = data_class(x1)
	
	print(gp)
	
	if (cls[1] == "na") stop("data contain only NAs, so tm_scale_class_int cannot be applied", call. = FALSE)
	if (cls[1] != "num") stop("tm_scale_intervals can only be used for numeric data", call. = FALSE)
	
	values = if (is.na(scale$values[1])) getAesOption("values.var", opt, aes, layer, cls = cls) else scale$values
	value.na = if (is.na(scale$value.na) || identical(scale$value.na, TRUE)) getAesOption("value.na", opt, aes, layer, cls = cls) else scale$value.na
	value.null = if (is.na(scale$value.null)) getAesOption("value.null", opt, aes, layer, cls = cls) else scale$value.null
	value.neutral = if (is.na(scale$value.neutral)) getAesOption("value.neutral", opt, aes, layer, cls = cls) else scale$value.neutral
	
	# if (inherits(values, "tmapSeq")) {
	# 	values = tmapSeq(values, n = scale$n)
	# }
	
	values.contrast = if (is.na(scale$values.contrast[1])) getAesOption("values.contrast", opt, aes, layer, cls = cls) else scale$values.contrast
	
	udiv = use_div(scale$breaks, scale$midpoint)
	if (identical(udiv, TRUE)) type = "div"
	
	show.messages <- opt$show.messages
	show.warnings <- opt$show.warnings
	
	breaks = scale$breaks
	style = scale$style
	
	labels = scale$labels
	
	label.na = scale$label.na
	na.show = identical(label.na, TRUE) || (!is.na(label.na) && label.na != "")
	if (is.na(label.na)) na.show = NA # will be TRUE if there are NAs
	
	

	if (is.logical(label.na)) label.na = getAesOption("label.na", opt, aes, layer, cls = cls)

	#breaks.specified <- !is.null(breaks)
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
	
	# update contrast if NA (automatic)
	if (is.na(values.contrast[1])) {
		values.contrast = c(.5/n, 1 - .5/n)
	}
	if (length(values.contrast) == 1) values.contrast = c(0, values.contrast)
	
	
	# reverse palette
	# if (length(palette)==1 && substr(palette[1], 1, 1)=="-") {
	# 	revPal <- function(p)rev(p)
	# 	palette <- substr(palette, 2, nchar(palette))
	# } else revPal <- function(p)p
	
	
	# map palette
	
	fun_check = paste0("tmapValuesCheck_", gp)
	
	are_valid = do.call(fun_check, args = list(x = values))
	if (!are_valid) stop("Incorrect values for layer ", layer, ", aesthetic ", aes, "; values should conform gp ", gp, call. = FALSE)
	
	# palid = tmapPalId(values[1])
	# 
	# arecolors = if (is.na(palid)) {
	# 	valid_colors(values[1])
	# } else TRUE
	# 
	# arenumbers = !arecolors && is.numeric(values)
	

	fun_isdiv = paste0("tmapValuesIsDiv_", gp)
	
	isdiv = do.call(fun_isdiv, args = list(x = values))
	
		
	midpoint = scale$midpoint
	
	# if (arecolors) {
	# 	if (!is.na(palid)) {
	# 		pal.div = .tmap_pals$type[palid] == "div"
	# 	} else {
	# 		pal.div = (!is.null(midpoint)) || (palette_type(values) == "div")
	# 	}
	# } else if (arenumbers) {
	# 	pal.div = any(values < 0) && any(values > 0)
	# } else {
	# 	pal.div = FALSE
	# }
	
	# determine midpoint
	if ((is.null(midpoint) || is.na(midpoint)) && isdiv) {
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
	
	fun_getVV = paste0("tmapValuesVV_", gp)
	VV = do.call(fun_getVV, list(x = values, isdiv = isdiv, n = n, breaks = breaks, midpoint = midpoint, contrast = values.contrast))
	
	vvalues = VV$vvalues
	if (is.na(value.neutral)) value.neutral = VV$value.neutral



	ids = classInt::findCols(q)
	vals = vvalues[ids]
	anyNA = any(is.na(vals))
	
	if (is.na(na.show)) na.show = anyNA
	if (anyNA) vals[is.na(vals)] = value.na

	if (is.na(value.na))
	
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
	
	
	
	if (na.show) {
		labels.brks = attr(labels, "brks")
		labels.align = attr(labels, "align")
		#labels <- c(labels, legend.NA.text)
		if (!is.null(labels.brks)) {
			labels <- c(labels, paste(label.na, " ", sep = ""))
			attr(labels, "brks") = rbind(labels.brks, rep(nchar(label.na) + 2, 2))
		} else {
			labels = c(labels, label.na)
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
	
	
	format_aes_results(vals, legend)
}
