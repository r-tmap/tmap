tmapScaleIntervals = function(x1, scale, legend, opt, aes, layer, p) {
	cls = data_class(x1)
	maincls = class(scale)[1]
	

#	if (cls[1] == "na") stop("data contain only NAs, so ", maincls, " cannot be applied", call. = FALSE)
	
	
	
	if (cls[1] != "num") {
		if (!is.factor(x1)) x1 = as.factor(x1)
		x1 = as.integer(x1)
		warning(maincls, " is supposed to be applied to numerical data", call. = FALSE)
	}

	if (p %in% c("pattern")) stop("tm_scale_intervals cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)
	
	values = if (is.na(scale$values[1])) getAesOption("values.var", opt, p, layer, cls = cls) else scale$values
	value.na = if (is.na(scale$value.na) || identical(scale$value.na, TRUE)) getAesOption("value.na", opt, p, layer, cls = cls) else scale$value.na
	value.null = if (is.na(scale$value.null)) getAesOption("value.null", opt, p, layer, cls = cls) else scale$value.null
	value.neutral = if (is.na(scale$value.neutral)) getAesOption("value.neutral", opt, p, layer, cls = cls) else scale$value.neutral
	
	# if (inherits(values, "tmapSeq")) {
	# 	values = tmapSeq(values, n = scale$n)
	# }
	

	
	
	
	
	values.contrast = if (is.na(scale$values.contrast[1])) getAesOption("values.contrast", opt, p, layer, cls = cls) else scale$values.contrast
	
	udiv = identical(use_div(scale$breaks, scale$midpoint), TRUE)
	#if (identical(udiv, TRUE)) type = "div"
	
	
	
	
	show.messages <- opt$show.messages
	show.warnings <- opt$show.warnings
	
	breaks = scale$breaks
	style = scale$style
	
	labels = scale$labels
	
	label.na = scale$label.na
	na.show = identical(label.na, TRUE) || (!is.na(label.na) && label.na != "")
	if (is.na(label.na)) na.show = NA # will be TRUE if there are NAs
	
	if (is.logical(label.na)) label.na = getAesOption("label.na", opt, p, layer, cls = cls)
	
	
	if (all(is.na(x1))) {
		if (identical(na.show, FALSE)) {
			legend = list(title = NA, 
						  nitems = 0,
						  labels = NA, 
						  dvalues = NA, 
						  vvalues = NA,
						  vneutral = value.na,
						  na.show = NA,
						  setup = list(show = FALSE))
		} else {
			legend = list(title = legend$title, 
						  nitems = 1,
						  labels = label.na, 
						  dvalues = NA, 
						  vvalues = value.na,
						  vneutral = value.na,
						  na.show = TRUE,
						  setup = legend)
		}
		return(format_aes_results(rep(value.na, length(x1)), legend))
	}
	
	
	

	
	as.count = scale$as.count
	interval.closure = scale$interval.closure
	if (!any(style == c("pretty", "log10_pretty", "fixed"))) {
		if (identical(as.count, TRUE) && show.warnings) warning("as.count not implemented for styles other than \"pretty\", \"log10_pretty\" and \"fixed\"", call. = FALSE)
		as.count = FALSE
	}
	if (is.na(as.count)) {
		as.count = is.integer(x1) && !any(!is.na(x1) & x1 < 0)
	}

	if (as.count) {
		if (interval.closure != "left" && show.warnings) warning("For as.count = TRUE, interval.closure will be set to \"left\"", call. = FALSE)
		interval.closure = "left"
	}
	
	
	
	#breaks.specified <- !is.null(breaks)
	is.log = (style == "log10_pretty")
	
	if (is.log && !attr(legend$format, "big.num.abbr.set")) legend$format$big.num.abbr = NA
	
	if (style == "log10_pretty") {
		x1 = log10(x1)
		style = "fixed"
		breaks = seq(floor(min(x1, na.rm = TRUE)), ceiling(max(x1, na.rm=TRUE)))
	} else if (as.count && style == "pretty") {
		breaks = prettyCount(x1, n=scale$n)
		style <- "fixed"
	} else if (as.count && style == "fixed") {
		breaks[length(breaks)] = breaks[length(breaks)] + 1L
	}	
	
	
	
	q <- num2breaks(x=x1, n=scale$n, style=style, breaks=breaks, interval.closure=interval.closure, var=paste(layer, aes, sep = "-"), as.count = as.count, args = scale$style.args)
	
	
	breaks = q$brks
	nbrks = length(breaks)
	n = nbrks - 1
	
	int.closure <- attr(q, "intervalClosure")
	
	# update contrast if NA (automatic)
	if (is.na(values.contrast[1])) {
		fun_contrast = paste0("tmapValuesContrast_", p)
		values.contrast = do.call(fun_contrast, args = list(x = values, n = n, isdiv = udiv))
	}
	if (length(values.contrast) == 1) values.contrast = c(0, values.contrast)
	
	
	# reverse palette
	# if (length(palette)==1 && substr(palette[1], 1, 1)=="-") {
	# 	revPal <- function(p)rev(p)
	# 	palette <- substr(palette, 2, nchar(palette))
	# } else revPal <- function(p)p
	
	
	# map palette
	
	fun_check = paste0("tmapValuesCheck_", p)
	
	are_valid = do.call(fun_check, args = list(x = values))
	if (!are_valid) stop("Incorrect values for layer ", layer, ", aesthetic ", aes, "; values should conform p ", p, call. = FALSE)
	
	# palid = tmapPalId(values[1])
	# 
	# arecolors = if (is.na(palid)) {
	# 	valid_colors(values[1])
	# } else TRUE
	# 
	# arenumbers = !arecolors && is.numeric(values)
	

	fun_isdiv = paste0("tmapValuesIsDiv_", p)
	
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
	
	fun_getVV = paste0("tmapValuesVV_", p)
	VV = do.call(fun_getVV, list(x = values, isdiv = isdiv, n = n, dvalues = breaks, midpoint = midpoint, contrast = values.contrast, are_breaks = TRUE, rep = scale$values.repeat))
	
	vvalues = VV$vvalues
	if (is.na(value.neutral)) value.neutral = VV$value.neutral



	ids = classInt::findCols(q)
	vals = vvalues[ids]
	anyNA = any(is.na(vals))
	
	if (is.na(na.show)) na.show = anyNA
	if (anyNA) vals[is.na(vals)] = value.na


	# detransform log 
	if (is.log) {
		if (any((breaks %% 1) != 0)) warning("non-rounded breaks occur, because style = \"log10_pretty\" is designed for large values", call. = FALSE)
		breaks <- 10^breaks
	}
	
	
	# create legend values
	values = breaks[-nbrks]
	
	if (is.null(labels)) {
		labels = do.call("fancy_breaks", c(list(vec=breaks, as.count = as.count, intervals=TRUE, interval.closure=int.closure), legend$format)) 
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
	
	#type = ifelse(nchar(vvalues[1]) > 50, "color_cont", "color_cls")
	
	legend = list(title = legend$title, 
				  nitems = length(labels),
				  labels = labels, 
				  dvalues = values, 
				  vvalues = vvalues,
				  vneutral = value.neutral,
				  na.show = na.show,
				  setup = legend)
				  #breaks=scale$breaks, type = type)
	
	
	format_aes_results(vals, legend)
}
