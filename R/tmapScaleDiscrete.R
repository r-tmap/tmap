tmapScaleDiscrete = function(x1, scale, legend, opt, aes, layer, p) {
	cls = data_class(x1)
	
	if (cls[1] == "na") stop("data contain only NAs, so tm_scale_discrete cannot be applied", call. = FALSE)
	if (cls[1] != "num") stop("tm_scale_discrete can only be used for numeric data", call. = FALSE)
	
	if (p %in% c("lty", "shape", "pattern")) stop("tm_scale_discrete cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)
	
	values = if (is.na(scale$values[1])) getAesOption("values.var", opt, p, layer, cls = cls) else scale$values
	value.na = if (is.na(scale$value.na) || identical(scale$value.na, TRUE)) getAesOption("value.na", opt, p, layer, cls = cls) else scale$value.na
	value.null = if (is.na(scale$value.null)) getAesOption("value.null", opt, p, layer, cls = cls) else scale$value.null
	value.neutral = if (is.na(scale$value.neutral)) getAesOption("value.neutral", opt, p, layer, cls = cls) else scale$value.neutral
	
	# if (inherits(values, "tmapSeq")) {
	# 	values = tmapSeq(values, n = scale$n)
	# }
	
	values.contrast = if (is.na(scale$values.contrast[1])) getAesOption("values.contrast", opt, p, layer, cls = cls) else scale$values.contrast
	
	
	anyNA = any(is.na(x1))
	
	u = sort(unique(x1))
	#u = na.omit(u)
	rng = range(u)

	ticks = scale$ticks
	if (!is.na(ticks[1])) {
		if (!all(u %in% ticks)) stop("Values have been found for which no ticks have been specified", call. = FALSE)
		if (!is.na(scale$step)) warning("step is ignored because ticks have been specified", call. = FALSE)
	} else if (!is.na(scale$step)) {
		ticks = seq(rng[1], rng[2], by = scale$step)
		if (!all(u %in% ticks))  stop("Unable to fit discrete scale based on specified 'step'; please specify 'ticks'", call. = FALSE)
	} else {
		for (n in c(3, 5, 10, 15, 20, 30, 50)) {
			ticks_candidate = pretty(rng, n = n)
			
			found_ticks = all(u %in% ticks_candidate)
			if (found_ticks) {
				ticks = ticks_candidate
				break
			}
		}
		if (!found_ticks) stop("Unable to fit a discrete scale, probably due to too many values", call. = FALSE)
	}

	n = length(ticks)

	d_isdiv = rng[1] < 0 && rng[2] > 0	
	
	# udiv = use_div(scale$breaks, scale$midpoint)
	# if (identical(udiv, TRUE)) type = "div"
	
	show.messages <- opt$show.messages
	show.warnings <- opt$show.warnings
	
	# breaks = scale$breaks
	# style = scale$style
	
	labels = scale$labels
	
	label.na = scale$label.na
	na.show = identical(label.na, TRUE) || (!is.na(label.na) && label.na != "")
	if (is.na(label.na)) na.show = NA # will be TRUE if there are NAs
	
	
	
	if (is.logical(label.na)) label.na = getAesOption("label.na", opt, aes, layer, cls = cls)
	
	#breaks.specified <- !is.null(breaks)
	# is.log = (style == "log10_pretty")
	
	# if (is.log && !attr(legend$format, "big.num.abbr.set")) legend$format$big.num.abbr = NA
	
	# if (style == "log10_pretty") {
	# 	x1 = log10(x1)
	# 	style = "fixed"
	# 	breaks = seq(floor(min(x1, na.rm = TRUE)), ceiling(max(x1, na.rm=TRUE)))
	# }	
	
	# update contrast if NA (automatic)
	if (is.na(values.contrast[1])) values.contrast = c(0, 1)
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
		if (d_isdiv && is.null(midpoint)) {
			
			if (show.messages) message("Variable(s) \"", paste(aes, collapse = "\", \""), "\" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.")
			midpoint = 0
		} else {
			if ((n %% 2) == 1) {
				# number of classes is odd, so take middle class (average of those breaks)
				midpoint <- mean.default(ticks[c((n+1) / 2, (n+3) / 2)])
			} else {
				midpoint <- ticks[(n+2) / 2]
			}
		}
	}
	
	fun_getVV = paste0("tmapValuesVV_", p)
	VV = do.call(fun_getVV, list(x = values, isdiv = isdiv, n = n, dvalues = ticks, are_breaks = FALSE, midpoint = midpoint, contrast = values.contrast, rep = scale$values.repeat))
	
	vvalues = VV$vvalues
	if (is.na(value.neutral)) value.neutral = VV$value.neutral
	
	
	# 
	# ids = classInt::findCols(q)
	# vals = vvalues[ids]
	# anyNA = any(is.na(vals))
	
	vals = vvalues[match(x1, ticks)]
	
	
	if (is.na(na.show)) na.show = anyNA
	if (anyNA) vals[is.na(vals)] = value.na
	

	# create legend values
	#values = breaks[-nbrks]
	
	if (is.null(labels)) {
		labels = do.call("fancy_breaks", c(list(vec=ticks, as.count = FALSE, intervals=FALSE), legend$format)) 
	} else {
		if (length(labels)!=n && show.warnings) warning("number of legend labels should be ", n, call. = FALSE)
		labels = rep(labels, length.out=n)
		attr(labels, "align") <- legend$format$text.align
	}
	
	if (legend$reverse) {
		#labels.brks <- attr(labels, "brks")
		labels.align <- attr(labels, "align")
		labels <- rev(labels)
		# if (!is.null(labels.brks)) {
		# 	attr(labels, "brks") = labels.brks[length(labels):1L,]
		# }
		attr(labels, "align") = labels.align
		vvalues = rev(vvalues)
	}		
	
	
	
	if (na.show) {
		#labels.brks = attr(labels, "brks")
		labels.align = attr(labels, "align")
		#labels <- c(labels, legend.NA.text)
		# if (!is.null(labels.brks)) {
		# 	labels <- c(labels, paste(label.na, " ", sep = ""))
		# 	attr(labels, "brks") = rbind(labels.brks, rep(nchar(label.na) + 2, 2))
		# } else {
		labels = c(labels, label.na)
		
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
				  dvalues = ticks, 
				  vvalues = vvalues,
				  vneutral = value.neutral,
				  na.show = na.show,
				  setup = legend)
				  #breaks=scale$breaks)
	
	
	format_aes_results(vals, legend)
}
