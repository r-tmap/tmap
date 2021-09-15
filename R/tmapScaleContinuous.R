tmapScaleContinuous = function(x1, scale, legend, opt, aes, layer, p) {
	style = if (inherits(scale, "tm_scale_continuous")) {
		"cont"
	} else if (inherits(scale, "tm_scale_log10")) {
		"log10"
	} else if (inherits(scale, "tm_scale_rank")) {
		"rank"
	}
	
	cls = data_class(x1)
	maincls = class(scale)[1]
	
	#if (cls[1] == "na") stop("data contain only NAs, so ", maincls, " cannot be applied", call. = FALSE)

	if (cls[1] != "num") {
		if (!is.factor(x1)) x1 = as.factor(x1)
		x1 = as.integer(x1)
		warning(maincls, " is supposed to be applied to numerical data", call. = FALSE)
	}
	
	if (inherits(x1, "units")) x1 = units::drop_units(x1)
	
	if (p %in% c("lty", "shape", "pattern")) stop("tm_scale_continuous cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)
	
	
	scale = get_scale_defaults(scale, opt, aes, layer, cls)
	
	show.messages <- opt$show.messages
	show.warnings <- opt$show.warnings
	
	with(scale, {
		udiv = identical(use_div(brks = NULL, midpoint), TRUE)

		if (all(is.na(x1))) return(tmapScale_returnNA(n = length(x1), legend = legend, value.na = value.na, label.na = label.na, na.show = na.show))
		
		ticks.specified = !is.null(ticks)
		
		is.log = (style == "log10")
		
		if (is.log && !attr(legend$format, "big.num.abbr.set")) legend$format$big.num.abbr = NA
		
		if (style == "log10") x1 = log10(x1)
		style = ifelse(style=="rank", "quantile", "fixed")
		
		if (style=="fixed") {
			if (ticks.specified) {
				breaks = ticks
				n = length(ticks) - 1
			} else {
				breaks = range(x1, na.rm = TRUE)
				# make sure at least one log10 falls in the range
				if (is.log && ceiling(breaks[1]) > floor(breaks[2])) breaks = c(floor(breaks[1]), ceiling(breaks[2]))
			}
			breaks = cont_breaks(breaks, n=101)
		}
		# } else {
		# 	# if ("breaks" %in% call && show.warnings) warning("breaks cannot be set for style = \"order\".", 
		# 	# 												 ifelse("labels" %in% call, "", " Breaks labels can be set with the argument labels."), ifelse(any(c("labels", "n") %in% call), "", " The number of breaks can be specified with the argument n."),  call. = FALSE)
		# 	custom_breaks <- breaks
		# }
		
		if (is.null(labels)) {
			ncont = n
		} else {
			if (ticks.specified && length(labels) != n+1) {
				if (show.warnings) warning("The length of legend labels is ", length(labels), ", which differs from the length of the breaks (", (n+1), "). Therefore, legend labels will be ignored", call.=FALSE)
				labels = NULL
			} else {
				if (style == "quantile" && ("n" %in% call) && show.warnings) {
					warning("n will not be used since labels are specified. Therefore, n will be set to the number of labels.", call. = FALSE)
				}
				ncont = length(labels)	
			}
		}
		q = num2breaks(x = x1, n = 101, style = style, breaks=breaks, approx=TRUE, interval.closure = "left", var=paste(layer, aes, sep = "-"), args = style.args)
		
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
		
		
		fun_check = paste0("tmapValuesCheck_", p)
		
		are_valid = do.call(fun_check, args = list(x = values))
		if (!are_valid) stop("Incorrect values for layer ", layer, ", aesthetic ", aes, "; values should conform p ", p, call. = FALSE)
		
		fun_isdiv = paste0("tmapValuesIsDiv_", p)
		
		isdiv = do.call(fun_isdiv, args = list(x = values))

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
		VV = do.call(fun_getVV, list(x = values, isdiv = isdiv, n = n, dvalues = breaks, midpoint = midpoint, contrast = values.contrast, are_breaks = TRUE, rep = values.repeat))
		
		vvalues = VV$vvalues
		if (is.na(value.neutral)) value.neutral = VV$value.neutral
		
		
		
		ids = classInt::findCols(q)
		vals = vvalues[ids]
		anyNA = any(is.na(vals))
		
		if (is.na(na.show)) na.show = anyNA
		if (anyNA) vals[is.na(vals)] = value.na
		
		
		if (style=="quantile") {
			id = seq(1, n+1, length.out=ncont)
			b = breaks[id]
			nbrks_cont = length(b)
		} else {
			if (ticks.specified) {
				b = ticks
			} else {
				if (is.log) {
					b = seq(floor(min(breaks)), ceiling(max(breaks)))
				} else {
					b = pretty(breaks, n=ncont)
				}
				b = b[b>=breaks[1] & b<=breaks[length(breaks)]]
			}
			nbrks_cont <- length(b)
			id = as.integer(cut(b, breaks=breaks, include.lowest = TRUE))
		}
		
		id_step = id[2] - id[1]
		id_lst = lapply(id, function(i){
			res = round(seq(i-floor(id_step/2), i+ceiling(id_step/2), length.out=11))[1:10]
			res[res<1 | res>101] = NA
			res
		})
		vvalues = lapply(id_lst, function(i) {
			if (legend$reverse) rev(vvalues[i]) else vvalues[i]
		})
		
		if (legend$reverse) vvalues = rev(vvalues)
		
		if (na.show) vvalues = c(vvalues, value.na)
		
		# temporarily stack gradient colors
		vvalues = sapply(vvalues, paste, collapse="-")
		
		
		# detransform log 
		if (is.log) {
			breaks = 10^breaks
			b = 10^b
		}
		
		# create legend values
		values = b
		
		# create legend labels for continuous cases
		if (is.null(labels)) {
			labels = do.call("fancy_breaks", c(list(vec=b, as.count = FALSE, intervals=FALSE, interval.closure=int.closure), legend$format)) 	
		} else {
			labels = rep(labels, length.out=nbrks_cont)
			attr(labels, "align") <- legend$format$text.align
		}
		
		if (legend$reverse) {
			labels.align = attr(labels, "align")
			labels = rev(labels)
			attr(labels, "align") = labels.align
		} 
		if (na.show) {
			labels.align = attr(labels, "align")
			labels = c(labels, label.na)
			attr(labels, "align") = labels.align
		}
		#attr(vvalues, "style") = style
		
		legend = list(title = legend$title, 
					  nitems = length(labels),
					  labels = labels, 
					  dvalues = values, 
					  vvalues = vvalues,
					  vneutral = value.neutral,
					  na.show = na.show,
					  setup = legend)

		format_aes_results(vals, legend)
		
	})
}
