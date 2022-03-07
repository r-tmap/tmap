tmapScaleContinuous = function(x1, scale, legend, o, aes, layer, sortRev) {
	# style = if (inherits(scale, "tm_scale_continuous")) {
	# 	"cont"
	# } else if (inherits(scale, "tm_scale_log10")) {
	# 	"log10"
	# } else if (inherits(scale, "tm_scale_rank")) {
	# 	"rank"
	# }
	# 
	
	
	cls = data_class(x1)
	maincls = class(scale)[1]
	
	#if (cls[1] == "na") stop("data contain only NAs, so ", maincls, " cannot be applied", call. = FALSE)

	if (cls[1] != "num") {
		if (!is.factor(x1)) x1 = as.factor(x1)
		x1 = as.integer(x1)
		warning(maincls, " is supposed to be applied to numerical data", call. = FALSE)
	}
	
	if (inherits(x1, "units")) x1 = units::drop_units(x1)
	
	if (aes %in% c("lty", "shape", "pattern")) stop("tm_scale_continuous cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)
	
	scale = get_scale_defaults(scale, o, aes, layer, cls)
	
	show.messages <- o$show.messages
	show.warnings <- o$show.warnings
	
	with(scale, {
		if (all(is.na(x1))) return(tmapScale_returnNA(n = length(x1), legend = legend, value.na = value.na, label.na = label.na, na.show = na.show))
		
		
		tr = get(paste0("trans_", trans))
		
		xrange = range(x1, na.rm = TRUE)
		
		if (xrange[1] < tr$domain[1]) stop("Values found that are lower than the valid domain", call. = FALSE)
		if (xrange[2] > tr$domain[2]) stop("Values found that are higher than the valid domain", call. = FALSE)
		
		udiv = identical(use_div(brks = NULL, midpoint), TRUE)

		ticks.specified = !is.null(ticks)
		limits.specified = !is.null(limits)
		
		if (!limits.specified) {
			limits = range(x1, na.rm = TRUE)
			if (ticks.specified) limits = range(c(limits, ticks))
		}

		if (limits[1] < tr$domain[1]) stop("Lower limit too low", call. = FALSE)
		if (limits[2] > tr$domain[2]) stop("Upper limit too high", call. = FALSE)
		
		if (ticks.specified) {
			if (limits.specified) {
				if (any(ticks < limits[1])) stop("(Some) ticks are lower than the lowest limit. Please remove these ticks or adjust the lower limit.", call. = FALSE)
				if (any(ticks > limits[2])) stop("(Some) ticks are higher than the upper limit. Please remove these ticks or adjust the upper limit.", call. = FALSE)
			}
			n = length(ticks) - 1
			ticks_t = tr$fun(ticks)
		}
		
		x_t = tr$fun(x1)
		limits_t = tr$fun(limits)
		domain_t = tr$fun(tr$domain)	
		
		breaks = cont_breaks(limits_t, n=101)
		
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
		q = num2breaks(x = x_t, n = 101, style = "fixed", breaks=breaks, approx=TRUE, interval.closure = "left", var=paste(layer, aes, sep = "-"), args = list())
		
		breaks = q$brks
		nbrks = length(breaks)
		n2 = nbrks - 1
		
		int.closure <- attr(q, "intervalClosure")
		
		# update range if NA (automatic)
		if (is.na(values.range[1])) {
			fun_range = paste0("tmapValuesRange_", aes)
			values.range = do.call(fun_range, args = list(x = values, n = n, isdiv = udiv))
		}
		if (length(values.range) == 1) values.range = c(0, values.range)
		
		
		fun_check = paste0("tmapValuesCheck_", aes)
		
		are_valid = do.call(fun_check, args = list(x = values))
		if (!are_valid) stop("Incorrect values for layer ", layer, ", aesthetic ", aes, "; values should conform aes ", aes, call. = FALSE)
		
		fun_isdiv = paste0("tmapValuesIsDiv_", aes)
		
		isdiv = !is.null(midpoint) || do.call(fun_isdiv, args = list(x = values))

		# determine midpoint
		if ((is.null(midpoint) || is.na(midpoint)) && isdiv) {
			rng <- range(x_t, na.rm = TRUE)
			if (rng[1] < 0 && rng[2] > 0 && is.null(midpoint)) {
				
				if (show.messages) message("Variable(s) \"", paste(aes, collapse = "\", \""), "\" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full range of visual values.")
				midpoint <- 0
			} else {
				if ((n2 %% 2) == 1) {
					# number of classes is odd, so take middle class (average of those breaks)
					midpoint <- mean.default(breaks[c((n2+1) / 2, (n2+3) / 2)])
				} else {
					midpoint <- breaks[(n2+2) / 2]
				}
			}
		}
		
		fun_getVV = paste0("tmapValuesVV_", aes)
		VV = do.call(fun_getVV, list(x = values, value.na = value.na, isdiv = isdiv, n = n2, dvalues = breaks, midpoint = midpoint, range = values.range, scale = values.scale * o$scale, are_breaks = TRUE, rep = values.repeat, o = o))
		
		vvalues = VV$vvalues
		value.na = VV$value.na
		if (is.na(value.neutral)) value.neutral = VV$value.neutral
		
		
		ids = classInt::findCols(q)
		vals = vvalues[ids]
		isna = is.na(vals)
		anyNA = any(isna)
		
		if (is.na(na.show)) na.show = anyNA
		
		if (is.null(sortRev)) {
			ids = NULL
		} else if (is.na(sortRev)) {
			ids[] = 1L
		} else if (sortRev) {
			ids = (as.integer(n2) + 1L) - ids
		}

		if (anyNA) {
			vals[isna] = value.na
			if (!is.null(sortRev)) ids[isna] = 0L
		}
		
		b_t = if (ticks.specified) {
			ticks_t
		} else {
#			tr$rev(pretty(limits_t, n = 10))
			# TODO
			#pretty()
			pretty(limits_t)
		}
		b_t = b_t[b_t>=limits_t[1] & b_t<=limits_t[2]]

		nbrks_cont <- length(b_t)
		id = as.integer(cut(b_t, breaks=breaks, include.lowest = TRUE))

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
		
		
		# if (is.log) {
		# 	breaks = 10^breaks
		# 	b = 10^b
		# }
		b = tr$rev(b_t)
		
		
		# create legend values
		values = b
		
		# create legend labels for continuous cases
		if (is.null(labels)) {
			labels = do.call("fancy_breaks", c(list(vec=b, as.count = FALSE, intervals=FALSE, interval.closure=int.closure), label.format)) 	
		} else {
			labels = rep(labels, length.out=nbrks_cont)
			attr(labels, "align") <- label.format$text.align
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
		
		legend = within(legend, {
			nitems = length(labels)
			labels = labels
			dvalues = values
			vvalues = vvalues
			vneutral = value.neutral
			na.show = na.show
		})
		
		format_aes_results(vals, ids, legend)
		
	})
}
