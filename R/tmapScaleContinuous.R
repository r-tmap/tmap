tmapScaleContinuous = function(x1, scale, legend, o, aes, layer, sortRev, bypass_ord) {
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
	
	if (attr(cls, "unique") && is.null(scale$limits) && is.null(scale$ticks)) stop("Unique value, so cannot determine continuous scale range. Please specify limits and/or ticks.", call. = FALSE)
	
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
		
		if (limits.specified) {
			x1_low = x1 < limits[1]
			x1_high = x1 > limits[2]
			if (any(x1_low, na.rm = TRUE)) {
				if (!outliers.trunc[1]) message("Values have been found that are lower than the lowest limit. These 'outliers' have been set to NA. Use outliers.trunc to truncate them to the lower limit")
				x1[which(x1_low)] = if (outliers.trunc[1]) limits[1] else NA
			}
			if (any(x1_high, na.rm = TRUE)) {
				if (!outliers.trunc[2]) message("Values have been found that are higher than the upper limit. These 'outliers' have been set to NA. Use outliers.trunc to truncate them to the upper limit")
				x1[which(x1_high)] = if (outliers.trunc[2]) limits[2] else NA
			}
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
		if (length(values.range) == 1 && !is.na(values.range[1])) values.range = c(0, values.range)
		
		
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

		sfun = paste0("tmapValuesScale_", aes)
		cfun = paste0("tmapValuesColorize_", aes)
		if (is.na(value.neutral)) value.neutral = VV$value.neutral else value.neutral = do.call(sfun, list(x = do.call(cfun, list(x = value.neutral, pc = o$pc)), scale = values.scale))
		
		
		ids = classInt::findCols(q)
		vals = vvalues[ids]
		isna = is.na(vals)
		anyNA = any(isna)
		
		na.show = update_na.show(label.show, legend$na.show, anyNA)
		
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
		
		if (ticks.specified) {
			b_t = ticks_t
			b = tr$rev(b_t)
		} else {
#			tr$rev(pretty(limits_t, n = 10))
			# TODO
			#pretty()
			#tr$fun(round(tr$rev(pretty(limits_t)),1))
			b  = prettyTicks(tr$rev(seq(limits_t[1], limits_t[2], length.out = n)))
			b_t = tr$fun(b)
		}
		sel = if (length(b_t) == 2) TRUE else (b_t>=limits_t[1] & b_t<=limits_t[2])
		b_t = b_t[sel]
		b = b[sel]

		nbrks_cont <- length(b_t)
		id = as.integer(cut(b_t, breaks=breaks, include.lowest = TRUE))

		id_step = id[-1] - head(id, -1)
		id_step = c(id_step[1], id_step, id_step[length(id_step)])
		id_lst = mapply(function(i, s1, s2){
			#res = round(seq(i-floor(id_step/2), i+ceiling(id_step/2), length.out=11))[1:10]
			res1 = round(seq(i-floor(s1/2), i, length.out=6))
			res2 = round(seq(i, i+ceiling(s2/2), length.out=6))[2:5]
			res = c(res1, res2)
			res[res<1 | res>101] = NA
			res
		}, id, head(id_step, -1), id_step[-1], SIMPLIFY = FALSE)
		vvalues = lapply(id_lst, function(i) {
			if (legend$reverse) rev(vvalues[i]) else vvalues[i]
		})
		
		if (legend$reverse) vvalues = rev(vvalues)
		
		if (na.show) vvalues = c(vvalues, value.na)
		
		# temporarily stack gradient values
		vvalues = cont_collapse(vvalues)
		
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
			na.show = get("na.show", envir = parent.env(environment()))
		})
		
		if (bypass_ord) {
			format_aes_results(vals, legend = legend)
		} else {
			format_aes_results(vals, ids, legend)			
		}
		
	})
}
