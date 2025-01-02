tmapScaleDiscrete = function(x1, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {
	cls = data_class(x1, midpoint_enabled = !is.null(scale$midpoint))
	maincls = class(scale)[1]


	#if (cls[1] == "na") stop("data contain only NAs, so tm_scale_discrete cannot be applied", call. = FALSE)
	#if (cls[1] != "num") stop("tm_scale_discrete can only be used for numeric data", call. = FALSE)

	if (cls[1] != "num") {
		if (!is.factor(x1)) x1 = as.factor(x1)
		x1 = as.integer(x1)
		warning(maincls, " is supposed to be applied to numerical data", call. = FALSE)
	}

	x1 = without_units(x1)

	if (aes %in% c("lty", "shape", "pattern")) stop("tm_scale_discrete cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)

	scale = get_scale_defaults(scale, o, aes, layer, cls)

	show.messages <- o$show.messages
	show.warnings <- o$show.warnings

	if (all(is.na(x1))) return(tmapScale_returnNA(n = length(x1), legend = legend, chart = chart, value.na = value.na, label.na = label.na, label.show = label.show, na.show = legend$na.show, sortRev = sortRev, bypass_ord = bypass_ord))

	with(scale, {

		u = sort(unique(x1))
		#u = na.omit(u)
		rng = range(u)

		if (!is.na(ticks[1])) {
			if (!all(u %in% ticks)) stop("Values have been found for which no ticks have been specified", call. = FALSE)
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

		check_values(layer, aes, values)

		fun_isdiv = paste0("tmapValuesIsDiv_", aes)

		isdiv = !is.null(midpoint) || do.call(fun_isdiv, args = list(x = values))


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

		# update range if NA (automatic)
		if (is.na(values.range[1])) {
			fun_range = paste0("tmapValuesRange_", aes)
			values.range = do.call(fun_range, args = list(x = values, n = n, isdiv = isdiv))
		}
		if (length(values.range) == 1 && !is.na(values.range[1])) values.range = c(0, values.range)


		fun_getVV = paste0("tmapValuesVV_", aes)
		VV = do.call(fun_getVV, list(x = values, value.na = value.na, isdiv = isdiv, n = n, dvalues = ticks, are_breaks = FALSE, midpoint = midpoint, range = values.range, scale = values.scale * o$scale, rep = values.repeat, o = o))

		vvalues = VV$vvalues
		value.na = VV$value.na

		sfun = paste0("tmapValuesScale_", aes)
		cfun = paste0("tmapValuesColorize_", aes)
		if (is.na(value.neutral)) value.neutral = VV$value.neutral else value.neutral = do.call(sfun, list(x = do.call(cfun, list(x = value.neutral, pc = o$pc)), scale = values.scale))

		mfun = paste0("tmapValuesSubmit_", aes)
		vvalues = do.call(mfun, list(x = vvalues, args = layer_args))
		value.na = do.call(mfun, list(x = value.na, args = layer_args))
		value.neutral = do.call(mfun, list(x = value.neutral, args = layer_args))


		ids = match(x1, ticks)
		vals = vvalues[ids]

		isna = is.na(vals)
		anyNA = any(isna)

		na.show = update_na.show(label.show, legend$na.show, anyNA)

		if (is.null(sortRev)) {
			ids = NULL
		} else if (is.na(sortRev)) {
			ids[] = 1L
		} else if (sortRev) {
			ids = (as.integer(n) + 1L) - ids
		}

		if (anyNA) {
			vals[isna] = value.na
			if (!is.null(sortRev)) ids[isna] = 0L
		}


		if (is.null(labels)) {
			labels = do.call("fancy_breaks", c(list(vec=ticks, as.count = FALSE, intervals=FALSE), label.format))
		} else {
			if (length(labels)!=n && show.warnings) warning("number of legend labels should be ", n, call. = FALSE)
			labels = rep(labels, length.out=n)
			attr(labels, "align") <- label.format$text.align
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

		legend = within(legend, {
			nitems = length(labels)
			labels = labels
			dvalues = ticks
			vvalues = vvalues
			vneutral = value.neutral
			na.show = get("na.show", envir = parent.env(environment()))
			scale = "discrete"
			layer_args = layer_args
		})

		chartFun = paste0("tmapChart", toTitleCase(chart$summary))

		chart = do.call(chartFun, list(chart,
									   bin_colors = vvalues,
									   breaks_def = NULL,
									   na.show = na.show,
									   x1 = x1))



		if (submit_legend) {
			if (bypass_ord) {
				format_aes_results(vals, legend = legend, chart = chart)
			} else {
				format_aes_results(vals, ids, legend, chart = chart)
			}
		} else {
			list(vals = vals, ids = ids, legend = legend, chart = chart, bypass_ord = bypass_ord)
		}
	})
}
