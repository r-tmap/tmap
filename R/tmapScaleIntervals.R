#' @export
#' @rdname tmap_internal
tmapScaleIntervals = function(x1, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {
	cls = data_class(x1, midpoint_enabled = !is.null(scale$midpoint))
	maincls = class(scale)[1]

	if (attr(cls, "unique") && is.null(scale$breaks)) stop("Unique value, so cannot determine intervals scale range. Please specify breaks.", call. = FALSE)

	if (!(cls[1] %in% c("num", "datetime", "date"))) {
		if (!is.factor(x1)) x1 = as.factor(x1)
		x1 = as.integer(x1)
		# cls = c("num", "int", "seq")
		warning(maincls, " is supposed to be applied to numerical or date/time data", call. = FALSE)
	}

	x1 = without_units(x1)

	if (aes %in% c("pattern")) stop("tm_scale_intervals cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)

	scale = get_scale_defaults(scale, o, aes, layer, cls)

	show.messages <- o$show.messages
	show.warnings <- o$show.warnings


	fun = paste0("interval_", cls[1])

	# pre-checks
	midpoint = NULL
	scale = within(scale, {
		allna = all(is.na(x1))
		if (anyDuplicated(breaks)) stop("breaks specified in the ", aes,  ".scale scaling function contains duplicates.", call. = FALSE)
		if (!is.null(breaks) && length(breaks) < 2 && cls[1] == "num") stop("breaks should contain at least 2 numbers", call. = FALSE)

		udiv = identical(use_div(breaks, midpoint), TRUE)

		if (allna && is.null(breaks)) {
			if (show.messages) message("Variable(s) \"", paste(aes, collapse = "\", \""), "\" only contains NAs. Legend disabled for tm_scale_intervals, unless breaks are specified")
			chart = within(chart, {
				labels = label.na
				vvalues = c(value.na, value.na)
				breaks = c(0, 1)
				na.show = TRUE
				x1 = x1[1]
			})
			return = tmapScale_returnNA(n = length(x1), legend = legend, chart = chart, value.na = value.na, label.na = label.na, label.show = label.show, na.show = legend$na.show, sortRev = sortRev, bypass_ord = bypass_ord)
		}
	})

	if (!is.null(scale$return)) return(scale$return)

	# data-type dependent processing
	scale = do.call(fun, list(scale = scale, x1 = x1, aes = aes, layer = layer, show.messages = show.messages, show.warnings = show.warnings))

	# visual variable processing
	with(scale, {
		fun_getVV = paste0("tmapValuesVV_", aes)
		VV = do.call(fun_getVV, list(x = values, value.na = value.na, isdiv = isdiv, n = n, dvalues = breaks, midpoint = midpoint, range = values.range, scale = values.scale * o$scale, are_breaks = TRUE, rep = values.repeat, o = o))

		vvalues = VV$vvalues
		value.na = VV$value.na

		sfun = paste0("tmapValuesScale_", aes)
		cfun = paste0("tmapValuesColorize_", aes)
		if (is.na(value.neutral)) value.neutral = VV$value.neutral else value.neutral = do.call(sfun, list(x = do.call(cfun, list(x = value.neutral, pc = o$pc)), scale = values.scale))

		# check for size[1] = 0
		if (aes %in% c("size", "lwd")) {
			if (vvalues[1] == 0) {
				message_scale_interval_value0(aes, values, layer)
			}
		}

		mfun = paste0("tmapValuesSubmit_", aes)
		vvalues = do.call(mfun, list(x = vvalues, args = layer_args))
		value.na = do.call(mfun, list(x = value.na, args = layer_args))
		value.neutral = do.call(mfun, list(x = value.neutral, args = layer_args))


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
			ids = (as.integer(n) + 1L) - ids
		}

		if (anyNA) {
			vals[isna] = value.na
			if (!is.null(sortRev)) ids[isna] = 0L
		}

		# detransform log
		if (is.log) {
			if (any((breaks %% 1) != 0)) message("non-rounded breaks occur, because style = \"log10_pretty\" is designed for large values")
			breaks <- 10^breaks
		}

		# create legend values
		#values = breaks[-nbrks]

		if (is.null(labels)) {
			labels = do.call("fancy_breaks", c(list(vec=breaks, as.count = as.count, intervals=TRUE, interval.closure=int.closure), label.format))
		} else {
			if (length(labels)!=nbrks-1 && show.warnings) warning("number of legend labels should be ", nbrks-1, call. = FALSE)
			labels = rep(labels, length.out=nbrks-1)
			attr(labels, "align") <- label.format$text.align
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

		legend = within(legend, {
			nitems = length(labels)
			labels = labels
			dvalues = values
			vvalues = vvalues
			vneutral = value.neutral
			na.show = get("na.show", envir = parent.env(environment()))
			scale = "intervals"
			layer_args = layer_args
		})

		chartFun = paste0("tmapChart", toTitleCase(chart$summary))

		chartArgs = list(chart,
						 bin_colors = vvalues,
						 breaks_def = breaks,
						 na.show = na.show,
						 x1 = x1)

		# if breaks are user specified via tm_chart, use them
		if ("breaks" %in% names(chart) && !is.null(chart$breaks)) {
			chartArgs["bin_colors"] = list(NULL)
			chartArgs$breaks_def = chart$breaks
		}

		chart = do.call(chartFun, chartArgs)

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

interval_num = function(scale, x1, aes, layer, show.messages, show.warnings) {
	within(scale, {
		local({
			styles = c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", "box", "log10", "log10_pretty")
			fn_call <-  call(paste0("tm_", layer[1]))
			if (!(style %in% styles)) {
				cli::cli_abort("Invalid style. Style should be one of {.str {styles}}",
					call = fn_call
				)
			}
		})


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
		if (is.log && !attr(label.format, "big.num.abbr.set")) label.format$big.num.abbr = NA

		if (style == "log10_pretty") {
			x1 = log10(x1)
			style = "fixed"
			breaks = seq(floor(min(x1, na.rm = TRUE)), ceiling(max(x1, na.rm=TRUE)))
		} else if (as.count && style == "pretty") {
			breaks = prettyCount(x1, n=n)
			style <- "fixed"
		} else if (as.count && style == "fixed") {
			breaks[length(breaks)] = breaks[length(breaks)] + 1L
		}

		q = num2breaks(x=x1, n=n, style=style, breaks=breaks, interval.closure=interval.closure, var=paste(layer, aes, sep = "-"), as.count = as.count, args = style.args)

		breaks = q$brks
		nbrks = length(breaks)
		n = nbrks - 1

		int.closure <- attr(q, "intervalClosure")

		# update range if NA (automatic)
		if (is.na(values.range[1])) {
			fun_range = paste0("tmapValuesRange_", aes)
			values.range = do.call(fun_range, args = list(x = values, n = n, isdiv = udiv))
		}
		if (length(values.range) == 1 && !is.na(values.range[1])) values.range = c(0, values.range)

		check_values(layer, aes, values)

		fun_isdiv = paste0("tmapValuesIsDiv_", aes)

		isdiv = !is.null(midpoint) || do.call(fun_isdiv, args = list(x = values))

		# determine midpoint
		if ((is.null(midpoint) || is.na(midpoint)) && isdiv) {
			rng <- range(x1, na.rm = TRUE)
			if (rng[1] < 0 && rng[2] > 0 && is.null(midpoint)) {

				if (show.messages) message(paste0("[scale] tm_", layer[1], ":() the data variable assigned to '", aes, "' contains positive and negative values, so midpoint is set to 0. Set 'midpoint = NA' in 'fill.scale = tm_scale_intervals(<HERE>)' to use all visual values (e.g. colors)"))
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
	})
}

interval_date = function(scale, x1, aes, layer) {
	interval_datetime(scale, x1, aes, layer)
}

interval_datetime = function(scale, x1, aes, layer, show.messages, show.warnings) {
	style = NULL
	interval.closure = NULL
	udiv = NULL
	show.messages = NULL
	within(scale, {
		local({
			styles = c("fixed", "pretty")
			fn_call <-  call(paste0("tm_", layer[1]))
			if (!(style %in% styles)) {
				cli::cli_abort("Invalid style. For date/time data variables, style should be one of {.str {styles}}",
							   call = fn_call
				)
			}
		})


		if (identical(as.count, TRUE) && show.warnings) {
			warning("as.count not applicable for data/time data variables", call. = FALSE)
			as.count = FALSE
		}

		#if (style == "pretty") {
		if (is.null(breaks)) {
			breaks = pretty_datetime_seq(x=x1, n = n)
		} else if (length(breaks) == 1L) {
			breaks = pretty_datetime_seq3(x=x1, by = breaks)
		}


		q <- list(var=x1,
				  brks=breaks)
		attr(q, "style") <- "fixed"
		attr(q, "nobs") <- nobs
		attr(q, "intervalClosure") <- interval.closure
		class(q) <- "classIntervals"

		breaks = q$brks
		nbrks = length(breaks)
		n = nbrks - 1

		is.log = FALSE

		int.closure <- attr(q, "intervalClosure")

		# update range if NA (automatic)
		if (is.na(values.range[1])) {
			fun_range = paste0("tmapValuesRange_", aes)
			values.range = do.call(fun_range, args = list(x = values, n = n, isdiv = udiv))
		}
		if (length(values.range) == 1 && !is.na(values.range[1])) values.range = c(0, values.range)

		check_values(layer, aes, values)

		fun_isdiv = paste0("tmapValuesIsDiv_", aes)

		isdiv = !is.null(midpoint) || do.call(fun_isdiv, args = list(x = values))

		# determine midpoint
		if ((is.null(midpoint) || is.na(midpoint)) && isdiv) {
			rng <- range(x1, na.rm = TRUE)

			if ((n %% 2) == 1) {
				# number of classes is odd, so take middle class (average of those breaks)
				midpoint <- mean(breaks[c((n+1) / 2, (n+3) / 2)])
			} else {
				midpoint <- breaks[(n+2) / 2]
			}

		}
	})

}
