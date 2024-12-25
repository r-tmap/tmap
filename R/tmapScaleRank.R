#' @export
#' @rdname tmap_internal
tmapScaleRank = function(x1, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {
	# update misc argument from tmap option scale.misc.args
	scale = update_scale_args("rank", scale, aes, o)

	cls = data_class(x1, midpoint_enabled = !is.null(scale$midpoint))
	maincls = class(scale)[1]

	if (cls[1] != "num") {
		if (!is.factor(x1)) x1 = as.factor(x1)
		x1 = as.integer(x1)
		warning(maincls, " is supposed to be applied to numerical data", call. = FALSE)
	}

	x1 = rank(without_units(x1))

	if (aes %in% c("lty", "shape", "pattern")) stop("tm_scale_rank cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)

	scale = get_scale_defaults(scale, o, aes, layer, cls)

	show.messages <- o$show.messages
	show.warnings <- o$show.warnings

	with(scale, {
		if (all(is.na(x1))) return(tmapScale_returnNA(n = length(x1), legend = legend, chart = chart, value.na = value.na, label.na = label.na, label.show = label.show, na.show = legend$na.show, sortRev = sortRev, bypass_ord = bypass_ord))


		ticks.specified = !is.null(ticks)

		if (ticks.specified) {
			n = length(ticks) - 1
		}

		limits = range(x1)
		breaks = cont_breaks(limits, n=o$continuous.nclasses)

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
		q = num2breaks(x = x1, n = o$continuous.nclasses, style = "fixed", breaks=breaks, approx=TRUE, interval.closure = "left", var=paste(layer, aes, sep = "-"), args = list())

		breaks = q$brks
		nbrks = length(breaks)
		n2 = nbrks - 1

		int.closure <- attr(q, "intervalClosure")

		# update range if NA (automatic)
		if (is.na(values.range[1])) {
			fun_range = paste0("tmapValuesRange_", aes)
			values.range = do.call(fun_range, args = list(x = values, n = n, isdiv = FALSE))
		}
		if (length(values.range) == 1 && !is.na(values.range[1])) values.range = c(0, values.range)


		check_values(layer, aes, values)

		fun_isdiv = paste0("tmapValuesIsDiv_", aes)


		fun_getVV = paste0("tmapValuesVV_", aes)
		VV = do.call(fun_getVV, list(x = values, value.na = value.na, isdiv = FALSE, n = n2, dvalues = breaks, range = values.range, scale = values.scale * o$scale, are_breaks = TRUE, rep = values.repeat, o = o))

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
			b = ticks
		} else {
			b  = prettyTicks(seq(limits[1], limits[2], length.out = n))
			if (!(aes %in% c("col", "fill"))) b = b[b!=0]
		}
		sel = if (length(b) == 2) TRUE else (b>=limits[1] & b<=limits[2])
		b = b[sel]

		nbrks_cont <- length(b)
		id = as.integer(cut(b, breaks=breaks, include.lowest = TRUE))

		id_step = id[-1] - head(id, -1)
		id_step = c(id_step[1], id_step, id_step[length(id_step)])
		id_lst = mapply(function(i, s1, s2){
			#res = round(seq(i-floor(id_step/2), i+ceiling(id_step/2), length.out=11))[1:10]
			res1 = round(seq(i-floor(s1/2), i, length.out=6))
			res2 = round(seq(i, i+ceiling(s2/2), length.out=6))[2:5]
			res = c(res1, res2)
			res[res<1 | res>o$continuous.nclasses] = NA
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



		legend = within(legend, {
			nitems = length(labels)
			labels = labels
			dvalues = values
			vvalues = vvalues
			vneutral = value.neutral
			na.show = get("na.show", envir = parent.env(environment()))
			scale = "rank"
			tr = trans_identity
			limits = limits
		})
		# NOTE: tr and limits are included in the output to facilitate the transformation of the leaflet continuous legend ticks (https://github.com/rstudio/leaflet/issues/665)
		chartFun = paste0("tmapChart", toTitleCase(chart$summary))

		chart = do.call(chartFun, list(chart,
									   bin_colors = NULL,
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
