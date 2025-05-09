update_scale_args = function(scaleName, args, aes, o) {
	if (scaleName %in% names(o$scale.misc.args)) {
		def = o$scale.misc.args[[scaleName]]
		for (nam in names(def)) {
			if (is.null(args[[nam]])) args[[nam]] = getAesValue(def[[nam]], aes)
		}
	}
	args
}


# extention of substr: when -1, then cound from tail
substr2 = function(x, start, stop) {
	if (start < 1 || stop < 1) {
		n = nchar(x)
		if (start < 1) {
			start = n + start
		}
		if (stop < 1) {
			stop = n + stop
		}
	}
	substr(x, start, stop)
}


tmapScaleContinuous = function(x1, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {


	# update misc argument from tmap option scale.misc.args
	scale = update_scale_args(substr2(class(scale)[1], 10, 0), scale, aes, o)

	cls = data_class(x1, midpoint_enabled = !is.null(scale$midpoint))
	maincls = class(scale)[1]

	if (attr(cls, "unique") && is.null(scale$limits) && is.null(scale$ticks)) stop("Unique value, so cannot determine continuous scale range. Please specify limits and/or ticks.", call. = FALSE)

	#if (cls[1] == "na") stop("data contain only NAs, so ", maincls, " cannot be applied", call. = FALSE)

	if (cls[1] != "num") {
		if (!is.factor(x1)) x1 = as.factor(x1)
		x1 = as.integer(x1)
		warning(maincls, " is supposed to be applied to numerical data", call. = FALSE)
	}




	x1 = without_units(x1)

	if (aes %in% c("lty", "shape", "pattern")) stop("tm_scale_continuous cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)

	scale = get_scale_defaults(scale, o, aes, layer, cls)

	vnum = (is.numeric(scale$values) || inherits(scale$values, "tmapSeq"))
	#vnum=FALSE

	show.messages <- o$show.messages
	show.warnings <- o$show.warnings

	with(scale, {
		allna = all(is.na(x1))


		# if (all(is.na(x1))) {
		# }


		tr = get(paste0("trans_", trans))

		trargs = formals(tr$fun)
		trargs$x = NULL
		trargs_match = intersect(names(trargs), names(trans.args))
		if (!is.null(trargs_match) && length(trargs_match)) {
			trargs[trargs_match] = trans.args[trargs_match]
		}


		if (!allna) {
			xrange = range(x1, na.rm = TRUE)

			if (xrange[1] < tr$domain[1]) stop("Values found that are lower than the valid domain", call. = FALSE)
			if (xrange[2] > tr$domain[2]) stop("Values found that are higher than the valid domain", call. = FALSE)
		}

		udiv = identical(use_div(brks = NULL, midpoint), TRUE)

		ticks.specified = !is.null(ticks)
		limits.specified = (length(limits) == 2L)

		if (!limits.specified) {
			if (allna) {
				if (show.messages) message("Variable(s) \"", paste(aes, collapse = "\", \""), "\" only contains NAs. Legend disabled for tm_scale_continuous, unless limits are specified")
				chart = within(chart, {
					labels = label.na
					vvalues = c(value.na, value.na)
					breaks = c(0, 1)
					na.show = TRUE
					x1 = x1[1]
				})
				return(tmapScale_returnNA(n = length(x1), legend = legend, chart = chart, value.na = value.na, label.na = label.na, label.show = label.show, na.show = legend$na.show, sortRev = sortRev, bypass_ord = bypass_ord))
			}
			if (is.na(limits)) {
				limits = range(x1, na.rm = TRUE)
			} else {
				if (length(limits) == 1L) limits = range(c(limits, x1), na.rm = TRUE)
				if (limits[1] < tr$domain[1]) limits[1] = tr$domain[1]
				if (limits[2] > tr$domain[2]) limits[2] = tr$domain[2]
			}
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
			ticks_t = do.call(tr$fun, c(list(x = ticks), trargs))
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

		x_t = do.call(tr$fun, c(list(x = x1), trargs))
		limits_t = do.call(tr$fun, c(list(x = limits), trargs))
		domain_t = do.call(tr$fun, c(list(x = tr$domain), trargs))

		if (!vnum) {
			breaks = cont_breaks(limits_t, n=o$continuous.nclasses)
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
			q = num2breaks(x = x_t, n = o$continuous.nclasses, style = "fixed", breaks=breaks, approx=TRUE, interval.closure = "left", var=paste(layer, aes, sep = "-"), args = list())

			breaks = q$brks
			nbrks = length(breaks)
			n2 = nbrks - 1

		}




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
			rng <- range(x_t, na.rm = TRUE)
			if (rng[1] < 0 && rng[2] > 0 && is.null(midpoint)) {

				if (show.messages) message("Variable(s) \"", paste(aes, collapse = "\", \""), "\" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full range of visual values.")
				midpoint <- 0
			} else {
				if (vnum) {
					midpoint = mean(limits_t)
				} else {
					if ((n2 %% 2) == 1) {
						# number of classes is odd, so take middle class (average of those breaks)
						midpoint <- mean.default(breaks[c((n2+1) / 2, (n2+3) / 2)])
					} else {
						midpoint <- breaks[(n2+2) / 2]
					}
				}
			}
		}

		fun_getVV = paste0("tmapValuesVV_", aes)

		if (!vnum) {
			#### discretisize

			# number of visual values in legend item (belonging to one label)
			nvv = o$continuous.nclass_per_legend_break


			VV = do.call(fun_getVV, list(x = values, value.na = value.na, isdiv = isdiv, n = n2, dvalues = breaks, midpoint = midpoint, range = values.range, scale = values.scale * o$scale, are_breaks = TRUE, rep = values.repeat, o = o))

			vv = VV$vvalues
			value.na = VV$value.na

			sfun = paste0("tmapValuesScale_", aes)
			cfun = paste0("tmapValuesColorize_", aes)
			if (is.na(value.neutral)) value.neutral = VV$value.neutral else value.neutral = do.call(sfun, list(x = do.call(cfun, list(x = value.neutral, pc = o$pc)), scale = values.scale))

			ids = classInt::findCols(q)
			vals = vv[ids]
		} else {
			if (is.numeric(values)) {
				values = tm_seq(values[1], values[length(values)], power = "lin")
			}
			VV = transform_values(x_t, limits_t, values.range, values$power, values.scale * o$scale)

			vals = VV$x
			if (is.na(value.neutral)) value.neutral = VV$neutral
		}

		isna = is.na(vals)
		anyNA = any(isna)

		na.show = update_na.show(label.show, legend$na.show, anyNA)

		if (is.null(sortRev)) {
			ids = NULL
		} else if (is.na(sortRev)) {
			ids = rep(1L, length(x1))
		} else if (sortRev) {
			if (vnum) {
				ids = rank(-vals)
			} else {
				ids = (as.integer(n2) + 1L) - ids
			}
		} else if (vnum) {
			ids = rank(vals)
		}

		if (anyNA) {
			vals[isna] = value.na
			if (!is.null(sortRev)) ids[isna] = 0L
		}

		labels_exp = !ticks.specified && trans == "log" && trargs$base != exp(1)


		if (ticks.specified) {
			b_t = ticks_t
			b = do.call(tr$rev, c(list(x = b_t), trargs))
		} else {
			if (labels_exp) {
				b = do.call(tr$rev, c(list(x = pretty(limits_t, n = n)), trargs))
			} else {
				b  = prettyTicks(do.call(tr$rev, c(list(x = seq(limits_t[1], limits_t[2], length.out = n)), trargs)))
			}

			if (!(aes %in% c("col", "fill"))) b = b[b!=0]
			b_t = do.call(tr$fun, c(list(x = b), trargs))
		}
		if (length(b_t) == 2) {
			sel = TRUE
			labels_select = TRUE
		} else {
			# #1039
			# to include min/max if they are closer to the next 'not included' tick than the included tick
			# e.g. tm_shape(World) + tm_polygons("gender", fill.scale = tm_scale_continuous())
			# lowest value is 0.009 whereas ticks are 0.2, 0.4, 0.6, 0.8.
			stp1 = (b_t[2] - b_t[1]) / 2
			stp2 = (b_t[length(b_t)] - b_t[length(b_t) - 1L]) / 2
			limits_t_tmp = c(limits_t[1] - stp1, limits_t[2] + stp2)
			sel = (b_t>=limits_t_tmp[1] & b_t<=limits_t_tmp[2])

			labels_select = ((b_t>=limits_t[1] & b_t<=limits_t[2])[sel])
		}
		b_t = b_t[sel]
		b = b[sel]



		nbrks_cont <- length(b_t)

		if (vnum) {
			vvalues = transform_values(b_t, limits_t, values.range, values$power, values.scale * o$scale, include.neutral = FALSE)
		} else {
			id = as.integer(cut(b_t, breaks=breaks, include.lowest = TRUE))

			if (sum(!is.na(id)) < 2) {
				if (is.null(scale$ticks)) {
					cli::cli_abort("{.field [tm_scale_continuous]} too few tick labels specified. Either increase {.arg n} or specify tick values via {.arg ticks}")
				} else {
					cli::cli_abort("{.field [tm_scale_continuous]} too few tick labels specified via {.arg ticks}")
				}
			}

			# impute missing head and tail (#1039)
			missing_head = is.na(id[1])
			if (missing_head) id[1] = 2 * id[2] - id[3]

			nid = length(id)

			missing_tail = is.na(id[nid])
			if (missing_tail) id[nid] = 2 * id[nid-1] - id[nid-2]

			id_step = id[-1] - head(id, -1)
			id_step = c(id_step[1], id_step, id_step[length(id_step)])
			id_lst = mapply(function(i, s1, s2){
				#res = round(seq(i-floor(id_step/2), i+ceiling(id_step/2), length.out=11))[1:10]
				res1 = round(seq(i-floor(s1/2), i, length.out=(nvv/2)+1L))
				res2 = round(seq(i, i+ceiling(s2/2), length.out=(nvv/2)+1L))[2:(nvv/2)]
				res = c(res1, res2)
				res[res<1 | res>o$continuous.nclasses] = NA
				res
			}, id, head(id_step, -1), id_step[-1], SIMPLIFY = FALSE)
			vvalues = lapply(id_lst, function(i) {
				if (legend$reverse) rev(vv[i]) else vv[i]
			})
		}






		if (legend$reverse) vvalues = rev(vvalues)

		if (na.show) vvalues = c(vvalues, value.na)

		# temporarily stack gradient values
		if (!vnum) vvalues = cont_collapse(vvalues)

		# create legend values
		values = b

		# create legend labels for continuous cases
		if (is.null(labels)) {
			if (labels_exp) {
				labels = paste(trargs$base, b_t, sep = "^")
			} else {
				labels = do.call("fancy_breaks", c(list(vec=b, as.count = FALSE, intervals=FALSE, interval.closure="left"), label.format))
			}
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
			labels_select = c(labels_select, TRUE)
			attr(labels, "align") = labels.align
		}

		legend = within(legend, {
			nitems = length(labels)
			labels = labels
			labels_select = labels_select
			dvalues = values
			vvalues = vvalues
			vneutral = value.neutral
			na.show = get("na.show", envir = parent.env(environment()))
			scale = "continuous"
			tr = tr
			limits = limits
			layer_args = layer_args
		})


		# NOTE: tr and limits are included in the output to facilitate the transformation of the leaflet continuous legend ticks (https://github.com/rstudio/leaflet/issues/665)
		#vvalues_mids = sapply(cont_split(vvalues), "[", nvv/2)
		#vvalues_mids[vvalues_mids == "NA"] = NA


		chartFun = paste0("tmapChart", toTitleCase(chart$summary))

		chart = do.call(chartFun, list(chart,
									   bin_colors = NULL,
									   breaks_def = chart$breaks,
									   na.show = na.show,
									   x1 = x1))



		# chart = within(chart, {
		# 	labels = labels
		# 	vvalues = sapply(cont_split(vvalues), "[", 5)
		# 	breaks_def = legend$dvalues
		# 	na.show = get("na.show", envir = parent.env(environment()))
		# 	x1 = x1
		# })



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
