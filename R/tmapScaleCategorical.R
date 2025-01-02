#' @export
#' @rdname tmap_internal
tmapScaleCategorical = function(x1, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {
	cls = if (inherits(scale, "tm_scale_categorical")) c("fact", "unord") else c("fact", "ord")

	if (is.factor(x1)) {
		defcols_cats = grepl("=<>=", levels(x1)[1], fixed = TRUE)
		defcols_nocats = grepl("=><=", levels(x1)[1], fixed = TRUE)

		if (defcols_cats || defcols_nocats) {
			res = strsplit(levels(x1), {if (defcols_cats) "=<>=" else "=><="}, fixed = TRUE)
			levels(x1) = vapply(res, "[", 1, FUN.VALUE = character(1))
			ct = vapply(res, "[", 2, FUN.VALUE = character(1))

			if (defcols_nocats && !legend$called) {
				legend$show = FALSE
			}
		} else {
			ct = NULL
		}
	} else {
		ct = NULL
	}


	scale = get_scale_defaults(scale, o, aes, layer, cls, ct)

	show.messages <- o$show.messages
	show.warnings <- o$show.warnings


	with(scale, {
		check_values(layer, aes, values)

		nms = getValuesNames(values)

		nv = length(nms)
		if (all(nms == "")) nms = NULL

		# cast to factor if needed
		if (!is.factor(x1)) {
			su = sort(unique(x1))
			x1 = tryCatch({
				factor(x1, levels=su)
			}, error = function(e) {
				stop("tm_scale_categorical in layer \"tm_", layer, "\", visual variable \"", aes, "\" cannot be applied due to an error categorization of the data", call. = FALSE)
			})

			if (is.numeric(su)) levels(x1) <- do.call("fancy_breaks", c(list(vec=su, intervals=FALSE, as.count = FALSE), label.format))
		}

		# select levels
		if (!is.null(levels)) {
			x1 = factor(x1, levels = levels)
		}

		# drop levels
		if (levels.drop) {
			y = droplevels(x1)
			matching = match(levels(y), levels(x1))
			if (length(values) == nlevels(x1) && is.null(nms)) {
				values = values[matching]
			}
			if (!is.null(labels) && (length(labels) == nlevels(x1)) && is.null(names(labels))) {
				labels = labels[matching]
			}
			x1 = y
		}

		lvls = levels(x1)
		n = nlevels(x1)

		if (is.null(labels)) {
			labs = lvls
		} else {
			if (is.null(names(labels))) {
				if (length(labels) != n) warning("labels do not have the same length as levels, so they are repeated", call. = FALSE)
				labs = rep(labels, length.out = n)
			} else {
				nms = names(labels)
				labs = structure(lvls, names = lvls)

				nms2 = intersect(nms, lvls)
				labs[nms2] = unname(labels[nms2])
			}
		}
		names(labs) = NULL

		if (!is.null(nms)) {
			xlev = levels(x1)
			if (!all(xlev %in% nms)) {
				stop("All levels should occur in the vector names of values: ", paste(setdiff(xlev, nms), collapse = ", "), " are missing", call. = FALSE)
			} else {
				values = values[match(xlev, nms)]
			}
		}



		# combine levels
		if (n.max < n) {
			if (show.warnings) warning("Number of levels of the variable assigned to the aesthetic \"",aes ,"\" of the layer \"", layer, "\" is ", n, ", which is larger than n.max (which is ", n.max, "), so levels are combined.", call. = FALSE)

			mapping = as.numeric(cut(seq.int(n), breaks=n.max))
			to = c(which(mapping[-n] - mapping[-1]!=0), n)
			from = c(0, to[-n.max]) + 1

			new_lvls = paste0(labs[from], "...", labs[to])

			x1 = factor(mapping[as.integer(x1)], levels=1L:n.max, labels=new_lvls)
			labs = new_lvls
		}
		n = nlevels(x1)

		# update range if NA (automatic)
		if (is.na(values.range[1])) {
			fun_range = paste0("tmapValuesRange_", aes)
			values.range = do.call(fun_range, args = list(x = values, n = n, isdiv = FALSE))
		}
		if (length(values.range) == 1 && !is.na(values.range[1])) values.range = c(0, values.range)

		fun_getCVV = paste0("tmapValuesCVV_", aes)
		VV = do.call(fun_getCVV, list(x = values, value.na = value.na, n = n, range = values.range, scale = values.scale, rep = values.repeat, o = o))

		values = VV$vvalues
		value.na = VV$value.na

		sfun = paste0("tmapValuesScale_", aes)
		cfun = paste0("tmapValuesColorize_", aes)
		if (is.na(value.neutral)) value.neutral = VV$value.neutral else value.neutral = do.call(sfun, list(x = do.call(cfun, list(x = value.neutral, pc = o$pc)), scale = values.scale))

		mfun = paste0("tmapValuesSubmit_", aes)
		values = do.call(mfun, list(x = values, args = layer_args))
		value.na = do.call(mfun, list(x = value.na, args = layer_args))
		value.neutral = do.call(mfun, list(x = value.neutral, args = layer_args))


		# legend.palette <- do.call("process_color", c(list(col=legend.palette), process.colors))
		# colorNA <- do.call("process_color", c(list(col=colorNA), process.colors))

		ids = as.integer(x1)
		vals = values[ids]

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




		if (legend$reverse) {
			labs = rev(labs)
			values = rev(values)
		}

		if (na.show) {
			labs = c(labs, label.na)
			values = c(values, value.na)
		}
		attr(labs, "align") = label.format$text.align


		# SPECIAL CASE: if icons are used, specify this information in the symbol legend, such that it can be taken (in step4_plot_collect_legends) by other legends (e.g. for symbol sizes)
		icon_scale = if ((aes == "shape") && any(values > 999) && getOption("tmap.mode") == "plot") layer_args$icon.scale else 1

		legend = within(legend, {
			nitems = length(labs)
			labels = labs
			dvalues = vals
			vvalues = values
			vneutral = value.neutral
			icon_scale = icon_scale
			na.show = get("na.show", envir = parent.env(environment()))
			scale = "categorical"
			layer_args = layer_args
		})


		chartFun = paste0("tmapChart", toTitleCase(chart$summary))
		chart = do.call(chartFun, list(chart,
									   bin_colors = values,
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

