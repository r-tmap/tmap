tmapScaleCategorical = function(x1, scale, legend, o, aes, layer, sortRev) {
	cls = if (inherits(scale, "tm_scale_categorical")) c("fact", "unord") else c("fact", "ord")
	
	
	scale = get_scale_defaults(scale, o, aes, layer, cls)
	
	show.messages <- o$show.messages
	show.warnings <- o$show.warnings
	
	with(scale, {

		nms = names(values) #color_names
		
		# cast to factor if needed
		if (!is.factor(x1)) {
			su = sort(unique(x1))
			
			x1 = factor(x1, levels=su)
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
			if (length(values) == nlevels(x1)) {
				values = values[matching]
				if (!is.null(nms)) nms = nms[matching]
			}
			if (!is.null(labels) && (length(labels) == nlevels(x1))) {
				labels = labels[matching]
			}
			x1 = y
		}
		
		
		
		
		# combine levels
		lvls = levels(x1)
		
		n = nlevels(x1)
		if (n.max < n) {
			if (show.warnings) warning("Number of levels of the variable assigned to the aesthetic \"",aes ,"\" of the layer \"", layer, "\" is ", n, ", which is larger than n.max (which is ", n.max, "), so levels are combined.", call. = FALSE)
			
			mapping = as.numeric(cut(seq.int(n), breaks=n.max))
			to = c(which(mapping[-n] - mapping[-1]!=0), n)
			from = c(0, to[-n.max]) + 1
			
			new_lvls = paste0(lvls[from], "...", lvls[to])
			
			x1 = factor(mapping[as.integer(x1)], levels=1L:n.max, labels=new_lvls)
		}
		n = nlevels(x1)
		
		# update range if NA (automatic)
		if (is.na(values.range[1])) {
			fun_range = paste0("tmapValuesRange_", aes)
			values.range = do.call(fun_range, args = list(x = values, n = n, isdiv = FALSE))
		}
		if (length(values.range) == 1) values.range = c(0, values.range)
		
		fun_getCVV = paste0("tmapValuesCVV_", aes)
		VV = do.call(fun_getCVV, list(x = values, value.na = value.na, n = n, range = values.range, scale = values.scale, rep = values.repeat, o = o))
	
		values = VV$vvalues
		value.na = VV$value.na
		
		if (is.na(value.neutral)) value.neutral = VV$value.neutral
		
		
		# legend.palette <- do.call("process_color", c(list(col=legend.palette), process.colors))
		# colorNA <- do.call("process_color", c(list(col=colorNA), process.colors))
		
		ids = as.integer(x1)
		vals = values[ids]
		isna = is.na(vals)
		anyNA = any(isna)
		
		if (is.na(na.show)) na.show = anyNA
		
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
			labs = levels(x1)	
		} else {
			labs <- rep(labels, length.out = n)
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
		
		legend = within(legend, {
			nitems = length(labs)
			labels = labs
			dvalues = vals
			vvalues = values
			vneutral = value.neutral
			na.show = na.show
		})
		

		format_aes_results(vals, ids, legend)
	})	
}
