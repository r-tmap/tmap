tmapScaleCategorical = function(x1, scale, legend, opt, aes, layer, p) {
	cls = if (inherits(scale, "tm_scale_categorical")) c("fact", "unord") else c("fact", "ord")
	
	show.warnings = opt$show.warnings
	
	values = if (is.na(scale$values[1])) getAesOption("values.var", opt, p, layer, cls = cls) else scale$values
	value.na = if (is.na(scale$value.na) || identical(scale$value.na, TRUE)) getAesOption("value.na", opt, aes, layer, cls = cls) else scale$value.na
	value.null = if (is.na(scale$value.null)) getAesOption("value.null", opt, aes, layer, cls = cls) else scale$value.null
	value.neutral = if (is.na(scale$value.neutral)) getAesOption("value.neutral", opt, aes, layer, cls = cls) else scale$value.neutral
	values.contrast = if (is.na(scale$values.contrast[1])) getAesOption("values.contrast", opt, p, layer, cls = cls) else scale$values.contrast
	
	
	
	nms = names(values) #color_names
	
	# cast to factor if needed
	if (!is.factor(x1)) {
		su = sort(unique(x1))
		
		x1 = factor(x1, levels=su)
		if (is.numeric(su)) levels(x1) <- do.call("fancy_breaks", c(list(vec=su, intervals=FALSE, as.count = FALSE), legend$format)) 	
	}
	
	# select levels
	if (!is.null(scale$levels)) {
		x1 = factor(x1, levels = scale$levels)
	}
	
	# drop levels
	if (scale$levels.drop) {
		y = droplevels(x1)
		matching = match(levels(y), levels(x1))
		if (length(values) == nlevels(x1)) {
			scale$values = values[matching]
			if (!is.null(nms)) nms = nms[matching]
		}
		if (!is.null(scale$labels) && (length(scale$labels) == nlevels(x1))) {
			scale$labels = scale$labels[matching]
		}
		x1 = y
	}
	
	
	
	
	# combine levels
	lvls = levels(x1)
	
	n = nlevels(x1)
	if (scale$n.max < n) {
		if (show.warnings) warning("Number of levels of the variable assigned to the aesthetic \"",aes ,"\" of the layer \"", layer, "\" is ", n, ", which is larger than n.max (which is ", scale$n.max, "), so levels are combined.", call. = FALSE)
		
		mapping = as.numeric(cut(seq.int(n), breaks=scale$n.max))
		to = c(which(mapping[-n] - mapping[-1]!=0), n)
		from = c(0, to[-scale$n.max]) + 1
		
		new_lvls = paste0(lvls[from], "...", lvls[to])
		
		x1 = factor(mapping[as.integer(x1)], levels=1L:scale$n.max, labels=new_lvls)
	}
	n = nlevels(x1)
	
	# update contrast if NA (automatic)
	if (is.na(values.contrast[1])) {
		fun_contrast = paste0("tmapValuesContrast_", p)
		values.contrast = do.call(fun_contrast, args = list(x = values, n = n, isdiv = FALSE))
	}
	if (length(values.contrast) == 1) values.contrast = c(0, values.contrast)
	
	fun_getCVV = paste0("tmapValuesCVV_", p)
	VV = do.call(fun_getCVV, list(x = values, n = n, contrast = values.contrast, rep = scale$values.repeat))

	values = VV$vvalues
	if (is.na(value.neutral)) value.neutral = VV$value.neutral
	
	
	# legend.palette <- do.call("process_color", c(list(col=legend.palette), process.colors))
	# colorNA <- do.call("process_color", c(list(col=colorNA), process.colors))
	
	v1 = values[as.integer(x1)]
	isNA = is.na(v1)
	
	if (is.null(scale$labels)) {
		labs = levels(x1)	
	} else {
		labs <- rep(scale$labels, length.out = n)
	}
	
	if (is.na(scale$label.na)) {
		scale$label.na = if (any(isNA)) "Missing" else ""	
	}
	na.show = (scale$label.na != "")
	
	if (any(isNA)) {
		v1[isNA] = value.na
	}
	
	if (legend$reverse) {
		labs = rev(labs)
		values = rev(values)
	}
	
	if (na.show) {
		labs = c(labs, scale$label.na)
		values = c(values, value.na)
	}
	attr(labs, "align") = legend$format$text.align
	

	legend = list(title = legend$title, 
				  nitems = length(labs),
				  labels = labs, 
				  dvalues = v1, 
				  vvalues = values,
				  vneutral = value.neutral,
				  na.show = na.show,
				  setup = legend)
	
	
	format_aes_results(v1, legend)
	
}
