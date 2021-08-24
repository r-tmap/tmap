tmapScaleCategorical = function(x1, scale, legend, opt, aes, layer, p) {
	cls = if (inherits(scale, "tm_scale_categorical")) c("fact", "unord") else c("fact", "ord")
	
	show.warnings = opt$show.warnings
	
	#cls_o = data_class(x1)
	#cls_t = if ("num" %in% cls_o) c("fact", "ord") else cls_o
	
	# 	
	if (is.na(scale$values[1])) {
		scale$values = getAesOption("values.var", opt, p, layer, cls = cls) 
	}
	if (is.na(scale$value.na)) scale$value.na = getAesOption("value.na", opt, p, layer, cls = cls)
	if (is.na(scale$value.null)) scale$value.null = getAesOption("value.null", opt, p, layer, cls = cls)
	
	values.contrast = if (is.na(scale$values.contrast[1])) getAesOption("values.contrast", opt, p, layer, cls = cls) else scale$values.contrast
	
	
	nms = names(scale$values) #color_names
	
	# cast to factor if needed
	if (!is.factor(x1)) {
		su = sort(unique(x1))
		
		x1 = factor(x1, levels=su)
		if (is.numeric(su)) levels(x1) <- do.call("fancy_breaks", c(list(vec=su, intervals=FALSE), legend$format)) 	
	}
	
	# select levels
	if (!is.null(scale$levels)) {
		x1 = factor(x1, levels = scale$levels)
	}
	
	# drop levels
	if (scale$levels.drop) {
		y = droplevels(x1)
		matching = match(levels(y), levels(x1))
		if (length(scale$values) == nlevels(x1)) {
			scale$values = scale$values[matching]
			if (!is.null(nms)) nms = nms[matching]
		}
		if (!is.null(scale$labels) && (length(scale$labels) == nlevels(x1))) {
			scale$labels = scale$labels[matching]
		}
		x1 = y
	}
	
	
	
	
	# combine levels
	lvls = levels(x1)
	
	nCol = nlevels(x1)
	if (scale$n.max < nCol) {
		if (show.warnings) warning("Number of levels of the variable assigned to the aesthetic \"",aes ,"\" of the layer \"", layer, "\" is ", nCol, ", which is larger than n.max (which is ", scale$n.max, "), so levels are combined.", call. = FALSE)
		
		mapping = as.numeric(cut(seq.int(nCol), breaks=scale$n.max))
		to = c(which(mapping[-nCol] - mapping[-1]!=0), nCol)
		from = c(0, to[-scale$n.max]) + 1
		
		new_lvls = paste0(lvls[from], "...", lvls[to])
		
		x1 = factor(mapping[as.integer(x1)], levels=1L:scale$n.max, labels=new_lvls)
	}
	nCol = nlevels(x1)
	
	
	
	
	# process values
	palid = tmapPalId(scale$values[1])
	
	arecolors = if (is.na(palid)) {
		valid_colors(scale$values[1])
	} else TRUE
	
	arenumbers = !arecolors && is.numeric(scale$values)
	
	if (arecolors) {
		values = if (!is.na(palid)) {
			tmap_get_palette(scale$values, nCol, rep = scale$values.repeat, contrast = values.contrast)
		} else if (!scale$values.repeat && (length(scale$values) < nCol)) {
			grDevices::colorRampPalette(scale$values)(nCol)	
		} else {
			rep(scale$values, length.out=nCol)
		}
	} else if (arenumbers) {
		values = if (length(scale$values) == 2) seq(scale$values[1], scale$values[2], length.out = nCol) else rep(scale$values, length.out = nCol)
	} else {
		values = rep(scale$values, length.out = nCol)
	}
	
	
	
	# legend.palette <- do.call("process_color", c(list(col=legend.palette), process.colors))
	# colorNA <- do.call("process_color", c(list(col=colorNA), process.colors))
	
	v1 = values[as.integer(x1)]
	isNA = is.na(v1)
	
	if (is.null(scale$labels)) {
		labs = levels(x1)	
	} else {
		labs <- rep(scale$labels, length.out = nCol)
	}
	
	if (is.na(scale$label.na)) {
		scale$label.na = if (any(isNA)) "Missing" else ""	
	}
	na.show = (scale$label.na != "")
	
	if (any(isNA)) {
		v1[isNA] = scale$value.na
	}
	
	if (legend$reverse) {
		labs = rev(labs)
		values = rev(values)
	}
	
	if (na.show) {
		labs = c(labs, scale$label.na)
		values = c(values, scale$value.na)
	}
	attr(labs, "align") = legend$format$text.align
	
	
	if (is.na(scale$value.neutral)) scale$value.neutral = getAesOption("value.neutral", opt, p, layer, cls = cls)
	if (is.na(scale$value.neutral)) {
		neutralID = 1 # if (palette.type=="div") round(((length(colsLeg$legend.palette)-1)/2)+1) else 1
		scale$value.neutral = values[1]
	}
	
	
	legend = list(title = legend$title, 
				  nitems = length(labs),
				  labels = labs, 
				  dvalues = v1, 
				  vvalues = values,
				  vneutral = scale$value.neutral,
				  na.show = na.show,
				  setup = legend)
	
	
	format_aes_results(v1, legend)
	
}
