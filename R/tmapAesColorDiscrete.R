tmapAesColor = function(x1, setup, legend, opt) {
	if (!is.numeric(x1) && !is.factor(x1)) x1 = factor(x1)
	
	isNA = all(is.na(x1)) 
	isNUM = is.numeric(x1)
	
	if (isNUM) {
		if (isNA) {
			setup$style = "cat"
		} else {
			rng = range(x1, na.rm = TRUE)
			if (abs(rng[2] - rng[1]) < 1e-9 && rng[1] != rng[2]) {
				if (opt$show.warnings) warning("The value range of the variable \"", "\" is less than 1e-9", call. = FALSE)
				x1[!is.na(x1)] <- round(rng[1], 9)
			}
		}
	} else if (isNA) {
		setup$style = "cat"
	}
	
	if (length(na.omit(unique(x1)))==1 && setup$style!="fixed") setup$style = "cat"
	
	if (is.factor(x1) || setup$style=="cat") {
		if (is.list(setup$palette)) {
			palette.type <- ifelse(is.ordered(x1) || (isNUM), "seq", "cat")
			palette <- setup$palette[[palette.type]] 
		} else if (setup$palette[1] %in% c("seq", "div", "cat")) {
			palette.type <- setup$palette[1]
			palette <- opt$aes.palette[[palette.type]]
		} else {
			palette <- setup$palette
			palette.type <- palette_type(palette)
		}
		colsLeg <- cat2pal(x1,
						   var = "g$col",
						   palette = palette,
						   drop.levels = setup$drop.levels,
						   stretch.palette = setup$stretch.palette,
						   contrast = setup$contrast,
						   colorNA = setup$colorNA,
						   colorNULL=setup$colorNULL,
						   legend.labels=setup$labels,
						   max_levels=opt$max.categories,
						   legend.NA.text = setup$textNA,
						   showNA = setup$showNA,
						   process.colors=c(list(alpha=opt$alpha), opt$pc),
						   legend.format=legend$format,
						   reverse=legend$reverse)
		breaks <- NA
		
		
		
		neutralID <- if (palette.type=="div") round(((length(colsLeg$legend.palette)-1)/2)+1) else 1
		col.neutral <- colsLeg$legend.palette[1]
	} else {
		is.diverging <- use_diverging_palette(x1, setup$breaks, setup$midpoint)
		palette = if (is.list(setup$palette)) {
			palette.type = ifelse(is.diverging, "div", "seq")
			palette = setup$palette[[palette.type]] 
		} else if (setup$palette[1] %in% c("seq", "div", "cat")) {
			palette.type = setup$palette[1]
			palette = opt$aes.palette[[palette.type]]
		} else {
			palette <- setup$palette
			#palette.type <- palette_type(palette)
		}
		colsLeg <- num2pal(x1, 
						   var = "g$col",
						   call = NULL,
						   n = setup$n, 
						   style=setup$style, 
						   style.args=setup$style.args,
						   as.count = setup$as.count,
						   breaks=setup$breaks, 
						   interval.closure=setup$interval.closure,
						   palette = palette,
						   midpoint = setup$midpoint, #auto.palette.mapping = setup$auto.palette.mapping,
						   contrast = setup$contrast, legend.labels=setup$labels,
						   colorNA=setup$colorNA, 
						   colorNULL=setup$colorNULL,
						   legend.NA.text = setup$textNA,
						   showNA = setup$showNA,
						   process.colors=c(list(alpha=opt$alpha), opt$pc),
						   legend.format=legend$format,
						   reverse=legend$reverse)
		breaks <- colsLeg$breaks
		#breakspal <- colsLeg$breaks.palette
		col.neutral <- colsLeg$legend.neutral.col
	}
	
	
	cols <- colsLeg$cols
	legend.labels <- colsLeg$legend.labels
	legend.values <- colsLeg$legend.values
	legend.palette <- colsLeg$legend.palette
	
	legend = list(title = legend$title, labels = legend.labels, values = legend.values, palette = legend.palette, col.neutral=col.neutral, breaks=breaks)
	
	format_aes_results(cols, legend)
}



tmapAesColorRGB = function(x1, x2, x3, setup, legend, opt) {
	values = grDevices::rgb(x1, x2, x3, maxColorValue = setup$maxValue)
	
	format_aes_results(values, list())
}

tmapAes2dSize = function(x1, setup, legend, opt) {
	max = if (is.na(setup$max)) max(x1, na.rm = TRUE) else setup$max
	values = x1 / max
	legend = list(max = max)
	
	format_aes_results(values, legend)
}

tmapAesShape = function(x1, legend, setup) {
	if (is.numeric(x1)) x1 = as.integer(x1)
	if (is.character(x1)) x1 = as.factor(x1)
	if (is.factor(x1)) x1 = as.integer(x1)
	#n = length(shapes)
	values = setup$shapes[x1]
	legend = list(shapes = setup$shapes)
	
	format_aes_results(values, legend)
}

