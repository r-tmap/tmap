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
	
	type = ifelse(nchar(legend.palette[1]) > 50, "color_cont", "color_cls")
	
	legend = list(title = legend$title, labels = legend.labels, values = legend.values, x = legend.palette, neutral=col.neutral, breaks=breaks, type = type)
	
	legend = list(title = legend$title, 
				  nitems = length(legend.labels),
				  labels = legend.labels, 
				  dvalues = legend.values, 
				  vvalues = legend.palette,
				  vneutral = col.neutral,
				  breaks=breaks, type = type)
	
	
	format_aes_results(cols, legend)
}





tmapAesColorRGB = function(x1, x2, x3, setup, legend, opt) {
	values = grDevices::rgb(x1, x2, x3, maxColorValue = setup$maxValue)
	
	format_aes_results(values, list())
}

tmapAes2dSize = function(x1, setup, legend, opt) {
	if (all(is.na(x1))) {
		size = rep(NA, length(x1))
		legend = list(title = NA, labels = NA, values = NA, sizes = NA)
	}
	
	
	if (!is.na(setup$lim[1])) {
		x1[x1<setup$lim[1]] <- NA
		x1[x1>setup$lim[2]] <- setup$lim[2]
	} else {
		x1[x1==0] <- NA
	}
	
	mx <- max(x1, na.rm=TRUE)
	xmax <- ifelse(is.na(setup$max), mx, setup$max)
	
	if (mx > xmax) {
		s <- sum(x1 > xmax, na.rm = TRUE)
		message("Note that ", s, " values of the variable \"", "VAR", "\" (the highest being ", mx, ") are larger than size.max, which is currently set to ", xmax, ". It is recommended to set size.max to at least ", mx, ". Another option is to set size.lim = c(0, ", xmax, "), which truncates the size of the ", s, " larger symbols. Use the scale argument to increase the size of all symbols.")
	}
	
	if (is.null(setup$sizes.legend)) {
		x_legend <- pretty(c(0, xmax), 5)
		x_legend <- x_legend[x_legend!=0]
		nxl <- length(x_legend)
		if (nxl>5) x_legend <- x_legend[-c(nxl-3, nxl-1)]
	} else {
		x_legend <- setup$sizes.legend
	}
#	symbol.size.legend.values <- x_legend
	
	if (is.null(setup$sizes.legend.labels)) {
		sizes.legend.labels <- do.call("fancy_breaks", c(list(vec=x_legend, intervals=FALSE), legend$format))
	} else {
		if (length(setup$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of symbols in the legend", call. = FALSE)
		sizes.legend.labels <- g$sizes.legend.labels
	}
	
	rescale = TRUE # TODO: what does it do?
	maxX <- ifelse(rescale, xmax, 1)
	scaling <- ifelse(setup$perceptual, 0.5716, 0.5)
	symbol.size <- setup$scale*(x1/maxX)^scaling
	symbol.max.size <- max(symbol.size, na.rm=TRUE)
	symbol.legend.sizes <- setup$scale*(x_legend/maxX)^scaling
	
	if (legend$reverse) {
		symbol.legend.sizes <- rev(symbol.legend.sizes)
		symbol.size.legend.labels <- rev(sizes.legend.labels)
	}
	attr(sizes.legend.labels, "align") <- legend$format$text.align
	print(range(symbol.size))
	
	legend = list(title = legend$title,
				  nitems = length(sizes.legend.labels),
				  labels=sizes.legend.labels,
				  #palette=rep("pink", length(sizes.legend.labels)), #dummy
				  dvalues = x_legend,
				  vvalues = symbol.legend.sizes,
				  vneutral = symbol.max.size,
				  max.size=symbol.max.size)
	
	values = symbol.size

	format_aes_results(values, legend)
}

tmapAesShape = function(x1, legend, setup, opt) {

	if (length(na.omit(unique(x1)))==1 && setup$style != "fixed") setup$style = "cat"
	
	if (is.factor(x1) || setup$style == "cat") {
		shapesLeg <- cat2shape(x,
							   var = "g$shape",
							   shapes=setup$shapes,
							   drop.levels = setup$shapes.drop.levels,
							   legend.labels=setup$shapes.labels,
							   shapeNA = setup$shapeNA,
							   legend.NA.text = setup$shape.textNA,
							   showNA = setup$shape.showNA,
							   legend.format=setup$legend.format,
							   reverse=reverse)
		symbol.shape <- shapesLeg$shps
		shape.legend.labels <- shapesLeg$legend.labels
		shape.legend.values <- shapesLeg$legend.values
		shape.legend.shapes <- shapesLeg$shapes
		shape.neutral <- shape.legend.shapes[1]
	} else {
		shapesLeg <- num2shape(x, 
							   var = "g$shape",
							   n=setup$shapes.n, 
							   style=setup$shapes.style,
							   style.args=setup$shapes.style.args, 
							   breaks=setup$shapes.breaks, 
							   interval.closure=setup$shapes.interval.closure,
							   shapes=setup$shapes,
							   legend.labels = setup$shapes.labels,
							   legend.NA.text = setup$shape.textNA,
							   shapeNA=setup$shapeNA, 
							   showNA = setup$shape.showNA,
							   legend.format=setup$legend.format,
							   reverse=reverse)
		symbol.shape <- shapesLeg$shps
		shape.legend.labels <- shapesLeg$legend.labels
		shape.legend.values <- shapesLeg$legend.values
		shape.legend.shapes <- shapesLeg$shapes
		shape.neutral <- shape.legend.shapes[1]
	}
	

	
	# if (is.numeric(x1)) x1 = as.integer(x1)
	# if (is.character(x1)) x1 = as.factor(x1)
	# if (is.factor(x1)) x1 = as.integer(x1)
	#n = length(shapes)
	values = setup$shapes[x1]
	legend = list(title = legend$title,
				  nitems = length(shape.legend.labels),
				  labels = shape.legend.labels,
				  dvalues = shape.legend.values,
				  vvalues = shape.legend.shapes,
				  vneutral = shape.neutral)
				  

	format_aes_results(values, legend)
}

