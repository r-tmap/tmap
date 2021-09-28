#' @export
tm_mv = function(...) {
	list(c(...))
}

tmapVars = function(x) {
	if (inherits(x, "tmapOption")) return(x)
	isL = is.list(x)
	if (!isL) x = as.list(x)
	structure(x, class = "tmapVars")
}
format_aes_results = function(values, ord, legend) {
	legnr = vector(mode = "integer", length = length(values))
	legnr[1] = legend_save(legend)
	list(values = values,
		 ord = ord,
		 legnr = legnr)
}





# set_legend_number = function(nr) {
# 	assign("legnr", nr, envir = .TMAP)
# }
legends_init = function() {
	assign("legs", list(), envir = .TMAP)
}

legend_save = function(legend) {
	legs = get("legs", envir = .TMAP)
	legs = c(legs, (list(legend)))
	assign("legs", legs, envir = .TMAP)
	length(legs)
}



data_type = function(x) {
	if (all(is.na(x))) {
		"na"
	} else if (is.ordered(x)) {
		"order"
	} else if (is.logical(x) || is.character(x) || is.factor(x)) {
		"factor"
	} else if (is.numeric(x)) {
		if (any(x < 0 & !is.na(x)) && any(x > 0 & !is.na(x))) {
			"div"
		} else {
			"seq"
		}
	} else {
		"unknown"
	}
}

data_type_grp = function(x) {
	if (x %in% c("seq", "div")) {
		"num"
	} else {
		x
	}
}

data_class = function(x) {
	
	# if (all(is.na(x))) {
	# 	"na"
	# } else
	 cls = if (is.numeric(x)) {
	 	y = if (inherits(x, "units")) units::drop_units(x) else x
	 	
		subclass1 = if (is.integer(x)) "int" else "real"
		subclass2 = if (any(y < 0 & !is.na(y)) && any(y > 0 & !is.na(y))) {
			"div"
		} else {
			"seq"
		}
		c("num", subclass1, subclass2)
	} else {
		subclass = if (is.ordered(x)) "ord" else "unord"
		c("fact", subclass)
	}
	 
	attr(cls, "units") = if (inherits(x, "units")) {
		paste0(" [", units(x), "]")
	} else ""
	cls
}


tmapScale = function(aes, value, scale, legend, free) {
	structure(list(aes = aes, value = tmapVars(value), scale = scale, legend = legend, free = free), class = "tmapScale")
}

tmapScaleAuto = function(x1, scale, legend, opt, aes, layer, p, sortDesc) {
	cls = data_class(x1)
	
	#if (cls[1] == "na")
	
	sc = getAesOption("scales.var", opt, aes, layer, cls = cls)
	
	tm_scalefun = paste0("tm_scale_", sc)
	
	scale_new = do.call(tm_scalefun, args = scale)
	
	
	FUN = scale_new$FUN
	scale_new$FUN = NULL
	do.call(FUN, list(x1 = x1, scale = scale_new, legend = legend, opt = opt, aes = aes, layer = layer, p, sortDesc))
	
}




# 
# tmapAesColorRGB = function(x1, x2, x3, scale, legend, opt) {
# 	values = grDevices::rgb(x1, x2, x3, maxColorValue = scale$maxValue)
# 	
# 	format_aes_results(values, list())
# }
# 
# 
# tmapAesAlpha = function(x1, scale, legend, opt) {
# 	if (!is.numeric(x1) && !is.factor(x1)) x1 = factor(x1)
# 	
# 	isNA = all(is.na(x1)) 
# 	isNUM = is.numeric(x1)
# 	
# 	if (isNUM) {
# 		if (isNA) {
# 			scale$style = "cat"
# 		} else {
# 			rng = range(x1, na.rm = TRUE)
# 			if (abs(rng[2] - rng[1]) < 1e-9 && rng[1] != rng[2]) {
# 				if (opt$show.warnings) warning("The value range of the variable \"", "\" is less than 1e-9", call. = FALSE)
# 				x1[!is.na(x1)] <- round(rng[1], 9)
# 			}
# 		}
# 	} else if (isNA) {
# 		scale$style = "cat"
# 	}
# 	
# 	#if (length(na.omit(unique(x1)))==1 && scale$style!="fixed") scale$style = "cat"
# 	
# 	if (is.na(scale$range[1])) scale$range = getTmapOption(tmapOption("aes.var", scale$aes), opt)
# 	if (is.na(scale$alphaNA)) scale$alphaNA = getTmapOption(tmapOption("aes.na", scale$aes), opt)
# 	if (is.na(scale$alphaNULL)) scale$alphaNULL = getTmapOption(tmapOption("aes.null", scale$aes), opt)
# 	
# 	
# 	
# 	if (is.factor(x1) || scale$style=="cat") {
# 		ar <- scale$range
# 		colsLeg <- cat2pal(x1,
# 						   var = "g$col",
# 						   palette = palette,
# 						   drop.levels = scale$drop.levels,
# 						   stretch.palette = scale$stretch.palette,
# 						   contrast = scale$contrast,
# 						   colorNA = scale$colorNA,
# 						   colorNULL=scale$colorNULL,
# 						   legend.labels=scale$labels,
# 						   max_levels=opt$max.categories,
# 						   legend.NA.text = scale$textNA,
# 						   showNA = scale$showNA,
# 						   process.colors=c(list(alpha=opt$alpha), opt$pc),
# 						   legend.format=legend$format,
# 						   reverse=legend$reverse)
# 		breaks <- NA
# 		
# 		
# 		
# 		neutralID <- if (palette.type=="div") round(((length(colsLeg$legend.palette)-1)/2)+1) else 1
# 		col.neutral <- colsLeg$legend.palette[1]
# 	} else {
# 		is.diverging <- use_diverging_palette(x1, scale$breaks, scale$midpoint)
# 		palette = if (is.list(scale$palette)) {
# 			palette.type = ifelse(is.diverging, "div", "seq")
# 			palette = scale$palette[[palette.type]] 
# 		} else if (scale$palette[1] %in% c("seq", "div", "cat")) {
# 			palette.type = scale$palette[1]
# 			palette = opt$aes.palette[[palette.type]]
# 		} else {
# 			palette <- scale$palette
# 			#palette.type <- palette_type(palette)
# 		}
# 		colsLeg <- num2pal(x1, 
# 						   var = "g$col",
# 						   call = NULL,
# 						   n = scale$n, 
# 						   style=scale$style, 
# 						   style.args=scale$style.args,
# 						   as.count = scale$as.count,
# 						   breaks=scale$breaks, 
# 						   interval.closure=scale$interval.closure,
# 						   palette = palette,
# 						   midpoint = scale$midpoint, #auto.palette.mapping = scale$auto.palette.mapping,
# 						   contrast = scale$contrast, legend.labels=scale$labels,
# 						   colorNA=scale$colorNA, 
# 						   colorNULL=scale$colorNULL,
# 						   legend.NA.text = scale$textNA,
# 						   showNA = scale$showNA,
# 						   process.colors=c(list(alpha=opt$alpha), opt$pc),
# 						   legend.format=legend$format,
# 						   reverse=legend$reverse)
# 		breaks <- colsLeg$breaks
# 		#breakspal <- colsLeg$breaks.palette
# 		col.neutral <- colsLeg$legend.neutral.col
# 	}
# 	
# 	
# 	cols <- colsLeg$cols
# 	legend.labels <- colsLeg$legend.labels
# 	legend.values <- colsLeg$legend.values
# 	legend.palette <- colsLeg$legend.palette
# 	
# 	type = ifelse(nchar(legend.palette[1]) > 50, "color_cont", "color_cls")
# 	
# 	legend = list(title = legend$title, 
# 				  nitems = length(legend.labels),
# 				  labels = legend.labels, 
# 				  dvalues = legend.values, 
# 				  vvalues = legend.palette,
# 				  vneutral = col.neutral,
# 				  breaks=breaks, type = type)
# 	
# 	
# 	format_aes_results(cols, legend)
# }
# 
# 
# 
# tmapAes2dSize = function(x1, scale, legend, opt) {
# 	if (all(is.na(x1))) {
# 		size = rep(NA, length(x1))
# 		legend = list(title = NA, nitems = 0)
# 	}
# 	
# 	
# 	if (!is.na(scale$lim[1])) {
# 		x1[x1<scale$lim[1]] <- NA
# 		x1[x1>scale$lim[2]] <- scale$lim[2]
# 	} else {
# 		x1[x1==0] <- NA
# 	}
# 	
# 	mx <- max(x1, na.rm=TRUE)
# 	xmax <- ifelse(is.na(scale$max), mx, scale$max)
# 	
# 	if (mx > xmax) {
# 		s <- sum(x1 > xmax, na.rm = TRUE)
# 		message("Note that ", s, " values of the variable \"", "VAR", "\" (the highest being ", mx, ") are larger than size.max, which is currently set to ", xmax, ". It is recommended to set size.max to at least ", mx, ". Another option is to set size.lim = c(0, ", xmax, "), which truncates the size of the ", s, " larger symbols. Use the scale argument to increase the size of all symbols.")
# 	}
# 	
# 	if (is.null(scale$sizes.legend)) {
# 		x_legend <- pretty(c(0, xmax), 5)
# 		x_legend <- x_legend[x_legend!=0]
# 		nxl <- length(x_legend)
# 		if (nxl>5) x_legend <- x_legend[-c(nxl-3, nxl-1)]
# 	} else {
# 		x_legend <- scale$sizes.legend
# 	}
# 	#	symbol.size.legend.values <- x_legend
# 	
# 	if (is.null(scale$sizes.legend.labels)) {
# 		sizes.legend.labels <- do.call("fancy_breaks", c(list(vec=x_legend, intervals=FALSE), legend$format))
# 	} else {
# 		if (length(scale$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of symbols in the legend", call. = FALSE)
# 		sizes.legend.labels <- g$sizes.legend.labels
# 	}
# 	
# 	rescale = TRUE # TODO: what does it do?
# 	maxX <- ifelse(rescale, xmax, 1)
# 	scaling <- ifelse(scale$perceptual, 0.5716, 0.5)
# 	symbol.size <- scale$scale*(x1/maxX)^scaling
# 	symbol.max.size <- max(symbol.size, na.rm=TRUE)
# 	symbol.legend.sizes <- scale$scale*(x_legend/maxX)^scaling
# 	
# 	if (legend$reverse) {
# 		symbol.legend.sizes <- rev(symbol.legend.sizes)
# 		symbol.size.legend.labels <- rev(sizes.legend.labels)
# 	}
# 	attr(sizes.legend.labels, "align") <- legend$format$text.align
# 	
# 	legend = list(title = legend$title,
# 				  nitems = length(sizes.legend.labels),
# 				  labels=sizes.legend.labels,
# 				  #palette=rep("pink", length(sizes.legend.labels)), #dummy
# 				  dvalues = x_legend,
# 				  vvalues = symbol.legend.sizes,
# 				  vneutral = symbol.max.size,
# 				  max.size=symbol.max.size)
# 	
# 	values = symbol.size
# 	
# 	format_aes_results(values, legend)
# }
# 
# tmapAesShape = function(x1, legend, scale, opt) {
# 	
# 	if (length(na.omit(unique(x1)))==1 && scale$style != "fixed") scale$style = "cat"
# 	
# 	if (is.factor(x1) || scale$style == "cat") {
# 		shapesLeg <- cat2shape(x1,
# 							   var = "g$shape",
# 							   shapes=scale$shapes,
# 							   drop.levels = scale$shapes.drop.levels,
# 							   legend.labels=scale$shapes.labels,
# 							   shapeNA = scale$shapeNA,
# 							   legend.NA.text = scale$shape.textNA,
# 							   showNA = scale$shape.showNA,
# 							   legend.format=scale$legend.format,
# 							   reverse=legend$reverse)
# 		symbol.shape <- shapesLeg$shps
# 		shape.legend.labels <- shapesLeg$legend.labels
# 		shape.legend.values <- shapesLeg$legend.values
# 		shape.legend.shapes <- shapesLeg$shapes
# 		shape.neutral <- shape.legend.shapes[1]
# 	} else {
# 		shapesLeg <- num2shape(x1, 
# 							   var = "g$shape",
# 							   n=scale$shapes.n, 
# 							   style=scale$shapes.style,
# 							   style.args=scale$shapes.style.args, 
# 							   breaks=scale$shapes.breaks, 
# 							   interval.closure=scale$shapes.interval.closure,
# 							   shapes=scale$shapes,
# 							   legend.labels = scale$shapes.labels,
# 							   legend.NA.text = scale$shape.textNA,
# 							   shapeNA=scale$shapeNA, 
# 							   showNA = scale$shape.showNA,
# 							   legend.format=scale$legend.format,
# 							   reverse=legend$reverse)
# 		symbol.shape <- shapesLeg$shps
# 		shape.legend.labels <- shapesLeg$legend.labels
# 		shape.legend.values <- shapesLeg$legend.values
# 		shape.legend.shapes <- shapesLeg$shapes
# 		shape.neutral <- shape.legend.shapes[1]
# 	}
# 	
# 	
# 	
# 	# if (is.numeric(x1)) x1 = as.integer(x1)
# 	# if (is.character(x1)) x1 = as.factor(x1)
# 	# if (is.factor(x1)) x1 = as.integer(x1)
# 	#n = length(shapes)
# 	values = scale$shapes[x1]
# 	legend = list(title = legend$title,
# 				  nitems = length(shape.legend.labels),
# 				  labels = shape.legend.labels,
# 				  dvalues = shape.legend.values,
# 				  vvalues = shape.legend.shapes,
# 				  vneutral = shape.neutral)
# 	
# 	
# 	format_aes_results(values, legend)
# }
# 
# 
# tmapAesLwd = function(x1, legend, scale, opt) {
# 	if (all(is.na(x1))) {
# 		size = rep(NA, length(x1))
# 		legend = list(title = NA, nitems = 0)
# 	}
# 	
# 	if (!is.numeric(x1)) stop("lwd argument contains a non-numeric variable", call. = FALSE)
# 	
# 	if (is.null(scale$lwd.legend)) {
# 		w_legend <- pretty(x1, 7)
# 		w_legend <- w_legend[w_legend!=0]
# 		nwl <- length(w_legend)
# 		if (nwl>5) w_legend <- w_legend[-c(length(w_legend)-3,length(w_legend)-1)]
# 	} else {
# 		w_legend <- scale$lwd.legend
# 	}
# 	
# 	
# 	
# 	maxW <- ifelse(rescale, max(x1, na.rm=TRUE), 1)
# 	line.legend.lwds <-  scale$scale * (w_legend/maxW)
# 	line.lwd.legend.values <- w_legend
# 	line.lwd.legend.labels <- format(w_legend, trim=TRUE)
# 	
# 	if (is.null(scale$lwd.legend.labels)) {
# 		line.lwd.legend.labels <- do.call("fancy_breaks", c(list(vec=w_legend, intervals=FALSE), legend$format))
# 	} else {
# 		if (length(scale$lwd.legend.labels) != length(w_legend)) stop("length of sizes.legend.labels is not equal to the number of lines in the legend", call. = FALSE)
# 		line.lwd.legend.labels <- scale$lwd.legend.labels
# 	}
# 	
# 	line.lwd <- scale$scale * (x1/maxW)
# 	if (reverse) {
# 		line.legend.lwds <- rev(line.legend.lwds)
# 		line.lwd.legend.labels <- rev(line.lwd.legend.labels)
# 	}
# 	attr(line.lwd.legend.labels, "align") <- legend$format$text.align
# 	
# 	
# 	legend = list(title = legend$title,
# 				  nitems = length(line.lwd.legend.labels),
# 				  labels=line.lwd.legend.labels,
# 				  #palette=rep("pink", length(sizes.legend.labels)), #dummy
# 				  dvalues = w_legend,
# 				  vvalues = line.legend.lwds,
# 				  vneutral = max(line.legend.lwds),
# 				  max.size=max(line.legend.lwds))
# 	
# 	values = line.lwd
# 	
# 	format_aes_results(values, legend)
# 	
# }
# 
