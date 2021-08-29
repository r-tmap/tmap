.defaultTmapOptions <- structure(
	list(
		modes = list(plot = list(name = "Grid"),
					 view = list(name = "Leaflet", crs = list(stars = sf::st_crs(3857), sf::st_crs(4326)), max.facets = 16, view.legend.position = c("right", "top"), control.position = c("left", "top"), leaflet.options = list())),
		
		crs = NA,
		max.facets = 64,
		
		grid.mark.height = 2,
		xylab.height = 1.25,
		coords.height = 1.25,
		xlab.show = FALSE,
		ylab.show = FALSE,
		xlab.pos = "bottom",
		ylab.pos = "right",
		grid.show = FALSE,
		grid.label.pos = c("right", "bottom"),
		panel.type = NA, # "wrap" or "xtab",
		panel.wrap.pos = "top", # or "left", "right", "bottom"
		panel.xtab.pos = c("left", "top"),
		
		unit = "metric",
		
		max.categories = 30,
		max.raster = 1e6,
		show.messages = TRUE,
		show.warnings = TRUE,
		output.format = "png",
		output.size = 49,
		output.dpi = 300,
		output.dpi.animation = 100,
		check.and.fix = FALSE,
		title = NA,
		scale = 1,
		title.size = 1.3,
		bg.color = "white",

		value.const = list(fill.polygons = "grey85",
						 fill.symbols = "grey60",
						 col.polygons = "grey40",
						 col.symbols = "grey60",
						 col.dots = "black",
						 col.lines = "black",
						 col.text = "black",
						 lwd.lines = 1,
						 lwd.polygons = 1,
						 lwd.symbols = 1,
						 lty.lines = "solid",
						 lty.polygons = 1,
						 lty.symbols = 1,
						 shape.symbols = 21,
						 shape.dots = 19,
						 size.symbols = 1,
						 size.dots = .02,
						 fill_alpha.polygons = 1,
						 fill_alpha.symbols = 1,
						 col_alpha.polygons = 1,
						 col_alpha.symbols = 1,
						 col_alpha.dots = 1,
						 col_alpha.lines = 1,
						 col_alpha.text = 1
		),
		value.na = list(
			fill = "grey75",
			col = "grey75",
			lty = "solid",
			lwd = NA,
			fill_alpha = 1,
			col_alpha = 1
			
		),
		value.null = list(
			fill = "grey95",
			col = "grey95",
			lty = "solid",
			lwd = NA,
			fill_alpha = 1,
			col_alpha = 1
		),
		scales.var = list(fill = list(fact = "categorical", num = "intervals", int = "discrete"),
						  col = list(fact = "categorical", num = "intervals", int = "discrete"),
						  lwd = list(fact = "categorical", num = "continuous", int = "discrete"),
						  lty = list(fact = "categorical", num = "categorical"),
						  shape = list(fact = "categorical", num = "categorical"),
						  size = list(fact = "categorical", num = "continuous"),
						  fill_alpha = list(fact = "categorical", num = "intervals"),
						  col_alpha = list(fact = "categorical", num = "intervals"),
						  area = list(fact = "categorical", num = "continuous")),
						  
		values.var = list(fill = list(seq = "brewer.ylorbr", div = "brewer.rdylgn", unord = "brewer.set3", ord = "brewer.ylorbr", cyc = "kovesi.cyclic_mrybm_35_75_c68_s25", biv = "brewer.qualseq"),
						  col = list(seq = "brewer.ylorbr", div = "brewer.rdylgn", unord = "brewer.set3", ord = "brewer.ylorbr", cyc = "kovesi.cyclic_mrybm_35_75_c68_s25", biv = "brewer.qualseq"),
						  size = tmap_seq(0, 1, power = "sqrt"),
						  lwd = c(0, 3),
						  lty = c("dashed", "dotted", "dotdash", "longdash", "twodash"),
						  fill_alpha = c(0.25, 1),
						  col_alpha = c(0.25, 1),
						  shape = 21:25,
						  area = c(0, 1)),
		values.contrast = list(fill = NA, col = NA, size = NA, lwd = NA, lty = NA, fill_alpha = NA, col_alpha = NA, shape = NA), # NA = automatic, NULL is not applicable
		value.neutral = list(size = 1,
							 lwd = 2,
							 fill_alpha = 1,
							 col_alpha = 1),
		label.na = "Missing",
		attr.color = "black",
		sepia.intensity = 0,
		saturation = 1,
		frame = TRUE,
		frame.lwd = 1,
		frame.double.line = FALSE,
		asp = NA,
		outer.margins = rep(0.02, 4),
		inner.margins = list(stars = rep(0, 4), rep(0.02, 4)),
		meta.margins = NA,
		meta.auto.margins = c(0.3, 0.3, 0.3, 0.3),
		between.margin = .5,
		outer.bg.color = NULL,
		fontface = "plain",
		fontfamily = "",
		compass.type = "arrow",
		earth.boundary = FALSE,
		earth.boundary.color = NULL,
		earth.boundary.lwd = 1,
		earth.datum = 4326,
		space.color = NULL,
		legend.show = TRUE,
		legend.only = FALSE,
		legend.position = tm_lp_auto("right", "bottom"),
		legend.space = c(rect = 0.2, symbols = 0.3, gradient = 0.8),
		legend.space.na = c(rect = 0.2, symbols = 0.3, gradient = 0.3),
		legend.stack = c(all = "vertical", per_row = "horizontal", per_col = "vertical", manual = "vertical"),
		legend.justified = TRUE,
		legend.resize.as.group = TRUE,
		legend.just = c("left", "bottom"),
		legend.width = NA,
		legend.height = NA,
		legend.hist.height = 0.3,
		legend.hist.width = 0.4,
		#legend.width,
		legend.title.color = NULL,
		legend.title.size = 1.1,
		legend.title.fontface = NULL,
		legend.title.fontfamily = NULL,
		legend.text.color = NULL,
		legend.text.size = 0.7,
		legend.text.fontface = NULL,
		legend.text.fontfamily = NULL,
		legend.hist.size = 0.7,
		legend.format = list(
			fun = NULL,
			scientific = FALSE,
			digits = NA,
			big.num.abbr = c(mln = 6, bln = 9),
			prefix = "",
			suffix = "",
			text.separator = "to",
			text.less.than = c("Less", "than"),
			text.or.more = c("or", "more"),
			text.align = NA,
			text.to.columns = FALSE
		),
		legend.frame = FALSE,
		legend.frame.lwd = 1,
		legend.bg.color = NA,
		legend.bg.alpha = 1,
		legend.hist.bg.color = NA,
		legend.hist.bg.alpha = 1,
		title.snap.to.legend = NA,
		title.position = c("left", "top"),
		title.color = NULL,
		title.fontface = NULL,
		title.fontfamily = NULL,
		title.bg.color = NA,
		title.bg.alpha = 1,
		panel.show = NA,
		panel.labels = NA,
		panel.label.size = 1,
		panel.label.color = "black",
		panel.label.fontface = NULL,
		panel.label.fontfamily = NULL,
		panel.label.bg.color = "grey80",
		panel.label.height = 1.25,
		panel.label.rot = c(90, 0),
		main.title = NA,
		main.title.size = 1.5,
		main.title.color = "black",
		main.title.fontface = NULL,
		main.title.fontfamily = NULL,
		main.title.position = "left",
		attr.outside = FALSE,
		attr.outside.position = "bottom",
		attr.outside.size = NA,
		attr.position = c("right", "bottom"),
		attr.just = c("left", "bottom"),
		basemaps = c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldTopoMap"),
		basemaps.alpha = c(1, 1, 1),
		overlays = NULL,
		overlays.alpha = 1,
		qtm.scalebar = TRUE,
		qtm.minimap = FALSE,
		qtm.mouse.coordinates = TRUE,
		alpha = NA,
		colorNA = NA,
		#projection = 3857,
		symbol.size.fixed = FALSE,
		dot.size.fixed = TRUE,
		text.size.variable = FALSE,
		bbox = NULL,
		set.bounds = FALSE,
		set.view = NA,
		set.zoom.limits = NA
	),
	style = "white"
)



#' @export
tm_layout = function(...) {
	tm_options(...)
}



#' tmap options
#' 
#' tmap options
#' 
#' @name tmap_options 
#' @rdname tmap_options
#' @export
tmap_options = function() {
	opt = get("tmapOptions", envir = .TMAP)	
	mode = getOption("tmap.mode")
	opt2 = opt$modes[[mode]]
	
	int_opt = intersect(names(opt), names(opt2))
	diff_opt = setdiff(names(opt2), names(opt))
	
	if (length(int_opt)) opt[int_opt] = opt2[int_opt]
	if (length(diff_opt)) opt = c(opt, opt2[diff_opt])
	opt
}




tmap_option = function(name, class = NULL) {
	get_option_class(tmap_options()[[name]], class = class)
}


get_option_class = function(opt, class = NULL) {
	if (!is.null(class) && is.list(opt) && any(names(opt) %in% c("stars", "sf", "sfc", "raster", "terra", "sp"))) {
		mtch = which(names(opt) %in% class)
		if (!length(mtch)) mtch = which(names(opt) == "")[1]
		opt = opt[[mtch]]
	}
	opt
}

# 
# 
# tmap_options_class = function(class) {
# 	opt = tmap_options()
# 	opt = lapply(opt, function(o) {
# 		if (is.list(o) && any(names(o) %in% c("stars", "sf", "sfc", "raster", "terra", "sp"))) {
# 			mtch = which(names(o) %in% class)
# 			if (!length(mtch)) mtch = which(names(o) == "")[1]
# 			o[[mtch]]
# 		} else {
# 			o
# 		}
# 	})
# 	opt
# }



tmap_graphics_name = function() {
	mode = getOption("tmap.mode")
	
	get("tmapOptions", envir = .TMAP)$modes[[mode]]$name
}


tmapOption = function(...) {
	structure(list(...), class = "tmapOption")
}

getTmapOption = function(x, opt) {
	x = unlist(x)
	y = opt
	print(x)
	for (i in 1:length(x)) {
		if (x[i] %in% names(y)) {
			y = y[[x[i]]]	
		} else {
			# string match (e.g. "fill.polygons" will be mapped to "fill")
			namesy_equal_nchar = vapply(nchar(names(y)), FUN = function(j) substr(x[i], 1, j), FUN.VALUE = character(1))
			w = which(names(y) == namesy_equal_nchar)
			if (length(w) == 0) return(NULL)
			y = y[[w[which.max(nchar(namesy_equal_nchar[w]))]]]
		}
	}
	y
}


getAesOption = function(x, opt, aes, layer, cls = NULL) {
	y = opt[[x]]
	al = paste(aes, layer, sep = ".")
	

	
	if (al %in% names(y)) {
		z = y[[al]]
	} else if (aes %in% names(y)) {
		z = y[[aes]]
	} else if (is.list(y)) {
		return(NA)
	} else {
		return(y)
	}
	
	if (!is.null(cls) && is.list(z)) {
		mid = vapply(names(z), FUN = "%in%", FUN.VALUE = logical(1), cls)
		if (any(mid)) {
			z = z[[which(mid)[1]]]
		}
	}
	z
}


#' @rdname tmap_options
#' @export
tm_options <- function(...) {
	
	calls = names(match.call(expand.dots = TRUE)[-1])
	
	e1 = parent.frame()
	args = lapply(as.list(match.call()[-1]), eval, envir = e1)
	
	tm_element_list(do.call(tm_element, c(args, list(calls = calls, subclass= "tm_options"))))
	
}	

#' @export
tm_place_legends_right = function(width) {
	tm_options(meta.margins = c(0, 0, 0, width), legend.position = tm_lp_out("right", "center"))
}

#' @export
tm_place_legends_left = function(width) {
	tm_options(meta.margins = c(0, width, 0, 0), legend.position = tm_lp_out("left", "center"))
}

#' @export
tm_place_legends_bottom = function(height) {
	tm_options(meta.margins = c(height, 0, 0, 0), legend.position = tm_lp_out("center", "bottom"))
}

#' @export
tm_place_legends_top = function(height) {
	tm_options(meta.margins = c(0, 0, height, 0), legend.position = tm_lp_out("center", "top"))
}

				 