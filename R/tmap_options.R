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

		aes.const = list(fill.polygons = "grey85",
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
		aes.na = list(
			fill = "grey75",
			col = "grey75"
		),
		aes.null = list(
			fill = "grey95",
			col = "grey95"
		),
		aes.var = list(fill = list(seq = "brewer.ylorbr", div = "brewer.rdylgn", cat = "brewer.set3", cyc = "kovesi.cyclic_mrybm_35_75_c68_s25", biv = "brewer.qualseq"),
					   col = list(seq = "brewer.ylorbr", div = "brewer.rdylgn", cat = "brewer.set3", cyc = "kovesi.cyclic_mrybm_35_75_c68_s25", biv = "brewer.qualseq"),
					   fill_alpha = c(0.25, 1),
					   col_alpha = c(0.25, 1),
					   shape = 21:25),
		
		attr.color = "black",
		sepia.intensity = 0,
		saturation = 1,
		frame = TRUE,
		frame.lwd = 1,
		frame.double.line = FALSE,
		asp = NA,
		outer.margins = rep(0.02, 4),
		inner.margins = list(stars = rep(0, 4), rep(0.02, 4)),
		meta.margins = c(0.3, 0.3, 0.3, 0.3),
		meta.automatic = TRUE,
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
		legend.outside = TRUE,
		legend.outside.position = c("right", "bottom"),
		#legend.outside.size = 0.3,
		legend.position = NULL,
		legend.stack = c(all = "vertical", per_row = "horizontal", per_col = "vertical"),
		legend.justified = TRUE,
		legend.resize.as.group = TRUE,
		legend.just = c("left", "bottom"),
		legend.width = 0.4,
		legend.height = 0.9,
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
	for (i in 1:length(x)) {
		if (x[i] %in% names(y)) {
			y = y[[x[i]]]	
		} else {
			# string match (e.g. "fill.polygons" will be mapped to "fill")
			w = which(names(y) == vapply(nchar(names(y)), FUN = function(j) substr(x[i], 1, j), FUN.VALUE = character(1)))
			if (length(w) == 0) return(NULL)
			y = y[[w]]
		}
	}
	y
}


#' @rdname tmap_options
#' @export
tm_options <- function(...) {
	
	calls = names(match.call(expand.dots = TRUE)[-1])
	
	e1 = parent.frame()
	args = lapply(as.list(match.call()[-1]), eval, envir = e1)
	
	tm_element_list(do.call(tm_element, c(args, list(calls = calls, subclass= "tm_options"))))
	
}	
				 