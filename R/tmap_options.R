.defaultTmapOptions <- structure(
	list(
		modes = list(plot = list(name = "Grid", use.gradient = TRUE),
					 view = list(name = "Leaflet", 
					 			crs = list(stars = sf::st_crs(3857), sf::st_crs(4326)), 
					 			max.facets = 16, 
					 			view.legend.position = c("right", "top"), 
					 			control.position = c("left", "top"), 
					 			basemaps = c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldTopoMap"),
					 			leaflet.options = list())),
		
		crs = NA,
		max.facets = 64,
		facet.flip = FALSE,
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
		bg.color = NA,

		value.const = list(fill.polygons = "grey85",
						 fill.symbols = "grey60",
						 col.polygons = "grey40",
						 col.symbols = "grey60",
						 col.raster = "grey40",
						 col = "black",
						 lwd = 1,
						 lty = "solid",
						 shape.symbols = 21,
						 shape.dots = 19,
						 size.symbols = 1,
						 size.dots = .02,
						 fill_alpha = 1,
						 col_alpha = 1),
		value.na = list(
			fill = "grey75",
			col = "grey75",
			col.raster = "#00000000",
			lty = "solid",
			lwd = NA,
			fill_alpha = 1,
			col_alpha = 1,
			col_alpha.raster = 0
		),
		value.null = list(
			fill = "grey95",
			col = "grey95",
			col.polygons = "grey40",
			lty = "solid",
			lwd = 0.2,
			fill_alpha = 1,
			col_alpha = 1,
			size = 0.2
		),
		value.blank = list(
			fill = "#00000000",
			col = "#00000000",
			lty = "blank",
			lwd = 0,
			fill_alpha = 0,
			col_alpha = 0
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
		label.format = list(
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
		label.na = "Missing",
		attr.color = "black",
		sepia.intensity = 0,
		saturation = 1,
		frame = TRUE,
		frame.lwd = 1,
		frame.double.line = FALSE,
		asp = NA,
		outer.margins = rep(0.02, 4),
		inner.margins = list(stars = rep(0, 4), SpatRaster = rep(0, 4), rep(0.02, 4)),
		inner.margins.extra = c(0, 0, 0, 0),
		meta.margins = NA,
		meta.auto.margins = c(0.4, 0.4, 0.4, 0.4),
		between.margin = .5,
		outer.bg.color = NA,
		fontface = "plain",
		fontfamily = "",
		compass.type = "arrow",
		earth.boundary = FALSE,
		earth.boundary.color = NULL,
		earth.boundary.lwd = 1,
		earth.datum = 4326,
		space.color = NULL,
		
		
		
		legend.show = TRUE,
		# legend.only = FALSE,
		legend.design = "standard",
		legend.orientation = "portrait",
		legend.position = tm_lp_auto(cell.h = "right", cell.v = "bottom", pos.h = "left", pos.v = "top", just.h = "left", just.v = "bottom"),
		legend.width = NA,
		legend.height = NA,
		legend.stack = c(all = "vertical", per_row = "horizontal", per_col = "vertical", manual = "vertical"),
		legend.group.frame = TRUE,
		#legend.group.just = c("left", "top"),
		#legend.block.just = c("left", "bottom"),
		legend.resize.as.group = TRUE,
		legend.reverse = FALSE,
		#legend.just = c("left", "bottom"),
		legend.title.color = NULL,
		legend.title.size = 1.1,
		legend.title.fontface = NULL,
		legend.title.fontfamily = NULL,
		legend.title.just = NA,
		legend.text.color = NULL,
		legend.text.size = 0.8,
		legend.text.fontface = NULL,
		legend.text.fontfamily = NULL,
		legend.frame = FALSE,
		legend.frame.lwd = 1,
		legend.bg.color = NA,
		legend.bg.alpha = 1,
		legend.settings.standard.portrait = list(item.height = c(rect = 1.2, symbols = 1, gradient = 3),
										item.width = c(rect = 1.2, symbols = 1, gradient = 1.2),
										item.space = c(rect = 0.2, symbols = 0.2, gradient = 0),
										item.na.height = c(rect = NA, symbols = NA, gradient = 1.2),
										item.na.width = c(rect = NA, symbols = NA, gradient = 1.2),
										item.na.space = c(rect = 0.2, symbols = 0.3, gradient = 1),
										title.padding  = c(0, 0, 0.25, 0),
										ticks = list(rect = list(), symbols = list(), gradient = list(c(0.8, 1))),
										ticks.disable.na = c(rect = FALSE, symbols = FALSE, gradient = TRUE),
										ticks.col = NA,
										ticks.lwd = 1.5,
										margins = c(0.4, 0.4, 0.4, 0.4),
										margin.item.text = 0.25),
		legend.settings.standard.landscape = list(item.height = c(rect = 1, symbols = 1, gradient = 1.2),
										 item.width = c(rect = 6, symbols = 3, gradient = 6),
										 item.space = c(rect = 0.2, symbols = 0.3, gradient = 0),
										 
										 item.na.height = c(rect = NA, symbols = NA, gradient = 2),
										 item.na.width = c(rect = NA, symbols = NA, gradient = 6),
										 item.na.space = c(rect = 0.2, symbols = 0.3, gradient = 0.3),
										 title.padding  = c(0, 0, 0.25, 0),
										 ticks = list(rect = list(), symbols = list(), gradient = list(c(0.8, 1))),
										 ticks.disable.na = c(rect = FALSE, symbols = FALSE, gradient = TRUE),
										 ticks.col = NA,
										 ticks.lwd = 1.5,
										 margins = c(0.4, 0.4, 0.4, 0.4),
										 margin.item.text = 0.25),
		
		# legend.hist.bg.color = NA,
		# legend.hist.bg.alpha = 1,
		# legend.hist.size = 0.7,
		# legend.hist.height = 0.3,
		# legend.hist.width = 0.4,
	
		
		
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
		basemaps = FALSE,
		basemaps.alpha = 1,
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


v3 = list(
	legend.position = tm_lp_auto_in(just.h = "left", just.v = "top"),
	legend.text.size = 0.7,
	legend.settings.standard.portrait = list(item.height = c(rect = 1, symbols = 1, gradient = 3),
											 item.width = c(rect = 1, symbols = 1, gradient = 1.2),
											 item.space = c(rect = 0, symbols = 0, gradient = 0),
											 item.na.height = c(rect = NA, symbols = NA, gradient = 1.2),
											 item.na.width = c(rect = NA, symbols = NA, gradient = 1.2),
											 item.na.space = c(rect = 0, symbols = 0, gradient = 1),
											 title.padding  = c(0, 0, 0.25, 0),
											 ticks = list(rect = list(), symbols = list(), gradient = list()),
											 ticks.disable.na = c(rect = FALSE, symbols = FALSE, gradient = TRUE),
											 ticks.col = NA,
											 ticks.lwd = 1.5,
											 margins = c(0.4, 0.4, 0.4, 0.4),
											 margin.item.text = 0.25),
	legend.settings.standard.landscape = list(item.height = c(rect = 1, symbols = 1, gradient = 1.2),
											  item.width = c(rect = 6, symbols = 3, gradient = 6),
											  item.space = c(rect = 0.2, symbols = 0.3, gradient = 0),
											  
											  item.na.height = c(rect = NA, symbols = NA, gradient = 2),
											  item.na.width = c(rect = NA, symbols = NA, gradient = 6),
											  item.na.space = c(rect = 0.2, symbols = 0.3, gradient = 0.3),
											  title.padding  = c(0, 0, 0.25, 0),
											  ticks = list(rect = list(), symbols = list(), gradient = list(c(0.8, 1))),
											  ticks.disable.na = c(rect = FALSE, symbols = FALSE, gradient = TRUE),
											  ticks.col = NA,
											  ticks.lwd = 1.5,
											  margins = c(0.4, 0.4, 0.4, 0.4),
											  margin.item.text = 0.25)
)

.defaultTmapStyles = list(
	v3 = v3,
	gray = c(v3, list(
		bg.color = "grey85",
		values.var = list(fill = list(seq = "brewer.greys", unord = "brewer.greys", ord = "brewer.greys", cyc = "brewer.greys"),
						  col = list(seq = "brewer.greys", unord = "brewer.greys", ord = "brewer.greys", cyc = "brewer.greys"))
	)),
	grey = list(
		bg.color = "grey85",
		values.var = list(fill = list(seq = "brewer.greys", unord = "brewer.greys", ord = "brewer.greys", cyc = "brewer.greys"),
						  col = list(seq = "brewer.greys", unord = "brewer.greys", ord = "brewer.greys", cyc = "brewer.greys"))
	),
	natural = list(
		bg.color = "lightskyblue1",
		value.const = list(fill.polygons = "darkolivegreen3",
						   fill.symbols = "tomato2",
						   col.polygons = "black",
						   col.symbols = "black",
						   col.lines = "steelblue",
						   col = "black"),
		value.na = list(
			fill = "white",
			col = "white",
			col.raster = "white"),
		value.null = list(
			fill = "grey70",
			col = "grey70",
			col.polygons = "grey70"),
		values.var = list(fill = list(seq = "brewer.ylgn", div = "brewer.rdylgn", unord = "brewer.set3", ord = "brewer.ylgn"),
						  col = list(seq = "brewer.ylgn", div = "brewer.rdylgn", unord = "brewer.set3", ord = "brewer.ylgn")),
		attr.color = "black",
		space.color = "white",
		legend.frame = TRUE,
		legend.bg.color = "grey90",
		earth.boundary = TRUE,
		basemaps = "Esri.NatGeoWorldMap",
		basemaps.alpha = 1),
	cobalt = list(bg.color = "#002240",
				  value.const = list(fill.polygons = "#0088FF",
				  				   fill.symbols = "#FF9D00",
				  				   col.polygons = "#002240",
				  				   col.symbols = "#002240",
				  				   col.lines = "#002240",
				  				   col = "#002240"),
				  value.na = list(
				  	fill = "grey60",
				  	col = "grey60",
				  	col.raster = "grey60"),
				  value.null = list(
				  	fill = "grey40",
				  	col = "grey40",
				  	col.polygons = "grey40"),
				  values.var = list(fill = list(seq = "brewer.ylgn", div = "brewer.rdylgn", unord = "brewer.set3", ord = "brewer.ylgn"),
				  				  col = list(seq = "brewer.ylgn", div = "brewer.rdylgn", unord = "brewer.set3", ord = "brewer.ylgn")),
				  attr.color = "white",
				  basemaps = "CartoDB.DarkMatter",
				  basemaps.alpha = .5)
)


.defaultTmapFormats = list(World = list(inner.margins=c(0, 0.05, 0.025, 0.01),
										legend.position=tm_lp_in("left", "bottom"),
										attr.position=c("right", "bottom"),
										scale=.8),
							World_wide = list(inner.margins=c(0, 0.2, 0.025, 0.01),
											  legend.position=tm_lp_in("left", "bottom"),
											  attr.position=c("right", "bottom"),
											  scale=.8),
							NLD = list(basemaps = c(Standard = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaart/EPSG:3857/{z}/{x}/{y}.png",
													Aerial = "//geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/Actueel_ortho25/EPSG:3857/{z}/{x}/{y}.jpeg",
													Pastel = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartpastel/EPSG:3857/{z}/{x}/{y}.png",
													Gray   = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartgrijs/EPSG:3857/{z}/{x}/{y}.png"),
									   frame=FALSE, 
									   inner.margins=c(.02, .2, .06, .02),
									   legend.position=tm_lp_in("left", "top"),
									   attr.position=c("left", "bottom")),
							NLD_wide = list(basemaps = c(Standard = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaart/EPSG:3857/{z}/{x}/{y}.png",
														 Aerial = "//geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/Actueel_ortho25/EPSG:3857/{z}/{x}/{y}.jpeg",
														 Pastel = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartpastel/EPSG:3857/{z}/{x}/{y}.png",
														 Gray   = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartgrijs/EPSG:3857/{z}/{x}/{y}.png"),
											frame=FALSE, 
											inner.margins=c(.02, .3, .06, .02),
											legend.position=tm_lp_in("left", "top"),
											attr.position=c("left", "bottom")))





complete_options = function(x, o) {
	nmx = names(x)
	nmo = names(o)
	if (is.null(nmo)) return(x)
	d = setdiff(nmx, nmo)
	e = intersect(nmx, nmo)
	if (length(d)) o = c(o, x[d])
	if (length(e)) {
		for (i in e) {
			o[[i]] = complete_options(x[[i]], o[[i]])
		}
	}
	o
}




#' tmap options
#' 
#' tmap options
#' 
#' @name tmap_options 
#' @rdname tmap_options
#' @export
tmap_options = function(...) {
	opt <- get("tmapOptions", envir = .TMAP)	
	nms = names(opt)
	show.warnings = opt$show.warnings
	
	# get current style name (default: white), and set new style name (with "(modified)")
	sty_cur = getOption("tmap.style")
	sty_new <- if (substr(sty_cur, nchar(sty_cur) - 9, nchar(sty_cur)) == "(modified)") sty_cur else paste(sty_cur, "(modified)")
	
	e1 = parent.frame()
	set_new_style = FALSE
	
	lst <- list(...)
	if (length(lst) >= 1 && is.null(names(lst))) {
		arg = lst[[1]]
		if (is.list(arg)) {
			## case 1: option list is given
			args = arg
			
			style_attr = attr(args, "style")
			if (!is.null(style_attr)) {
				sty_new = style_attr
				set_new_style = TRUE
			}
			
			if (length(lst) > 1 && show.warnings) warning("Only the first argument is used; the other arguments are ignored.")
		} else {
			## case 2: option name is given
			args = sapply(lst, "[", 1)
			if (!all(args %in% nms) && show.warnings) warning("the following options do not exist: ", paste(setdiff(args, nms), collapse = ", "))
			args = intersect(args, nms)
			return(opt[args])
		}
	} else {
		## case 3: named options are set
		## case 4: tmap_options is called without arguments
		args = lapply(as.list(match.call()[-1]), eval, envir = e1)	
	}
	
	unknown_args = setdiff(names(args), names(.defaultTmapOptions))
	if (length(unknown_args) == 1) {
		stop("the following option does not exist: ", unknown_args)
	} else if (length(unknown_args) > 1) {
		stop("the following options do not exist: ", paste(unknown_args, collapse = ", "))
	}
	
	if (!length(args)) {
		# case 4
		return(opt)	
	} else {
		# case 1 and 3
		backup = opt[names(args)]
		opt[names(args)] = args # check_named_items(args, backup)
		
		options(tmap.style=sty_new)
		attr(opt, "style") = sty_new
		assign("tmapOptions", opt, envir = .TMAP)
		
		if (set_new_style) {
			if (opt$show.messages) message("tmap options successfully loaded as style \"", sty_new, "\"")
			styles = get("tmapStyles", envir = .TMAP)
			styles[[sty_new]] = suppressMessages(tmap_options_diff())
			assign("tmapStyles", styles, envir = .TMAP)
		} 
		invisible(backup)
	}	
}

#' @name tmap_options_mode
#' @rdname tmap_options
#' @export
tmap_options_mode = function(mode = NA) {
	opt = get("tmapOptions", envir = .TMAP)	
	if (is.na(mode)) mode = getOption("tmap.mode")
	opt2 = opt$modes[[mode]]
	
	int_opt = intersect(names(opt), names(opt2))
	diff_opt = setdiff(names(opt2), names(opt))
	
	if (length(int_opt)) opt[int_opt] = opt2[int_opt]
	if (length(diff_opt)) opt = c(opt, opt2[diff_opt])
	opt
}


tmap_option = function(name, type = NULL) {
	get_option_class(tmap_options()[[name]], class = type, spatial_class = FALSE)
}


get_option_class = function(opt, class = NULL, spatial_class = TRUE) {
	is_spatial = !spatial_class || (any(names(opt) %in% c("stars", "sf", "sfc", "raster", "terra", "sp")))
	if (!is.null(class) && is.list(opt) && is_spatial) {
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

# getTmapOption = function(x, opt) {
# 	x = unlist(x)
# 	y = opt
# 	for (i in 1:length(x)) {
# 		if (x[i] %in% names(y)) {
# 			y = y[[x[i]]]	
# 		} else {
# 			# string match (e.g. "fill.polygons" will be mapped to "fill")
# 			namesy_equal_nchar = vapply(nchar(names(y)), FUN = function(j) substr(x[i], 1, j), FUN.VALUE = character(1))
# 			w = which(names(y) == namesy_equal_nchar)
# 			if (length(w) == 0) return(NULL)
# 			y = y[[w[which.max(nchar(namesy_equal_nchar[w]))]]]
# 		}
# 	}
# 	y
# }


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
tm_place_legends_right = function(width = NA) {
	if (is.na(width)) {
		tm_options(legend.position = tm_lp_out("right", "center"))
	} else {
		tm_options(meta.margins = c(0, 0, 0, width), legend.position = tm_lp_out("right", "center"))
	}
}

#' @export
tm_place_legends_left = function(width = NA) {
	if (is.na(width)) {
		tm_options(legend.position = tm_lp_out("left", "center"))
	} else {
		tm_options(meta.margins = c(0, 0, 0, width), legend.position = tm_lp_out("right", "center"))
	}
}

#' @export
tm_place_legends_bottom = function(height = NA) {
	if (is.na(height)) {
		tm_options(legend.position = tm_lp_out("center", "bottom"))
	} else {
		tm_options(meta.margins = c(height, 0, 0, 0), legend.position = tm_lp_out("center", "bottom"))
	}
}

#' @export
tm_place_legends_top = function(height = NA) {
	if (is.na(height)) {
		tm_options(legend.position = tm_lp_out("center", "top"))
	} else {
		tm_options(meta.margins = c(height, 0, 0, 0), legend.position = tm_lp_out("center", "top"))
	}
}

#' @export
tm_extra_innner_margin = function(left = 0, right = 0, top = 0, bottom = 0) {
	tm_options(inner.margins.extra = c(bottom, left, top, right))
}




#' @rdname tm_layout
#' @param style name of the style
#' @export
tm_style <- function(style, ...) {
	args <- list(...)
	
	.tmapOptions <- get("tmapOptions", envir = .TMAP)	
	check_style(style)
	
	args$style <- style
	structure(list(tm_layout=args), class = "tm")
}


#' @rdname tm_layout
#' @param format name of the format
#' @export
tm_format <- function(format, ...) {
	args <- list(...)
	
	.tmapFormats <- get("tmapFormats", envir = .TMAP)
	
	if (!(format %in% names(.tmapFormats))) stop("Unknown format. Please check tmap_format() for available formats")
	
	formatArgs <- .tmapFormats[[format]]
	if (length(args)) {
		formatArgs[names(args)] <- args	
	}
	formatArgs$style <- NA
	
	called <- names(args)
	if (is.null(called)) called <- character(0)
	
	attr(formatArgs, "format_args") <- called
	structure(list(tm_layout=formatArgs), class = "tm")
}

				 