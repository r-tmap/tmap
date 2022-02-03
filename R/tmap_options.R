.defaultTmapOptions <- structure(
	list(
		# mode specific options or default values
		modes = list(plot = list(name = "Grid", use.gradient = T),
					 view = list(name = "Leaflet", 
					 			crs = list(stars = sf::st_crs(3857), sf::st_crs(4326)), 
					 			facet.max = 16, 
					 			view.legend.position = c("right", "top"), 
					 			control.position = c("left", "top"), 
					 			basemap.server = c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldTopoMap"),
					 			leaflet.options = list())),
		
		crs = NA,
		
		# facets
		facet.max = 64, # was max.facets
		facet.flip = FALSE,
		
		# spatial object class specific options
		raster.max.cells = 1e6, # was max.raster
		
		# general
		show.messages = TRUE,
		show.warnings = TRUE,
		
		# output
		output.format = "png",
		output.size = 49,
		output.dpi = 300,
		output.dpi.animation = 100,
		
		# default visual variable values
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
		values.var = list(fill = list(seq = "brewer.ylorbr", div = "brewer.rdylgn", unord = "brewer.set3", ord = "brewer.ylorbr", cyc = "kovesi.cyclic_mrybm_35_75_c68_s25", biv = "brewer.qualseq"),
						  col = list(seq = "brewer.ylorbr", div = "brewer.rdylgn", unord = "brewer.set3", ord = "brewer.ylorbr", cyc = "kovesi.cyclic_mrybm_35_75_c68_s25", biv = "brewer.qualseq"),
						  size = tmap_seq(0, 1, power = "sqrt"),
						  lwd = c(0, 3),
						  lty = c("dashed", "dotted", "dotdash", "longdash", "twodash"),
						  fill_alpha = c(0.25, 1),
						  col_alpha = c(0.25, 1),
						  shape = 21:25,
						  area = c(0, 1)),
		values.contrast = list(fill = NA, col = NA, size = c(0.2, 1), lwd = c(0.2, 1), lty = NA, fill_alpha = NA, col_alpha = NA, shape = NA), # NA = automatic, NULL is not applicable
		value.neutral = list(size = 1,
							 lwd = 2,
							 fill_alpha = 1,
							 col_alpha = 1),
		
		# scales
		scales.var = list(fill = list(fact = "categorical", num = "intervals", int = "discrete"),
						  col = list(fact = "categorical", num = "intervals", int = "discrete"),
						  lwd = list(fact = "categorical", num = "continuous", int = "discrete"),
						  lty = list(fact = "categorical", num = "categorical"),
						  shape = list(fact = "categorical", num = "categorical"),
						  size = list(fact = "categorical", num = "continuous"),
						  fill_alpha = list(fact = "categorical", num = "intervals"),
						  col_alpha = list(fact = "categorical", num = "intervals"),
						  area = list(fact = "categorical", num = "continuous")),
		
		# labels			  
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

		
		###############################3
		# tm_layout options
		###############################3
		scale = 1,
		asp = NA,
		
		# background
		bg.color = NA,		
		outer.bg.color = NA,
		
		# frame
		frame = TRUE,
		frame.lwd = 1,
		frame.r = 2,
		frame.double.line = FALSE,
		
			
		# margins	
		outer.margins = rep(0.02, 4),
		inner.margins = list(stars = rep(0, 4), SpatRaster = rep(0, 4), rep(0.02, 4)),
		inner.margins.extra = c(0, 0, 0, 0),
		meta.margins = NA,
		meta.auto.margins = c(0.4, 0.4, 0.4, 0.4),
		between.margin = 0.5,
		component.offset = c(inside = 0.25, INSIDE = 0, outside = 0, OUTSIDE = 0),
		component.stack.margin = 0,
		grid.mark.height = 2,
		xylab.height = 1.25,
		coords.height = 1.25,

		# xlab, ylab, grid
		xlab.show = FALSE,
		ylab.show = FALSE,
		xlab.pos = "bottom",
		ylab.pos = "right",
		grid.show = FALSE,
		grid.label.pos = c("right", "bottom"),
				
		# panel
		panel.type = NA, # "wrap" or "xtab",
		panel.wrap.pos = "top", # or "left", "right", "bottom"
		panel.xtab.pos = c("left", "top"),
		
		# data
		unit = "metric",
		
		# colors
		color.sepia.intensity = 0,
		color.saturation = 1,
		color.vision.deficiency.sim = "none",
		
		# text
		fontface = "plain",
		fontfamily = "",
		
		# legend		
		legend.show = TRUE,
		legend.design = "standard",
		legend.orientation = "portrait",
		legend.position = tm_pos_auto_out(cell.h = "right", cell.v = "bottom", pos.h = "left", pos.v = "top", just.h = "left", just.v = "top"),
		legend.width = NA,
		legend.height = NA,
		legend.stack = c(all = "vertical", per_row = "horizontal", per_col = "vertical", manual = "vertical"),
		legend.group.frame = TRUE,
		legend.resize.as.group = FALSE,
		legend.reverse = FALSE,
		legend.title.color = NULL,
		legend.title.size = 1.1,
		legend.title.fontface = NULL,
		legend.title.fontfamily = NULL,
		legend.title.just = NA,
		legend.text.color = NULL,
		legend.text.size = 0.8,
		legend.text.fontface = NULL,
		legend.text.fontfamily = NULL,
		legend.frame = TRUE,
		legend.frame.lwd = 1,
		legend.frame.r = 2,
		legend.bg.color = NA,
		legend.bg.alpha = 1,
		legend.settings.standard.portrait = list(item.height = c(rect = 1.2, symbols = 1, gradient = 3, lines = 1.2),
										item.width = c(rect = 1.2, symbols = 1, gradient = 1.2, lines = 1.2),
										item.r = 2,
										item.space = c(rect = 0.2, symbols = 0.2, gradient = 0, lines = 0.2),
										item.na.height = c(rect = NA, symbols = NA, gradient = 1.2, lines = NA),
										item.na.width = c(rect = NA, symbols = NA, gradient = 1.2, lines = NA),
										item.na.space = c(rect = 0.2, symbols = 0.3, gradient = 1, lines = 0.2),
										title.padding  = c(0, 0, 0.25, 0),
										ticks = list(rect = list(), symbols = list(), gradient = list(c(0.8, 1)), lines = list()),
										ticks.disable.na = c(rect = FALSE, symbols = FALSE, gradient = TRUE, lines = FALSE),
										ticks.col = NA,
										ticks.lwd = 1.5,
										margins = c(0.4, 0.4, 0.4, 0.4),
										margin.item.text = 0.25),
		legend.settings.standard.landscape = list(item.height = c(rect = 1, symbols = 1, gradient = 1.2, lines = 1),
										 item.width = c(rect = 6, symbols = 3, gradient = 6, lines = 6),
										 item.r = 2,
										 item.space = c(rect = 0.2, symbols = 0.3, gradient = 0, lines = 0.2),
										 item.na.height = c(rect = NA, symbols = NA, gradient = 2, lines = NA),
										 item.na.width = c(rect = NA, symbols = NA, gradient = 4, lines = NA),
										 item.na.space = c(rect = 0.2, symbols = 0.3, gradient = 0.3, lines = 0.2),
										 title.padding  = c(0, 0, 0.25, 0),
										 ticks = list(rect = list(), symbols = list(), gradient = list(c(0.8, 1)), lines = list()),
										 ticks.disable.na = c(rect = FALSE, symbols = FALSE, gradient = TRUE, lines = FALSE),
										 ticks.col = NA,
										 ticks.lwd = 1.5,
										 margins = c(0.4, 0.4, 0.4, 0.4),
										 margin.item.text = 0.25),
		
		# components
		compass.type = "arrow",
		title.size = 1.3,
		title.color = NULL,
		title.fontface = NULL,
		title.fontfamily = NULL,
		title.bg.color = NA,
		title.bg.alpha = 1,
		title.padding = c(0.25, 0.25, 0.25, 0.25),
		title.frame = FALSE,
		title.frame.lwd = 1,
		title.frame.r = 2,
		title.stack = "left",
		title.position = tm_pos_out("center", "top", "left", "top", "left", "top"),
		title.width = NA,
		title.heigth = NA,
		title.group.frame = TRUE,
		title.resize.as.group = FALSE,
		
		panel.show = NA,
		panel.labels = NA,
		panel.label.size = 1,
		panel.label.color = "black",
		panel.label.fontface = NULL,
		panel.label.fontfamily = NULL,
		panel.label.bg.color = "grey80",
		panel.label.height = 1.25,
		panel.label.rot = c(90, 0),

		# not implemented yet
		qtm.scalebar = TRUE,
		qtm.minimap = FALSE,
		qtm.mouse.coordinates = TRUE,

		# not used/implemented in tmap4 (yet?)
		title = NA,
		earth.boundary = FALSE,
		earth.boundary.color = NULL,
		earth.boundary.lwd = 1,
		earth.datum = 4326,
		space.color = NULL,
		attr.color = "black",
		max.categories = 30,
		legend.hist.bg.color = NA,
		legend.hist.bg.alpha = 1,
		legend.hist.size = 0.7,
		legend.hist.height = 0.3,
		legend.hist.width = 0.4,
		title.snap.to.legend = NA,
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
		basemap.server = "Esri.WorldGrayCanvas",
		basemap.alpha = 1,
		basemap.zoom = NA,
		overlays = NULL,
		overlays.alpha = 1,
		alpha = NA,
		colorNA = NA,
		symbol.size.fixed = FALSE,
		dot.size.fixed = TRUE,
		text.size.variable = FALSE,
		bbox = NULL,
		check.and.fix = FALSE,
		set.bounds = FALSE,
		set.view = NA,
		set.zoom.limits = NA
	),
	style = "white"
)

styles = list(
	v3 = list(
		frame.lwd = 1,
		frame.r = 0,
		legend.position = tm_pos_auto_in(just.h = "left", just.v = "top"),
		legend.text.size = 0.7,
		legend.title.size = 0.9,
		legend.frame = FALSE,
		legend.frame.r = 0,
		legend.settings.standard.portrait = list(item.height = c(rect = 1, symbols = 1, gradient = 3),
												 item.width = c(rect = 1, symbols = 1, gradient = 1.2),
												 item.r = 0,
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
												  item.r = 0,
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
	),
	gray = list(
		bg.color = "grey85",
		values.var = list(fill = list(seq = "brewer.greys", unord = "brewer.greys", ord = "brewer.greys", cyc = "brewer.greys"),
						  col = list(seq = "brewer.greys", unord = "brewer.greys", ord = "brewer.greys", cyc = "brewer.greys"))
	),
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
				  basemaps.alpha = .5),
	albatross = list(bg.color = "#00007F",
					 value.const = list(fill.polygons = "#4C4C88",
					 				   fill.symbols = "#BFBFFF",
					 				   col.polygons = "#00004C",
					 				   col.symbols = "#00004C",
					 				   col.lines = "#BFBFFF",
					 				   col = "#00004C"),
					 value.na = list(
					 	fill = "grey60",
					 	col = "grey60",
					 	col.raster = "grey60"),
					 value.null = list(
					 	fill = "#4C4C88",
					 	col = "#4C4C88",
					 	col.polygons = "#4C4C88"),
					 values.var = list(fill = list(seq = "brewer.ylorrd", div = "brewer.rdylgn", unord = "brewer.set3", ord = "brewer.ylorrd"),
					 				  col = list(seq = "brewer.ylorrd", div = "brewer.rdylgn", unord = "brewer.set3", ord = "brewer.ylorrd")),
					 attr.color = "#BFBFFF",
					 basemaps = "CartoDB.DarkMatter",
					 basemaps.alpha = .5),
	classic = list(color.sepia.intensity = .7,
				   fontfamily = "serif",
				   frame = TRUE,
				   frame.double.line = TRUE,
				   compass.type = "rose")
)

.defaultTmapStyles = list(
	gray = styles$gray,
	grey = styles$grey,
	natural = styles$natural,
	cobalt = styles$cobalt,
	albatross = styles$albatross,
	classic = styles$classic,
	v3 = styles$v3,
	gray_v3 = c(styles$gray, styles$v3),
	grey_v3 = c(styles$grey, styles$v3),
	natural_v3 = c(styles$natural_v3, styles$v3),
	cobalt_v3 = c(styles$cobalt, styles$v3),
	albatross_v3 = c(styles$albatross, styles$v3),
	classic_v3 = c(styles$classic, styles$v3)
)


.defaultTmapFormats = list(World = list(inner.margins=c(0, 0.05, 0.025, 0.01),
										legend.position=tm_pos_in("left", "bottom"),
										attr.position=c("right", "bottom"),
										scale=.8),
							World_wide = list(inner.margins=c(0, 0.2, 0.025, 0.01),
											  legend.position=tm_pos_in("left", "bottom"),
											  attr.position=c("right", "bottom"),
											  scale=.8),
							NLD = list(basemaps = c(Standard = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaart/EPSG:3857/{z}/{x}/{y}.png",
													Aerial = "//geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/Actueel_ortho25/EPSG:3857/{z}/{x}/{y}.jpeg",
													Pastel = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartpastel/EPSG:3857/{z}/{x}/{y}.png",
													Gray   = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartgrijs/EPSG:3857/{z}/{x}/{y}.png"),
									   frame=FALSE, 
									   inner.margins=c(.02, .2, .06, .02),
									   legend.position=tm_pos_in("left", "top"),
									   attr.position=c("left", "bottom")),
							NLD_wide = list(basemaps = c(Standard = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaart/EPSG:3857/{z}/{x}/{y}.png",
														 Aerial = "//geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/Actueel_ortho25/EPSG:3857/{z}/{x}/{y}.jpeg",
														 Pastel = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartpastel/EPSG:3857/{z}/{x}/{y}.png",
														 Gray   = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartgrijs/EPSG:3857/{z}/{x}/{y}.png"),
											frame=FALSE, 
											inner.margins=c(.02, .3, .06, .02),
											legend.position=tm_pos_in("left", "top"),
											attr.position=c("left", "bottom")))





complete_options = function(x, o) {
	nmx = names(x)
	nmo = names(o)
	if (length(x) == 0L) return(o)
	if (is.null(nmo) || is.null(nmx)) return(x)
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
#' @param ... See details
#' @details
#' | option        | description |
#' | ------------- |:-------------:|
#' | modes		|  Mode specific options. It is a named list where names correspond to the available modes. Each item is a list of options. |
#' | crs		|  Map crs (see \code{\link{tm_shape}}). \code{NA} means the crs is specified in \code{\link{tm_shape}} |
#' | facet.max		| Maximum number of facets |
#' | facet.flip		| Should facets be flipped (in case of facet wrap)? This can also be set via \code{\link{tm_facets_flip}} |
#' | raster.max.cells		| Maximum number of raster grid cells  |
#' | show.messages		| Show messages? |
#' | show.warnings		| Show warnings? |
#' | output.format		| Output format |
#' | output.size		| Output size |
#' | output.dpi		| Output dpi |
#' | output.dpi.animation		| Output dpi for animations |
#' | value.const		| Default visual value constants e.g. the default fill color for \code{tm_shape(World) + tm_polygons()}. A list is required with per visual variable a value. |
#' | value.na		| Default visual values that are used to visualize NA data values. A list is required with per visual variable a value.|
#' | value.null		| Default visual values that are used to visualize null (out-of-scope) data values. A list is required with per visual variable a value.|
#' | value.blank		| Default visual values that correspond to blank. For color these are \code{"#00000000"} meaning transparent. A list is required with per visual variable a value. |
#' | values.var		| Default values when a data variable to mapped to a visual variable, e.g. a color palette. A list is required with per visual variable a value. |
#' | values.contrast		| Default contrast for values. See \code{values.contrast} of \code{\link{tm_scale_categorical}}. A list is required with per visual variable a value.
#' | value.neutral		| Default values for when a data variable to mapped to a visual variable, e.g. a color palette. A list is required with per visual variable a value. | |
#' | scales.var		| Default scales. |
#' | label.format		| Format for the labels (was legend.format in tmap v3) |
#' | label.na		| Default label for missing values |
#' See \code{\link{tm_layout}} for layout specific options
#' @name tmap_options 
#' @rdname tmap_options
#' @export
tmap_options = function(...) {
	o <- get("tmapOptions", envir = .TMAP)	
	nms = names(o)
	show.warnings = o$show.warnings
	
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
			return(o[args])
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
		return(o)	
	} else {
		# case 1 and 3
		backup = o[names(args)]
		o[names(args)] = args # check_named_items(args, backup)
		
		options(tmap.style=sty_new)
		attr(o, "style") = sty_new
		assign("tmapOptions", o, envir = .TMAP)
		
		if (set_new_style) {
			if (o$show.messages) message("tmap options successfully loaded as style \"", sty_new, "\"")
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
tmap_options_mode = function(mode = NA, default.options = FALSE) {
	o = if (default.options) .defaultTmapOptions else get("tmapOptions", envir = .TMAP)	
	
	if (is.na(mode)) mode = getOption("tmap.mode")
	opt2 = o$modes[[mode]]
	
	int_opt = intersect(names(o), names(opt2))
	diff_opt = setdiff(names(opt2), names(o))
	
	if (length(int_opt)) o[int_opt] = opt2[int_opt]
	if (length(diff_opt)) o = c(o, opt2[diff_opt])
	o
}


tmap_option = function(name, type = NULL) {
	get_option_class(tmap_options()[[name]], class = type, spatial_class = FALSE)
}


get_option_class = function(o, class = NULL, spatial_class = TRUE) {
	is_spatial = !spatial_class || (any(names(o) %in% c("stars", "sf", "sfc", "raster", "terra", "sp")))
	if (!is.null(class) && is_spatial) { # && is.list(o)
		mtch = which(names(o) %in% class)
		if (!length(mtch)) mtch = which(names(o) == "")[1]
		o = o[[mtch]]
	}
	o
}

# 
# 
# tmap_options_class = function(class) {
# 	o = tmap_options()
# 	o = lapply(o, function(o) {
# 		if (is.list(o) && any(names(o) %in% c("stars", "sf", "sfc", "raster", "terra", "sp"))) {
# 			mtch = which(names(o) %in% class)
# 			if (!length(mtch)) mtch = which(names(o) == "")[1]
# 			o[[mtch]]
# 		} else {
# 			o
# 		}
# 	})
# 	o
# }



tmap_graphics_name = function() {
	mode = getOption("tmap.mode")
	
	get("tmapOptions", envir = .TMAP)$modes[[mode]]$name
}


tmapOption = function(...) {
	structure(list(...), class = "tmapOption")
}

# getTmapOption = function(x, o) {
# 	x = unlist(x)
# 	y = o
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


getAesOption = function(x, o, aes, layer, cls = NULL) {
	y = o[[x]]
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
		tm_options(legend.position = tm_pos_out("right", "center"))
	} else {
		tm_options(meta.margins = c(0, 0, 0, width), legend.position = tm_pos_out("right", "center"))
	}
}

#' @export
tm_place_legends_left = function(width = NA) {
	if (is.na(width)) {
		tm_options(legend.position = tm_pos_out("left", "center"))
	} else {
		tm_options(meta.margins = c(0, 0, 0, width), legend.position = tm_pos_out("right", "center"))
	}
}

#' @export
tm_place_legends_bottom = function(height = NA) {
	if (is.na(height)) {
		tm_options(legend.position = tm_pos_out("center", "bottom"))
	} else {
		tm_options(meta.margins = c(height, 0, 0, 0), legend.position = tm_pos_out("center", "bottom"))
	}
}

#' @export
tm_place_legends_top = function(height = NA) {
	if (is.na(height)) {
		tm_options(legend.position = tm_pos_out("center", "top"))
	} else {
		tm_options(meta.margins = c(height, 0, 0, 0), legend.position = tm_pos_out("center", "top"))
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
	#structure(list(tm_layout=args), class = "tm")
	do.call(tm_options, args)
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
	
	
	#called <- names(args)
	#if (is.null(called)) called <- character(0)
	
	#attr(formatArgs, "format_args") <- called
	#structure(list(tm_layout=formatArgs), class = "tm")
	do.call(tm_options, formatArgs)
	
}

# get options with a prefic
get_prefix_opt = function(prefix, class, o) {
	if (missing(prefix)) prefix = substr(class, 4, nchar(class))
	ot = o[names(o)[substr(names(o), 1, nchar(prefix)) == prefix]]
	names(ot) = substr(names(ot), nchar(prefix)+2, nchar(names(ot)))
	ot
}


				 