.defaultTmapOptions = structure(
	list(
		# mode specific options or default values
		modes = list(plot = list(name = "Grid", 
								 use.gradient = FALSE,
								 basemap.show = FALSE),
					 view = list(name = "Leaflet", 
					 			use.WebGL = FALSE,
					 			legend.position = tm_pos_in(pos.h = "right", pos.v = "bottom", align.h = "left", align.v = "top", just.h = "left", just.v = "bottom"),
					 			crs = list(dimensions = 3857, 4326), 
					 			facet.max = 16, 
					 			#view.legend.position = c("right", "top"), 
					 			control.position = c("left", "top"), 
					 			control.collapse = FALSE,
					 			panel.show = FALSE,
					 			basemap.show = TRUE,
					 			set.bounds = FALSE,
					 			set.view = NA,
					 			set.zoom.limits = NA,
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
						 fill.dots = "black",
						 col.polygons = "grey40",
						 col.symbols = "grey40",
						 col.raster = "grey40",
						 col = "black",
						 lwd = 1,
						 lty = "solid",
						 text = "Abc",
						 fontface = "plain",
						 shape.symbols = 21,
						 shape.bubbles = 21,
						 shape.dots = 19,
						 size.symbols = 1,
						 size.bubbles = 1.3333,
						 size.dots = .15,
						 size.text = 1,
						 fill_alpha = 1,
						 col_alpha = 1),
		value.na = list(
			fill = "grey75",
			col = "grey75",
			col.raster = "#00000000",
			lty = "solid",
			lwd = NA,
			text = "Unknown",
			fontface = "plain",
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
			text = "",
			fontface = "plain",
			fill_alpha = 1,
			col_alpha = 1,
			size = 0.2
		),
		value.blank = list(
			fill = "#00000000",
			col = "#00000000",
			lty = "blank",
			lwd = 0,
			text = "",
			fontface = "plain",
			fill_alpha = 0,
			col_alpha = 0
		),
		values.var = list(fill = list(seq = "hcl.blues3", div = "pu_gn_div",
									  unord = "tol.muted", ord = "hcl.blues3", cyc = "tol.rainbow_pu_rd", biv = "pu_gn_bivs"),
						  col = list(seq = "hcl.blues3", div = "pu_gn_div",
						  		   unord = "tol.muted", ord = "hcl.blues3", cyc = "tol.rainbow_pu_rd", biv = "pu_gn_bivs"),
						  size = tmap_seq(0, 1, power = "sqrt"),
						  size.bubbles = tmap_seq(0, 1, power = "sqrt"),
						  lwd = c(0, 3),
						  lty = c("dashed", "dotted", "dotdash", "longdash", "twodash"),
						  text = LETTERS,
						  fontface = c("plain", "italic", "bold"),
						  fill_alpha = c(0.25, 1),
						  col_alpha = c(0.25, 1),
						  shape = 21:25,
						  area = c(0, 1)),
		values.range = list(fill = NA, col = NA, size = c(0, 1), lwd = c(0.1, 1),
							lty = NA, text = NA, fontface = NA, fill_alpha = NA,
							col_alpha = NA, shape = NA), # NA = automatic, NULL is not applicable
		value.neutral = list(size = 0.75,
							 lwd = 2,
							 lty = "solid",
							 fill_alpha = 1,
							 col_alpha = 1,
							 text = "Abc",
							 fontface = "plain"),
		values.scale = list(
			1,
			lwd.lines = 1,
			size.symbols = 1,
			size.bubbles = 1.3333
		),
		
		# scales
		scales.var = list(fill = list(fact = "categorical", num = "intervals", int = "discrete"),
						  col = list(fact = "categorical", num = "intervals", int = "discrete"),
						  lwd = list(fact = "categorical", num = "continuous", int = "discrete"),
						  lty = list(fact = "categorical", num = "intervals"),
						  shape = list(fact = "categorical", num = "intervals"),
						  size = list(fact = "categorical", num = "continuous"),
						  fill_alpha = list(fact = "categorical", num = "intervals"),
						  col_alpha = list(fact = "categorical", num = "intervals"),
						  area = list(fact = "categorical", num = "continuous"),
						  text = list(fact = "asis", num = "asis"),
						  fontface = list(fact = "categorical", num = "categorical")),
		
		scale.misc.args = list(continuous = list(n = c(fill = 5, col = 5, 5), 
												 outliers.trunc = c(FALSE, FALSE), 
												 trans = "identity",
												 limits = list(fill = NA, col = NA, 0)),
							   rank = list(n = 5)), # NA means take data range, 0 means include 0
		
		
		
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
			text.to.columns = FALSE,
			html.escape = TRUE
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
		component.offset = c(inside = 0.75, INSIDE = 0, outside = 0, OUTSIDE = 0),
		component.stack.margin = 0,
		grid.mark.height = 2,
		xylab.height = 1.25,
		coords.height = 1.25,

		# xlab, ylab
		xlab.show = FALSE,
		xlab.text = "",
		xlab.size = 1,
		xlab.color = "black",
		xlab.rotation = 0,
		xlab.space = 0,
		xlab.fontface = "plain",
		xlab.fontfamily = "",
		xlab.side = "bottom",
		
		ylab.show = FALSE,
		ylab.text = "",
		ylab.size = 1,
		ylab.color = "black",
		ylab.rotation = 0,
		ylab.space = 0,
		ylab.fontface = "plain",
		ylab.fontfamily = "",
		ylab.side = "left",
		

		# panel
		panel.type = NA, # "wrap" or "xtab",
		panel.wrap.pos = "top", # or "left", "right", "bottom"
		panel.xtab.pos = c("left", "top"),
		
		# data
		unit = "metric",
		
		# general visual settings
		
		# colors
		color.sepia.intensity = 0,
		color.saturation = 1,
		color.vision.deficiency.sim = "none",
		
		# text
		text.fontface = "plain",
		text.fontfamily = "",
		

		
		# legend		
		legend.show = TRUE,
		legend.design = "standard",
		legend.orientation = "portrait",
		legend.position = tm_pos_auto_out(cell.h = "right", cell.v = "bottom",
										  pos.h = "left", pos.v = "top",
										  align.h = "left", align.v = "top", just.h = "left", just.v = "top"),
		legend.width = NA,
		legend.height = NA,
		legend.stack = c(all = "vertical", per_row = "horizontal", per_col = "horizontal", all_row = "vertical", all_col = "horizontal", manual = "vertical"),
		legend.group.frame = TRUE,
		legend.resize.as.group = FALSE,
		legend.reverse = FALSE,
		legend.na.show = NA,
		legend.title.color = NULL,
		legend.title.size = 0.9,
		legend.title.fontface = NULL,
		legend.title.fontfamily = NULL,
		legend.xlab.color = NULL,
		legend.xlab.size = 0.9,
		legend.xlab.fontface = NULL,
		legend.xlab.fontfamily = NULL,
		legend.ylab.color = NULL,
		legend.ylab.size = 0.9,
		legend.ylab.fontface = NULL,
		legend.ylab.fontfamily = NULL,
		legend.text.color = NULL,
		legend.text.size = 0.7,
		legend.text.fontface = NULL,
		legend.text.fontfamily = NULL,
		legend.frame = TRUE,
		legend.frame.lwd = 1,
		legend.frame.r = 2,
		legend.bg.color = NA,
		legend.bg.alpha = 1,
		legend.settings.standard.portrait = list(item.height = c(rect = 1.2, symbols = 1, gradient = 3, lines = 1.2, text = 1.2, bivariate = 1.2),
										item.width = c(rect = 1.2, symbols = 1, gradient = 1.2, lines = 1.2, text = 1.2, bivariate = 1.2),
										item.r = 2,
										item.space = c(rect = 0.2, symbols = 0.2, gradient = 0, lines = 0.2, text = 0.2, bivariate = 0),
										item.na.height = c(rect = NA, symbols = NA, gradient = 1.2, lines = NA, text = NA, bivariate = NA),
										item.na.width = c(rect = NA, symbols = NA, gradient = 1.2, lines = NA, text = NA, bivariate = NA),
										item.na.space = c(rect = 0.2, symbols = 0.3, gradient = 1, lines = 0.2, text = 0.2, bivariate = 0.2),
										item.shape = 107,
										title.padding  = c(0, 0, 0.25, 0),
										xlab.padding = c(0, 0, 0.25, 0),
										ylab.padding = c(0, 0, 0.25, 0),
										title.align = c(bivariate = "right", "left"),
										xlab.align = "left",
										ylab.align = "center",
										ticks = list(rect = list(), symbols = list(), gradient = list(c(1, 1.5)), lines = list(), text = list(), bivariate = list()),
										ticks.disable.na = c(rect = FALSE, symbols = FALSE, gradient = TRUE, lines = FALSE, text = FALSE, bivariate = TRUE),
										ticks.col = NA,
										ticks.lwd = 1.5,
										margins = c(0.4, 0.4, 0.4, 0.4),
										margin.item.text = 0.25),
		legend.settings.standard.landscape = list(item.height = c(rect = 1, symbols = 1, gradient = 1.2, lines = 1, text = 1),
										 item.width = c(rect = 6, symbols = 3, gradient = 6, lines = 6, text = 6),
										 item.r = 2,
										 item.space = c(rect = 0.2, symbols = 0.3, gradient = 0, lines = 0.2, text = 0.2),
										 item.na.height = c(rect = NA, symbols = NA, gradient = 2, lines = NA, text = NA),
										 item.na.width = c(rect = NA, symbols = NA, gradient = 4, lines = NA, text = NA),
										 item.na.space = c(rect = 0.2, symbols = 0.3, gradient = 0.3, lines = 0.2, text = 0.2),
										 item.shape = 107,
										 
										 title.padding  = c(0, 0, 0.25, 0),
										 xlab.padding = c(0, 0, 0.25, 0),
										 ylab.padding = c(0, 0, 0.25, 0),
										 title.align = c(bivariate = "right", "left"),
										 xlab.align = "left",
										 ylab.align = "center",
										 ticks = list(rect = list(), symbols = list(), gradient = list(c(1, 1.2)), lines = list(), text = list()),
										 ticks.disable.na = c(rect = FALSE, symbols = FALSE, gradient = TRUE, lines = FALSE, text = FALSE),
										 ticks.col = NA,
										 ticks.lwd = 1.5,
										 margins = c(0.4, 0.4, 0.4, 0.4),
										 margin.item.text = 0.25),
		
		# components
		title.show = FALSE,
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
		title.stack = "vertical",
		title.position = tm_pos_out(cell.h = "center", cell.v ="top", pos.h = "left", pos.v = "top", align.h = "left", align.v = "top", just.h = "left", just.v = "bottom"),
		title.group.frame = TRUE,
		title.resize.as.group = FALSE,

		credits.show = FALSE,
		credits.size = .7,
		credits.color = NA,
		credits.fontface = NA,
		credits.fontfamily = NA,
		credits.bg.color = NA,
		credits.bg.alpha = 1,
		credits.padding = c(0.25, 0.25, 0.25, 0.25),
		credits.frame = FALSE,
		credits.frame.lwd = 1,
		credits.frame.r = 2,
		credits.stack = "vertical",
		credits.position = tm_pos_in(pos.h = "right", pos.v = "bottom", align.h = "left", align.v = "top", just.h = "left", just.v = "bottom"),
		credits.width = NA,
		credits.heigth = NA,
		credits.group.frame = TRUE,
		credits.resize.as.group = FALSE,
		
		compass.north=0, 
		compass.type="arrow", 
		compass.text.size=.8,
		compass.size=NA,
		compass.show.labels=1, 
		compass.cardinal.directions=c("N", "E", "S", "W"), 
		compass.text.color=NA,
		compass.color.dark=NA, 
		compass.color.light=NA,
		compass.lwd=1,
		compass.bg.color=NA,
		compass.bg.alpha=NA,
		compass.margins = c(0.4, 0.4, 0.4, 0.4),
		
		# standard arguments:
		compass.show = FALSE,
		compass.stack = "vertical",
		compass.position = tm_pos_in(pos.h = "right", pos.v = "bottom", align.h = "left", align.v = "top", just.h = "left", just.v = "bottom"),
		compass.frame = FALSE,
		compass.frame.lwd = 1,
		compass.frame.r = 2,
		compass.group.frame = TRUE,
		compass.resize.as.group = FALSE,
		
		scalebar.show = FALSE,
		scalebar.breaks=NULL,
		scalebar.width=.3, 
		scalebar.text.size = .5,
		scalebar.text.color=NA,
		scalebar.color.dark="black", 
		scalebar.color.light="white",
		scalebar.lwd=1,
		scalebar.position=NA,
		scalebar.bg.color=NA,
		scalebar.bg.alpha=NA,
		scalebar.size = NULL,
		scalebar.margins = c(0.01,0.01,0.01,0.01),
		
		# standard arguments:
		scalebar.stack = "vertical",
		scalebar.position = tm_pos_in(pos.h = "right", pos.v = "bottom", align.h = "left", align.v = "top", just.h = "left", just.v = "bottom"),
		scalebar.frame = FALSE,
		scalebar.frame.lwd = 1,
		scalebar.frame.r = 2,
		scalebar.group.frame = TRUE,
		scalebar.resize.as.group = FALSE,		
			
		grid.show = FALSE,
		grid.labels.pos = c("left", "bottom"),
		grid.x=NA,
		grid.y=NA,
		grid.n.x=NA,
		grid.n.y=NA,
		grid.crs=NA,
		grid.col=NA,
		grid.lwd=1,
		grid.alpha=NA,
		grid.labels.show=TRUE,
		grid.labels.size=.6,
		grid.labels.col=NA,
		grid.labels.rot = c(0, 0),
		grid.labels.format = list(big.mark = ","),
		grid.labels.cardinal = FALSE,
		grid.labels.margin.x=0,
		grid.labels.margin.y=0,
		grid.labels.space.x=NA,
		grid.labels.space.y=NA,
		grid.labels.inside.frame=FALSE,
		grid.ticks = TRUE, #labels.show & !labels.inside.frame,
		grid.lines = TRUE,
		grid.ndiscr = 100,

		
		# standard arguments:
		# mouse.stack = "vertical",
		# mouse.position = tm_pos_in(pos.h = "right", pos.v = "bottom", align.h = "left", align.v = "top", just.h = "left", just.v = "bottom"),
		# mouse.frame = FALSE,
		# mouse.frame.lwd = 1,
		# mouse.frame.r = 2,
		# mouse.group.frame = TRUE,
		# mouse.resize.as.group = FALSE,
		# 
		
		mouse_coordinates.stack = "vertical", 
		mouse_coordinates.position = tm_pos_in(pos.h = "right", pos.v = "bottom", align.h = "left", align.v = "top", just.h = "left", just.v = "bottom"),
		mouse_coordinates.show = FALSE,

		panel.show = NA,
		panel.labels = NA,
		panel.label.size = 1,
		panel.label.color = "black",
		panel.label.fontface = NULL,
		panel.label.fontfamily = NULL,
		panel.label.bg.color = "grey80",
		panel.label.height = 1,
		panel.label.rot = c(90, 0),

		# 
		bbox = NULL,
		set.bounds = FALSE,
		set.view = NA,
		set.zoom.limits = NA,
		
		# not implemented yet
		qtm.scalebar = TRUE,
		qtm.minimap = FALSE,
		qtm.mouse.coordinates = TRUE,

		# not used/implemented in tmap4 (yet?)
		#title = NA,
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
		#title.snap.to.legend = NA,
		attr.outside = FALSE,
		attr.outside.position = "bottom",
		attr.outside.size = NA,
		attr.position = c("right", "bottom"),
		attr.just = c("left", "bottom"),
		basemap.server = c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldTopoMap"),
		basemap.alpha = 1,
		basemap.zoom = NA,
		overlays = NULL,
		overlays.alpha = 1,
		alpha = NA,
		colorNA = NA,
		symbol.size.fixed = FALSE,
		dot.size.fixed = TRUE,
		text.size.variable = FALSE,
		check.and.fix = FALSE
	),
	style = "white",
	specified = character()
)

styles = list(
	v3 = list(
		modes = list(view = list(legend.bg.alpha = 0.8)),
		value.na = list(
			fill = "grey75",
			col = "grey75",
			col.raster = "#00000000",
			lty = "solid",
			lwd = NA,
			text = "Unknown",
			fontface = "plain",
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
			text = "",
			fontface = "plain",
			fill_alpha = 1,
			col_alpha = 1,
			size = 0.2
		),
		value.blank = list(
			fill = "#00000000",
			col = "#00000000",
			lty = "blank",
			lwd = 0,
			text = "",
			fontface = "plain",
			fill_alpha = 0,
			col_alpha = 0
		),
		values.var = list(fill = list(seq = "brewer.yl_or_br", div = "brewer.rd_yl_gn", unord = "brewer.set3", ord = "brewer.yl_or_br"),
						  col = list(seq = "brewer.yl_or_br", div = "brewer.rd_yl_gn", unord = "brewer.set3", ord = "brewer.yl_or_br")),
		frame.lwd = 1,
		frame.r = 0,
		title.position = tm_pos_in(pos.h = "left", pos.v = "top", align.h = "left", align.v = "top", just.h = "left", just.v = "bottom"),
		title.size = 1.3,
		legend.position = tm_pos_auto_in(align.h = "left", align.v = "top", just.h = "left", just.v = "bottom"),
		legend.text.size = 0.7,
		legend.title.size = 0.9,
		legend.frame = FALSE,
		legend.frame.r = 0,
		legend.settings.standard.portrait = list(item.height = c(rect = 1, symbols = 1, gradient = 1, bivariate = 1),
												 item.width = c(rect = 1, symbols = 1, gradient = 1.2, bivariate = 1),
												 item.r = 0,
												 item.space = c(rect = 0, symbols = 0, gradient = 0, bivariate = 0),
												 item.na.height = c(rect = NA, symbols = NA, gradient = 1.2, bivariate = 1),
												 item.na.width = c(rect = NA, symbols = NA, gradient = 1.2, bivariate = 1),
												 item.na.space = c(rect = 0, symbols = 0, gradient = 0, bivariate = 0),
												 title.padding  = c(0, 0, 0.25, 0),
												 ticks = list(rect = list(), symbols = list(), gradient = list(), bivariate = list()),
												 ticks.disable.na = c(rect = FALSE, symbols = FALSE, gradient = TRUE, bivariate = TRUE),
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
				  values.var = list(fill = list(seq = "brewer.yl_gn", div = "brewer.rd_yl_gn", unord = "brewer.set3", ord = "brewer.yl_gn"),
				  				  col = list(seq = "brewer.yl_gn", div = "brewer.rd_yl_gn", unord = "brewer.set3", ord = "brewer.yl_gn")),
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
	gray_v3 = c(styles$v3, styles$gray),
	grey_v3 = c(styles$v3, styles$grey),
	natural_v3 = c(styles$v3, styles$natural),
	cobalt_v3 = c(styles$v3, styles$cobalt),
	albatross_v3 = c(styles$v3, styles$albatross),
	classic_v3 = c(styles$v3, styles$classic)
)

.defaultTmapFormats = list(World = list(inner.margins=c(0, 0.05, 0.025, 0.01),
										legend.position=tm_pos_in("left", "bottom"),
										attr.position=c("right", "bottom"),
										scale=.8,
										title.size = 1.3),
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
#' | option         	| description |
#' | ------         	| ----------- |
#' | `modes`		    |  Mode specific options. It is a named list where names correspond to the available modes. Each item is a list of options. |
#' | `crs`		    	|  Map crs (see [tm_shape()]). `NA` means the crs is specified in [tm_shape()]. The crs that is used by the transformation functions is defined in [tm_shape()].|
#' | `facet.max`		| Maximum number of facets |
#' | `facet.flip`		| Should facets be flipped (in case of facet wrap)? This can also be set via [tm_facets_flip()] |
#' | `raster.max.cells`	| Maximum number of raster grid cells  |
#' | `show.messages`	| Show messages? |
#' | `show.warnings`	| Show warnings? |
#' | `output.format`	| Output format |
#' | `output.size`		| Output size |
#' | `output.dpi`		| Output dpi |
#' | `output.dpi.animation`		| Output dpi for animations |
#' | `value.const`		| Default visual value constants e.g. the default fill color for `tm_shape(World) + tm_polygons()`. A list is required with per visual variable a value. |
#' | `value.na`			| Default visual values that are used to visualize NA data values. A list is required with per visual variable a value.|
#' | `value.null`		| Default visual values that are used to visualize null (out-of-scope) data values. A list is required with per visual variable a value.|
#' | `value.blank`		| Default visual values that correspond to blank. For color these are `"#00000000"` meaning transparent. A list is required with per visual variable a value. |
#' | `values.var`		| Default values when a data variable to mapped to a visual variable, e.g. a color palette. A list is required with per visual variable a value. |
#' | `values.range`		| Default range for values. See `values.range` of [tm_scale_categorical()]. A list is required with per visual variable a value.
#' | `value.neutral`	| Default values for when a data variable to mapped to a visual variable, e.g. a color palette. A list is required with per visual variable a value. | |
#' | `scales.var`		| Default scales. |
#' | `label.format`		| Format for the labels (was `legend.format` in tmap v3). |
#' | `label.na`			| Default label for missing values. |
#' See [tm_layout()] for layout specific options.
#' @name tmap_options 
#' @rdname tmap_options
#' @export
tmap_options = function(...) {
	o = get("tmapOptions", envir = .TMAP)	
	nms = names(o)
	show.warnings = o$show.warnings
	
	# get current style name (default: white), and set new style name (with "(modified)")
	sty_cur = getOption("tmap.style")
	sty_new = if (substr(sty_cur, nchar(sty_cur) - 9, nchar(sty_cur)) == "(modified)") sty_cur else paste(sty_cur, "(modified)")
	
	e1 = parent.frame()
	set_new_style = FALSE
	
	lst = list(...)
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
			if (!all(args %in% nms) && show.warnings) warning("The following options do not exist: ", paste(setdiff(args, nms), collapse = ", "))
			args = intersect(args, nms)
			return(o[args])
		}
	} else {
		## case 3: named options are set
		## case 4: tmap_options is called without arguments
		args = lapply(as.list(match.call()[-1]), eval, envir = e1)	
	}
	
	mode_opts = setdiff(unique(unlist(lapply(o$modes, names))), "name")
	
	all_opts = union(mode_opts, names(.defaultTmapOptions))
	
	unknown_args = setdiff(names(args), all_opts)
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
		attr(o, "specified") = names(args)
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
#' @param mode mode, e.g. `"plot"` or `"view"`
#' @param default.options return the default options or the current options?
#' @rdname tmap_options
#' @export
tmap_options_mode = function(mode = NA, default.options = FALSE) {
	o = if (default.options) .defaultTmapOptions else get("tmapOptions", envir = .TMAP)	
	
	if (is.na(mode)) mode = getOption("tmap.mode")
	opt2 = o$modes[[mode]]
	
	specified = attr(o, "specified")
	int_opt = setdiff(intersect(names(o), names(opt2)), specified)
	diff_opt = setdiff(names(opt2), names(o))
	
	if (length(int_opt)) o[int_opt] = opt2[int_opt]
	if (length(diff_opt)) o = c(o, opt2[diff_opt])
	o
}


tmap_option = function(name, type = NULL) {
	get_option_class(tmap_options()[[name]], class = type, spatial_class = FALSE)
}

get_option_class = function(o, class = NULL, spatial_class = TRUE) {
	is_spatial = !spatial_class || (any(names(o) %in% c("stars", "sf", "sfc", "raster", "terra", "sp", "dimensions")))
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
	

	
	if (any(al %in% names(y))) {
		id = which(al %in% names(y))[1] # take first, most specific layer, e.g. when layer = c("dots", "symbols"), take dots if exists
		z = y[[al[id]]]
	} else if (aes %in% names(y)) {
		# take matching visual variable (regardless what layer)
		z = y[[aes]]
	} else if (is.list(y)) {
		# check if there are non-named list items, if so take the first one
		eid = which(names(y) == "")[1]
		if (!is.na(eid)) {
			z = y[[eid]]
		} else {
			return(NA)
		}
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

getAesValue = function(x, aes) {
	nms = names(x)
	
	if (is.null(nms)) {
		x
	} else if (any(nms %in% c("fill", "col", "size", "shape", "lwd", "lty", "fontsize", "fontface"))) {
		if (aes %in% nms) {
			x[[aes]]
		} else {
			if (any(nms == "")) {
				x[[which(nms == "")[1]]]
			} else {
				x
			}
		}
	} else {
		x
	} 
}


#' @rdname tmap_options
#' @export
tm_options = function(...) {
	
	calls = names(match.call(expand.dots = TRUE)[-1])
	
	e1 = parent.frame()
	args = lapply(as.list(match.call()[-1]), eval, envir = e1)
	
	tm_element_list(do.call(tm_element, c(args, list(calls = calls, subclass = "tm_options"))))
	
}	

#' @rdname tm_extra_innner_margin
#' @name tm_place_legends_right
#' @export
tm_place_legends_right = function(width = NA) {
	if (is.na(width)) {
		tm_options(legend.position = tm_pos_out("right", "center"))
	} else {
		tm_options(meta.margins = c(0, 0, 0, width), legend.position = tm_pos_out("right", "center"))
	}
}

#' @rdname tm_extra_innner_margin
#' @name tm_place_legends_left
#' @param width width
#' @export
tm_place_legends_left = function(width = NA) {
	if (is.na(width)) {
		tm_options(legend.position = tm_pos_out("left", "center"))
	} else {
		tm_options(meta.margins = c(0, 0, 0, width), legend.position = tm_pos_out("right", "center"))
	}
}

#' @rdname tm_extra_innner_margin
#' @name tm_place_legends_bottom
#' @param height height
#' @export
tm_place_legends_bottom = function(height = NA) {
	if (is.na(height)) {
		tm_options(legend.position = tm_pos_out("center", "bottom"))
	} else {
		tm_options(meta.margins = c(height, 0, 0, 0), legend.position = tm_pos_out("center", "bottom"))
	}
}

#' @rdname tm_extra_innner_margin
#' @name tm_place_legends_top
#' @export
tm_place_legends_top = function(height = NA) {
	if (is.na(height)) {
		tm_options(legend.position = tm_pos_out("center", "top"))
	} else {
		tm_options(meta.margins = c(height, 0, 0, 0), legend.position = tm_pos_out("center", "top"))
	}
}

#' @rdname tm_extra_innner_margin
#' @name tm_place_legends_inside
#' @param pos.h,pos.v position (horizontal and vertical)
#' @export
tm_place_legends_inside = function(pos.h = NULL, pos.v = NULL) {
	if (is.null(pos.h) || is.null(pos.v)) {
		if (!is.null(pos.h)) {
			warning("tm_place_legends_inside also requires pos.v", call. = FALSE)
		} else if (!is.null(pos.v)) {
			warning("tm_place_legends_inside also requires pos.h", call. = FALSE)
		}
		tm_options(legend.position = tm_pos_auto_in())
	} else {
		tm_options(legend.position = tm_pos_in(pos.h = pos.h, pos.v = pos.v))
	}
}

#' tmap layout: helper functions
#' 
#' @param left,right,top,bottom extra margins
#' @export
#' @rdname tm_extra_innner_margin
#' @name tm_extra_innner_margin
tm_extra_innner_margin = function(left = 0, right = 0, top = 0, bottom = 0) {
	tm_options(inner.margins.extra = c(bottom, left, top, right))
}

#' @rdname tm_layout
#' @param style name of the style
#' @export
tm_style = function(style, ...) {
	args = list(...)
	
	.tmapOptions = get("tmapOptions", envir = .TMAP)	
	check_style(style)
	
	args$style = style
	#structure(list(tm_layout=args), class = "tm")
	do.call(tm_options, args)
}

#' @rdname tm_layout
#' @param format name of the format
#' @export
tm_format = function(format, ...) {
	args = list(...)
	
	.tmapFormats = get("tmapFormats", envir = .TMAP)
	
	if (!(format %in% names(.tmapFormats))) stop("Unknown format. Please check tmap_format() for available formats")
	
	formatArgs = .tmapFormats[[format]]
	if (length(args)) {
		formatArgs[names(args)] = args	
	}
	formatArgs$style = NA
	
	if ("title" %in% names(formatArgs)) {
		warning("The 'title' argument of 'tm_format()' is deprecated as of tmap 4.0. Please use 'tm_title()' instead.", call. = FALSE)
		title = formatArgs$title
		formatArgs$title = NULL
		do.call(tm_options, formatArgs) + tm_title(text = title)
	} else {
		do.call(tm_options, formatArgs)
	}

}

# get options with a prefic
get_prefix_opt = function(prefix, class, o) {
	if (missing(prefix)) prefix = substr(class, 4, nchar(class))
	ot = o[names(o)[substr(names(o), 1, nchar(prefix)) == prefix]]
	names(ot) = substr(names(ot), nchar(prefix)+2, nchar(names(ot)))
	ot
}

# (partly) named vector: get 1st name match or otherwise 1st non-named argument
# used in tm_scale_continuous, but similar function should exists for options? (todo: check)
get_vector_id = function(x, id) {
	if (is.null(names(x))) {
		x[1]
	} else if (id %in% names(x)) {
		unname(x[id][1L])
	} else if (any("" %in% names(x))) {
		unname(x[which(names(x) == "")[1]])
	} else {
		x[1]
	}
}


#' Internal tmap function to add a default value for the layer functions
#' 
#' Internal tmap function to add a default value for the layer functions
#' 
#' @param option, one of: `"value.const"`, `"value.na"`, `"value.blank"`, `"values.var"`, `'values.range'`, `"value.neutral"`, `"scales.var"`
#' @param id name of the visual variable with layer, in the format `"x.y"`,
#'   where `x` is the visual variable and `y` is the layer.
#'   It is also possible to set `x` only; then it applies to all layer functions. 
#' @param value value
#' @keywords internal
#' @export
tmapAddLayerOptions = function(option, id, value) {
	if (!(option %in% c("value.const", "value.na", "value.blank", "values.var", "values.range", "value.neutral", "scales.var"))) {
		stop("Unknown option")
	}
	o = tmap_option(option)
	o[[id]] = value
	o2 = structure(list(o), names = option)
	tmap_options(o2)
}





#' @rdname tmap_options
#' @export
tmap_options_diff <- function() {
	.tmapOptions <- get("tmapOptions", envir = .TMAP)	
	iden <- mapply(identical, .tmapOptions, .defaultTmapOptions)
	
	if (all(iden)) {
		message("current tmap options are similar to the default tmap options (style \"white\")")
	} else {
		message("current tmap options (style \"", attr(.tmapOptions, "style"), "\") that are different from default tmap options (style \"white\"):")
		.tmapOptions[!iden]
	}
}

#' @rdname tmap_options
#' @export
tmap_options_reset <- function() {
	assign("tmapOptions", .defaultTmapOptions, envir = .TMAP)
	options(tmap.style="white")
	message("tmap options successfully reset")
	invisible(NULL)
}

#' @export
#' @param style style, see [tmap_style()] for available styles
#' @rdname tmap_options
tmap_options_save <- function(style) {
	show.messages <- get("tmapOptions", envir = .TMAP)$show.messages
	
	stylediff <- suppressMessages(tmap_options_diff())
	
	.tmapOptions <- get("tmapOptions", envir = .TMAP)	
	
	if (is.null(stylediff)) {
		if (show.messages) message("current style is the same as the default style, so nothing to save")
		return(invisible(.tmapOptions))
	}
	
	options(tmap.style=style)
	attr(.tmapOptions, "style") <- style
	assign("tmapOptions", .tmapOptions, envir = .TMAP)
	
	styles <- get("tmapStyles", envir = .TMAP)
	styles[[style]] <- suppressMessages(tmap_options_diff())
	assign("tmapStyles", styles, envir = .TMAP)
	
	if (show.messages) message("current tmap options saved as style \"", style, "\"")
	invisible(.tmapOptions)
}
