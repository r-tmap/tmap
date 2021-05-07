.defaultTmapOptions <- structure(
	list(
		graphics = list(plot = list(name = "Grid", crs = NA, facets = 64),
						view = list(name = "Leaflet", crs = list(stars = sf::st_crs(3857), sf::st_crs(4326)), facets = 16)),
		asp = NA,
		show.messages = TRUE,
		outer.margins = c(0.02, 0.02, 0.02, 0.02),
		inner.margins = NA,
		meta.margins = c(0, 0, 0, 0.2),
		between.margins = 0.5,
		panel.label.height = 1.25,
		grid.mark.height = 2,
		xylab.height = 1.25,
		coords.height = 1.25,
		xlab.show = FALSE,
		ylab.show = FALSE,
		xlab.pos = "bottom",
		ylab.pos = "right",
		grid.show = FALSE,
		grid.label.pos = c("right", "bottom"),
		panel.type = "none", # "wrap" or "xtab",
		panel.wrap.pos = "top", # or "left", "right", "bottom"
		panel.xtab.pos = c("left", "top")
		# 
		# 
		# 
		# unit = "metric",
		# limits = c(facets.plot = 64, facets.view = 4),
		# max.categories = 30,
		# max.raster = c(plot = 1e6, view = 1e6),
		# show.messages = TRUE,
		# show.warnings = TRUE,
		# output.format = "png",
		# output.size = 49,
		# output.dpi = 300,
		# output.dpi.animation = 100,
		# check.and.fix = FALSE,
		# title = NA,
		# scale = 1,
		# title.size = 1.3,
		# bg.color = "white",
		# aes.color = c(
		# 	fill = "grey85",
		# 	borders = "grey40",
		# 	symbols = "grey60",
		# 	dots = "black",
		# 	lines = "black",
		# 	text = "black",
		# 	na = "grey75",
		# 	null = "grey95"
		# ),
		# aes.palette = list(seq = "YlOrBr", div = "RdYlGn", cat = "Set3"),
		# attr.color = "black",
		# sepia.intensity = 0,
		# saturation = 1,
		# frame = TRUE,
		# frame.lwd = 1,
		# frame.double.line = FALSE,
		# asp = NA,
		# outer.margins = rep(0.02, 4),
		# inner.margins = NA,
		# between.margin = .5,
		# outer.bg.color = NULL,
		# fontface = "plain",
		# fontfamily = "",
		# compass.type = "arrow",
		# earth.boundary = FALSE,
		# earth.boundary.color = NULL,
		# earth.boundary.lwd = 1,
		# earth.datum = 4326,
		# space.color = NULL,
		# legend.show = TRUE,
		# legend.only = FALSE,
		# legend.outside = NA,
		# legend.outside.position = "right",
		# legend.outside.size = 0.3,
		# legend.position = NULL,
		# legend.stack = "vertical",
		# legend.just = c("left", "bottom"),
		# legend.width = 0.4,
		# legend.height = 0.9,
		# legend.hist.height = 0.3,
		# legend.hist.width = 0.4,
		# #legend.width,
		# legend.title.color = NULL,
		# legend.title.size = 1.1,
		# legend.title.fontface = NULL,
		# legend.title.fontfamily = NULL,
		# legend.text.color = NULL,
		# legend.text.size = 0.7,
		# legend.text.fontface = NULL,
		# legend.text.fontfamily = NULL,
		# legend.hist.size = 0.7,
		# legend.format = list(
		# 	fun = NULL,
		# 	scientific = FALSE,
		# 	digits = NA,
		# 	big.num.abbr = c(mln = 6, bln = 9),
		# 	prefix = "",
		# 	suffix = "",
		# 	text.separator = "to",
		# 	text.less.than = c("Less", "than"),
		# 	text.or.more = c("or", "more"),
		# 	text.align = NA,
		# 	text.to.columns = FALSE
		# ),
		# legend.frame = FALSE,
		# legend.frame.lwd = 1,
		# legend.bg.color = NA,
		# legend.bg.alpha = 1,
		# legend.hist.bg.color = NA,
		# legend.hist.bg.alpha = 1,
		# title.snap.to.legend = NA,
		# title.position = c("left", "top"),
		# title.color = NULL,
		# title.fontface = NULL,
		# title.fontfamily = NULL,
		# title.bg.color = NA,
		# title.bg.alpha = 1,
		# panel.show = NA,
		# panel.labels = NA,
		# panel.label.size = 1,
		# panel.label.color = "black",
		# panel.label.fontface = NULL,
		# panel.label.fontfamily = NULL,
		# panel.label.bg.color = "grey80",
		# panel.label.height = 1.25,
		# panel.label.rot = c(90, 0),
		# main.title = NA,
		# main.title.size = 1.5,
		# main.title.color = "black",
		# main.title.fontface = NULL,
		# main.title.fontfamily = NULL,
		# main.title.position = "left",
		# attr.outside = FALSE,
		# attr.outside.position = "bottom",
		# attr.outside.size = NA,
		# attr.position = c("right", "bottom"),
		# attr.just = c("left", "bottom"),
		# basemaps = c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldTopoMap"),
		# basemaps.alpha = c(1, 1, 1),
		# overlays = NULL,
		# overlays.alpha = 1,
		# qtm.scalebar = TRUE,
		# qtm.minimap = FALSE,
		# qtm.mouse.coordinates = TRUE,
		# alpha = NA,
		# colorNA = NA,
		# projection = 3857,
		# symbol.size.fixed = FALSE,
		# dot.size.fixed = TRUE,
		# text.size.variable = FALSE,
		# bbox = NULL,
		# set.bounds = FALSE,
		# set.view = NA,
		# set.zoom.limits = NA,
		# view.legend.position = c("right", "top"),
		# control.position = c("left", "top"),
		# leaflet.options = list()
	),
	style = "white"
)

.defaultTmapStyles <- list(
	gray = list(
		bg.color = "grey85",
		aes.color = c(
			fill = "grey70",
			borders = "grey20",
			symbols = "grey50",
			dots = "black",
			lines = "black",
			text = "black",
			na = "grey60",
			null = "grey80"
		)
	),
	grey = list(
		bg.color = "grey85",
		aes.color = c(
			fill = "grey70",
			borders = "grey20",
			symbols = "grey50",
			dots = "black",
			lines = "black",
			text = "black",
			na = "grey60",
			null = "grey80"
		)
	),
	natural = list(
		bg.color = "lightskyblue1",
		aes.color = c(
			fill = "darkolivegreen3",
			borders = "black",
			symbols = "tomato2",
			dots = "firebrick",
			lines = "steelblue",
			text = "black",
			na = "white",
			null = "grey70"
		),
		aes.palette = list(seq = "YlGn", div = "RdYlGn", cat = "Set3"),
		attr.color = "black",
		space.color = "white",
		legend.frame = TRUE,
		legend.bg.color = "grey90",
		earth.boundary = TRUE,
		basemaps = "Esri.NatGeoWorldMap",
		basemaps.alpha = 1
	),
	cobalt = list(
		bg.color = "#002240",
		aes.color = c(
			fill = "#0088FF",
			borders = "#002240",
			symbols = "#FF9D00",
			dots = "#FF9D00",
			lines = "#FFEE80",
			text = "white",
			na = "grey60",
			null = "grey40"
		),
		aes.palette = list(seq = "YlGn", div = "RdYlGn", cat = "Set3"),
		attr.color = "white",
		basemaps = "CartoDB.DarkMatter",
		basemaps.alpha = .5
	),
	col_blind = list(
		bg.color = "white",
		aes.color = c(
			fill = "grey85",
			borders = "black",
			symbols = "#D55E00",
			dots = "#0072B2",
			lines = "#009E73",
			text = "black",
			na = "white",
			null = "grey90"
		),
		aes.palette = list(
			seq = "Blues",
			div = "RdBu",
			cat = c(
				"#D55E00",
				"#56B4E9",
				"#E69F00",
				"#009E73",
				"#F0E442",
				"#0072B2",
				"#CC79A7"
			)
		),
		attr.color = "black"
	),
	albatross = list(
		bg.color = "#00007F",
		aes.color = c(
			fill = "#4C4C88",
			borders = "#00004C",
			symbols = "#BFBFFF",
			dots = "#BFBFFF",
			lines = "#BFBFFF",
			text = "#FFE700",
			na = "grey60",
			null = "#4C4C88"
		),
		aes.palette = list(seq = "YlOrRd", div = "RdYlGn", cat = "Set3"),
		attr.color = "#BFBFFF",
		basemaps = "CartoDB.DarkMatter",
		basemaps.alpha = .5
	),
	beaver = list(
		bg.color = "#FFFFFF",
		aes.color = c(
			fill = "#FFE200",
			borders = "#000000",
			symbols = "#A30000",
			dots = "#A30000",
			lines = "#A30000",
			text = "#000000",
			na = "grey80",
			null = "grey95"
		),
		aes.palette = list(seq = "YlOrBr", div = "RdYlGn", cat = "Dark2"),
		attr.color = "black"
	),
	bw = list(saturation = 0),
	classic = list(
		sepia.intensity = .7,
		fontfamily = "serif",
		frame.double.line = TRUE,
		compass.type = "rose",
		basemaps = "Esri.WorldTopoMap",
		basemaps.alpha = .5
	),
	watercolor = list(
		basemaps = "Stamen.Watercolor",
		aes.color = c(
			fill = "#D95F02",
			borders = "grey20",
			symbols = "#D95F02",
			dots = "red",
			lines = "red",
			text = "black",
			na = "grey80",
			null = "#FDCDAC"
		),
		aes.palette = list(seq = "Greens", div = "PiYG", cat = "Pastel1")
	)
)


.defaultTmapFormats <- list(World = list(inner.margins=c(0, 0.05, 0.025, 0.01),
							  legend.position=c("left", "bottom"), 
							  attr.position=c("right", "bottom"),
							  scale=.8),
				 World_wide = list(inner.margins=c(0, 0.2, 0.025, 0.01),
							  legend.position=c("left", "bottom"), 
							  attr.position=c("right", "bottom"),
							  scale=.8),
				 NLD = list(basemaps = c(Standard = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaart/EPSG:3857/{z}/{x}/{y}.png",
				 						Aerial = "//geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/Actueel_ortho25/EPSG:3857/{z}/{x}/{y}.jpeg",
				 						Pastel = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartpastel/EPSG:3857/{z}/{x}/{y}.png",
				 						Gray   = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartgrijs/EPSG:3857/{z}/{x}/{y}.png"),
				 			frame=FALSE, 
				 		   inner.margins=c(.02, .2, .06, .02),
				 		   legend.position=c("left", "top"), 
				 		   attr.position=c("left", "bottom")),
				 NLD_wide = list(basemaps = c(Standard = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaart/EPSG:3857/{z}/{x}/{y}.png",
				 							 Aerial = "//geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/Actueel_ortho25/EPSG:3857/{z}/{x}/{y}.jpeg",
				 							 Pastel = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartpastel/EPSG:3857/{z}/{x}/{y}.png",
				 							 Gray   = "//geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartgrijs/EPSG:3857/{z}/{x}/{y}.png"),
				 				frame=FALSE, 
				 				inner.margins=c(.02, .3, .06, .02),
				 				legend.position=c("left", "top"), 
				 				attr.position=c("left", "bottom")))
				 



#' Options for tmap
#' 
#' @param ... arguments
#' @example ./examples/tmap_options.R
#' @rdname tmap_options
#' @name tmap_options
#' @export
tmap_options <- function(...) {

	

	.tmapOptions <- get("tmapOptions", envir = .TMAP)	
	show.warnings = .tmapOptions$show.warnings
	
	current.style <- getOption("tmap.style")
	newstyle <- if (substr(current.style, nchar(current.style) - 9, nchar(current.style)) == "(modified)") {
		current.style
	} else paste(current.style, "(modified)")
	
	
	optnames <- names(.tmapOptions)
	
	e1 <- parent.frame()
	
	set_new_style <- FALSE
	
	lst <- list(...)
	if (length(lst) >= 1 && is.null(names(lst))) {
		arg <- lst[[1]]
		if (is.list(arg)) {
			## case 1: option list is given
			args <- arg
			
			style_attr <- attr(args, "style")
			if (!is.null(style_attr)) {
				newstyle <- style_attr
				set_new_style <- TRUE
			}
			
			if (length(lst) > 1 && show.warnings) warning("The first argument is used, but the other arguments are ignored.")
		} else {
			## case 2: option name is given
			args <- sapply(lst, "[", 1)
			if (!all(args %in% optnames) && show.warnings) warning("the following options do not exist: ", paste(setdiff(args, optnames), collapse = ", "))
			args <- intersect(args, optnames)
			return(.tmapOptions[args])
		}
	} else {
		## case 3: named options are set
		## case 4: tmap_options is called without arguments
		args <- lapply(as.list(match.call()[-1]), eval, envir = e1)	
	}
	
	unknown_args <- setdiff(names(args), names(.defaultTmapOptions))
	if (length(unknown_args) == 1) {
		stop("the following option does not exist: ", unknown_args)
	} else if (length(unknown_args) > 1) {
		stop("the following options do not exist: ", paste(unknown_args, collapse = ", "))
	}
	
	if (!length(args)) {
		# case 4
		return(.tmapOptions)	
	} else {
		# case 1 and 3
		backup <- .tmapOptions[names(args)]
		.tmapOptions[names(args)] <- check_named_items(args, backup)
		
		options(tmap.style=newstyle)
		attr(.tmapOptions, "style") <- newstyle
		assign("tmapOptions", .tmapOptions, envir = .TMAP)
		
		if (set_new_style) {
			if (.tmapOptions$show.messages) message("tmap options successfully loaded as style \"", newstyle, "\"")
			styles <- get("tmapStyles", envir = .TMAP)
			styles[[newstyle]] <- suppressMessages(tmap_options_diff())
			assign("tmapStyles", styles, envir = .TMAP)
		} 
		
		invisible(backup)
	}
}


## function to check named items (such as max.raster and legend.format)
check_named_items <- function(a, b) {
	named_items <- which(vapply(b, FUN = function(i) !is.null(names(i)), FUN.VALUE = logical(1)))
	
	dynamic_vec_names <- c("basemaps", "overlays")
	
	show.warnings = get("tmapOptions", envir = .TMAP)$show.warnings
	
	if (length(named_items) != 0L) {
		a[named_items] <- mapply(function(an, bn, nm) {
			if (nm %in% dynamic_vec_names) {
				an
			} else {
				res <- bn
				cls <- ifelse(is.list(bn), "list", "vector")
				if (is.null(names(an))) {
					if (show.warnings) warning("tmap option ", nm, " requires a named ", cls, call. = FALSE)
				} else if (!all(names(an) %in% names(bn))) {
					formatC_names <- setdiff(names(formals(formatC)), "x")
					if (nm == "legend.format") {
						invalid <- setdiff(names(an), c(names(bn), formatC_names))
					} else {
						invalid <- setdiff(names(an), names(bn))
					}
					
					if (length(invalid) > 0 && show.warnings) warning("invalid ", cls, " names of tmap option ", nm, ": ", paste(invalid, collapse = ", "), call. = FALSE)
					
				}
				res[names(an)] <- an
				res
			}
		},a[named_items], b[named_items], names(b[named_items]), SIMPLIFY = FALSE)
	}
	a
}


#' @rdname tmap_options
#' @export
tm_options <- function(...) {
	
	calls <- names(match.call(expand.dots = TRUE)[-1])

	e1 <- parent.frame()
	args <- lapply(as.list(match.call()[-1]), eval, envir = e1)

	tm_element_list(do.call(tm_element, c(args, list(calls = calls, subclass= "tm_options"))))
	
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

#' #' @export
#' #' @rdname tmap_style
#' #' @param x tmap options list (should be the same format as \code{tmap_options()})
#' tmap_style_load <- function(x) {
#' 	style <- attr(x, "style")
#' 	attr(x, "style") <- NULL
#' 	styles <- get("tmapStyles", envir = .TMAP)
#' 	styles[[style]] <- x
#' 	assign("tmapStyles", styles, envir = .TMAP)
#' 	if (get("tmapOptions", envir = .TMAP)$show.messages) message("style \"", style, "\" loaded successfully")
#' 	invisible(NULL)
#' }
