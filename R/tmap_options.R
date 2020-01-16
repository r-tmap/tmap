.defaultTmapOptions <- structure(
	list(
		unit = "metric",
		limits = c(facets.plot = 64, facets.view = 4),
		max.categories = 30,
		max.raster = c(plot = 1e7, view = 1e6),
		show.messages = TRUE,
		output.format = "png",
		output.size = 49,
		output.dpi = 300,
		title = NA,
		scale = 1,
		title.size = 1.3,
		bg.color = "white",
		aes.color = c(
			fill = "grey85",
			borders = "grey40",
			symbols = "grey60",
			dots = "black",
			lines = "black",
			text = "black",
			na = "grey75",
			null = "grey95"
		),
		aes.palette = list(seq = "YlOrBr", div = "RdYlGn", cat = "Set3"),
		attr.color = "black",
		sepia.intensity = 0,
		saturation = 1,
		frame = TRUE,
		frame.lwd = 1,
		frame.double.line = FALSE,
		asp = NA,
		outer.margins = rep(0.02, 4),
		inner.margins = NA,
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
		legend.outside = NA,
		legend.outside.position = "right",
		legend.outside.size = 0.3,
		legend.position = NULL,
		legend.stack = "vertical",
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
		design.mode = FALSE,
		basemaps = c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldTopoMap"),
		basemaps.alpha = c(1, 1, 1),
		overlays = NULL,
		overlays.alpha = 1,
		qtm.scalebar = TRUE,
		qtm.minimap = FALSE,
		alpha = NA,
		colorNA = NA,
		projection = 3857,
		symbol.size.fixed = FALSE,
		dot.size.fixed = TRUE,
		text.size.variable = FALSE,
		bbox = NULL,
		set.bounds = FALSE,
		set.view = NA,
		set.zoom.limits = NA,
		view.legend.position = c("right", "top"),
		control.position = c("left", "top"),
		popup.all.data = NULL
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
#' Get or set global options for tmap. The behaviour of \code{tmap_options} is similar to \code{\link[base:options]{options}}: all tmap options are retrieved when this function is called without arguments. When arguments are specified, the corresponding options are set, and the old values are silently returned as a list. The function \code{tmap_options_reset} is used to reset all options back to the default values (also the \code{style} is reset to \code{"white"}). Differences with the default values can be shown with \code{tmap_options_diff}. The function \code{tmap_options_save} can be used to save the current options as a new style. See details below on how to create a new style.
#' 
#' The options can be divided into three parts: one part contains the arguments from \code{\link{tm_layout}}, one part contains the arguments from \code{\link{tm_view}}, and one part contains options that can only be set with \code{tmap_options}. Observe that the options from \code{\link{tm_layout}} and \code{\link{tm_view}} can also be set with those functions. It is recommended to use \code{tmap_options} when setting specific options during global session. However, options that are only relevant for a specific map can better be set with \code{\link{tm_layout}} or \code{\link{tm_view}}.
#' 
#' A new style can be created in two ways. The first approach is to use the function \code{tmap_options_save}, which takes a snapshot of the current tmap options. E.g., \code{tmap_options_save("my_style")} will save the current tmap options as a style called \code{"my_style"}. See the examples in which a style called \code{"red"} is created. The second way to create a style is to create a list with tmap options and with a attribute called style. This approach is illustrated in the last example, in which a style called \code{"black"} is created.
#' 
#' The newly created style, say \code{"my_style"}, will be accessible globally via \code{tmap_style("my_style")} and \code{+ tm_style("my_style")} until the R session is restarted or \code{tmap} is reloaded. In order to save the style for future use or sharing, obtain the option list as follows: \code{my_style <- tmap_options()} and save the object \code{my_style} in the usual way. Next time, the style can be loaded simply by running \code{tmap_options(my_style)}, which corresponds to the second way to create a style (see the paragraph above).
#' 
#' @param ...  options from \code{\link{tm_layout}} or \code{\link{tm_view}}. Note that the difference with using \code{\link{tm_layout}} or \code{\link{tm_view}} directly, is that options set with \code{tmap_options} remain for the entire session (unless changed with \code{tmap_options} or \code{\link{tmap_style}}). It can also be a single unnamed argument which is a named list of options (similar behaviour as \code{\link[base:options]{options}}).
#' @param unit this is the default value for the \code{unit} argument of \code{\link{tm_shape}}. It specifies the unit of measurement, which is used in the scale bar and the calculation of density values. By default (when loading the package), it is \code{"metric"}. Other valid values are \code{"imperial"}, \code{"km"}, \code{"m"}, \code{"mi"}, and \code{"ft"}.
#' @param limits this option determines how many facets (small multiples) are allowed for per mode. It should be a vector of two numeric values named \code{facets.view} and \code{facets.plot}. By default (i.e. when loading the package), it is set to \code{c(facets.view = 4, facets.plot = 64)}
#' @param max.categories in case \code{col} is the name of a categorical variable in the layer functions (e.g. \code{\link{tm_polygons}}), this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories}, then levels are combined.
#' @param max.raster the maximum size of rasters, in terms of number of raster cells. It should be a vector of two numeric values named \code{plot} and \code{view}, which determines the size in plotting and viewing mode. The default values are \code{c(plot = 1e7, view = 1e6)}. Rasters that are larger will be shown at a decreased resolution.
#' @param basemaps default basemaps. Basemaps are normally configured with \code{\link{tm_basemap}}. When this is not done, the basemaps specified by this option are shown (in view mode). Vector of one or more names of baselayer maps, or \code{NULL} if basemaps should be omitted. For options see the list \code{leaflet::providers}, which can be previewed at \url{http://leaflet-extras.github.io/leaflet-providers/preview}. Also supports URL's for tile servers, such as \code{"http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"}. If a named vector is provided, the names are used in the layer control legend (similar to the \code{group} argument of \code{\link{tm_basemap}}. See also \code{overlays}, which is the default option for overlay tiles.
#' @param basemaps.alpha default transparency (opacity) value for the basemaps. Can be a vector of values, one for each basemap.
#' @param overlays default overlay tilemaps. Overlays tilemaps are shown as front layer (in contrast to basemaps, which are background layers), so they are only useful when they are semi-transparent. Like basemaps, a vector of tilemaps is expected, or \code{NULL} is overlays should be omitted.
#' @param overlays.alpha default transparency (opacity) value for the overlay maps. Can be a vector of values, one for each overlay map.
#' @param qtm.scalebar should a scale bar be added to interactive maps created with \code{\link{qtm}}. In other words, should \code{tm_scale_bar()} be added automatically? The value \code{NA} means that the scale bar is only added when \code{\link{qtm}} is called without arguments or with a search term. The default value is \code{TRUE}.
#' @param qtm.minimap should a minimap be added to interactive maps created with \code{\link{qtm}}. In other words, should \code{tm_minimap()} be added automatically? The value \code{NA} means that the minimap is only added in navigation mode (i.e. when \code{\link{qtm}} is called without arguments or with a search term. The default value is \code{FALSE}.
#' @param show.messages should messages be shown?
#' @param output.format The format of the static maps saved with \code{\link{tmap_save}} without specification of the filename. The default is \code{"png"}.
#' @param output.size The size of the static maps saved with \code{\link{tmap_save}} without specification of width and height. The unit is squared inch and the default is 49. This means that square maps (so with aspect ratio 1) will be saved as 7 by 7 inch images and a map with aspect ratio 2 (e.g. most world maps) will be saved as approximately 10 by 5 inch.
#' @param output.dpi The default number of dots per inch for \code{\link{tmap_save}} and \code{\link{tmap_animation}}.
#' @param style style name
#' @example ./examples/tmap_options.R
#' @rdname tmap_options
#' @name tmap_options
#' @export
#' @seealso \code{\link{tm_layout}}, \code{\link{tm_view}}, and \code{\link{tmap_style}}
tmap_options <- function(..., unit, limits, max.categories, max.raster, basemaps, basemaps.alpha, overlays, overlays.alpha, qtm.scalebar, qtm.minimap, show.messages, output.format, output.size, output.dpi) {

	.tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)	
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
			
			if (length(lst) > 1) warning("The first argument is used, but the other arguments are ignored.")
		} else {
			## case 2: option name is given
			args <- sapply(lst, "[", 1)
			if (!all(args %in% optnames)) warning("the following options do not exist: ", paste(setdiff(args, optnames), collapse = ", "))
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
		assign("tmapOptions", .tmapOptions, envir = .TMAP_CACHE)
		
		if (set_new_style) {
			if (.tmapOptions$show.messages) message("tmap options successfully loaded as style \"", newstyle, "\"")
			styles <- get("tmapStyles", envir = .TMAP_CACHE)
			styles[[newstyle]] <- suppressMessages(tmap_options_diff())
			assign("tmapStyles", styles, envir = .TMAP_CACHE)
		} 
		
		invisible(backup)
	}
}


## function to check named items (such as max.raster and legend.format)
check_named_items <- function(a, b) {
	named_items <- which(vapply(b, FUN = function(i) !is.null(names(i)), FUN.VALUE = logical(1)))
	
	dynamic_vec_names <- c("basemaps", "overlays")
	
	if (length(named_items) != 0L) {
		a[named_items] <- mapply(function(an, bn, nm) {
			if (nm %in% dynamic_vec_names) {
				an
			} else {
				res <- bn
				cls <- ifelse(is.list(bn), "list", "vector")
				if (is.null(names(an))) {
					warning("tmap option ", nm, " requires a named ", cls, call. = FALSE)
				} else if (!all(names(an) %in% names(bn))) {
					formatC_names <- setdiff(names(formals(formatC)), "x")
					if (nm == "legend.format") {
						invalid <- setdiff(names(an), c(names(bn), formatC_names))
					} else {
						invalid <- setdiff(names(an), names(bn))
					}
					if (length(invalid) > 0) warning("invalid ", cls, " names of tmap option ", nm, ": ", paste(invalid, collapse = ", "), call. = FALSE)
					
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
tmap_options_diff <- function() {
	.tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)	
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
	assign("tmapOptions", .defaultTmapOptions, envir = .TMAP_CACHE)
	options(tmap.style="white")
	message("tmap options successfully reset")
	invisible(NULL)
}

#' @export
#' @rdname tmap_options
tmap_options_save <- function(style) {
	show.messages <- get("tmapOptions", envir = .TMAP_CACHE)$show.messages
	
	stylediff <- suppressMessages(tmap_options_diff())
	
	.tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)	
	
	if (is.null(stylediff)) {
		if (show.messages) message("current style is the same as the default style, so nothing to save")
		return(invisible(.tmapOptions))
	}
	
	options(tmap.style=style)
	attr(.tmapOptions, "style") <- style
	assign("tmapOptions", .tmapOptions, envir = .TMAP_CACHE)
	
	styles <- get("tmapStyles", envir = .TMAP_CACHE)
	styles[[style]] <- suppressMessages(tmap_options_diff())
	assign("tmapStyles", styles, envir = .TMAP_CACHE)
	
	if (show.messages) message("current tmap options saved as style \"", style, "\"")
	invisible(.tmapOptions)
}

#' #' @export
#' #' @rdname tmap_style
#' #' @param x tmap options list (should be the same format as \code{tmap_options()})
#' tmap_style_load <- function(x) {
#' 	style <- attr(x, "style")
#' 	attr(x, "style") <- NULL
#' 	styles <- get("tmapStyles", envir = .TMAP_CACHE)
#' 	styles[[style]] <- x
#' 	assign("tmapStyles", styles, envir = .TMAP_CACHE)
#' 	if (get("tmapOptions", envir = .TMAP_CACHE)$show.messages) message("style \"", style, "\" loaded successfully")
#' 	invisible(NULL)
#' }
