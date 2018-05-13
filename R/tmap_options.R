.defaultTmapOptions <- structure(list(
					unit="metric",
					limits=c(facets.plot=64, facets.view=4),
					 title=NA,
					 scale=1,
					 title.size=1.3,
					 bg.color= "white",
					 aes.color=c(fill="grey85", borders="grey40", symbols="grey60", dots="black", lines="black", text="black", na="grey75", null="grey95"),
					 aes.palette=list(seq="YlOrBr", div="RdYlGn", cat="Set3"),
					 attr.color="black",
					 sepia.intensity=0, 
					 saturation=1, 
					 frame=TRUE,
					 frame.lwd=1,
					 frame.double.line=FALSE,
					 asp = NA,
					 outer.margins = rep(0.02, 4),
					 inner.margins = NA,
					 between.margin = .5,
					 outer.bg.color=NULL,
					 fontface="plain", 
					 fontfamily="",
					 compass.type="arrow",
					 earth.boundary=FALSE,
					 earth.boundary.color=NULL, #attr.color,
					 earth.boundary.lwd=1,
					 earth.datum="WGS84",
					 space.color=NULL,
					 legend.show = TRUE,
					 legend.only = FALSE,
					 legend.outside=NA,
					 legend.outside.position="right",
					 legend.outside.size=0.3,
					 legend.position = NULL,
					 legend.stack = "vertical",
					 legend.just = c("left", "bottom"),
					 legend.width = 0.4,
					 legend.height = 0.9,
					 legend.hist.height = 0.3,
					 legend.hist.width = 0.4, #legend.width,
					 legend.title.size=1.1,
					 legend.text.size=0.7,
					 legend.hist.size=0.7,
					 legend.format=list(fun=NULL, scientific = FALSE, digits= NA,
					 				   text.separator = "to", 
					 				   text.less.than = c("Less", "than"),
					 				   text.or.more = c("or", "more"),
					 				   text.align = NA,
					 				   text.to.columns = FALSE),
					 legend.frame = FALSE,
					 legend.frame.lwd = 1,
					 legend.text.color = NULL, #attr.color,
					 legend.bg.color = NA,
					 legend.bg.alpha = 1,
					 legend.hist.bg.color = NA,
					 legend.hist.bg.alpha = 1,
					 title.snap.to.legend = NA,
					 title.position = c("left", "top"),
					 title.color=NULL, #attr.color,
					 title.bg.color=NA,
					 title.bg.alpha = 1,
					 panel.show = NA,
					 panel.labels=NA,
					 panel.label.size = 1,
					 panel.label.color = "black",
					 panel.label.bg.color = "grey80",
					 panel.label.height = 1.25,
					 panel.label.rot = c(90, 0),
					 main.title = NA,
					 main.title.size = 1.5,
					 main.title.color = "black",
					 main.title.position = "left",
					 attr.outside = FALSE,
					 attr.outside.position = "bottom",
					 attr.outside.size=NA,
					 attr.position = c("right", "bottom"),
					 attr.just = c("left", "bottom"),
					 design.mode = FALSE,
					 basemaps = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldTopoMap"),
					 basemaps.alpha = c(1, 1, 1),
					base.groups=NA,
					overlay.groups=NA,
					alpha=NA,
					colorNA=NA,
					projection=3857,
					symbol.size.fixed=FALSE,
					dot.size.fixed=TRUE,
					text.size.variable=FALSE,
					bbox = NULL,
					set.bounds=FALSE,
					set.view=NA,
					set.zoom.limits=NA,
					view.legend.position=c("right", "top"),
					control.position=c("left", "top"),
					popup.all.data=NULL),
					style = "white")

.defaultTmapStyles <- list(gray = list(bg.color="grey85", 
								aes.color=c(fill="grey70", borders="grey20", symbols="grey50", dots="black", lines="black", text="black", na="grey60", null="grey80")),
					grey = list(bg.color="grey85", 
								aes.color=c(fill="grey70", borders="grey20", symbols="grey50", dots="black", lines="black", text="black", na="grey60", null="grey80")),
					natural = list(bg.color="lightskyblue1",
								   aes.color=c(fill="darkolivegreen3", borders="black", symbols="tomato2", dots="firebrick", lines="steelblue", text="black", na="white", null="grey70"),
								   aes.palette=list(seq="YlGn", div="RdYlGn", cat="Set3"),
								   attr.color="black",
								   space.color="white",
								   legend.frame=TRUE,
								   legend.bg.color="grey90",
								   earth.boundary=TRUE,
								   basemaps="Esri.NatGeoWorldMap",
								   basemaps.alpha=1),
					cobalt = list(bg.color="#002240",
								  aes.color=c(fill="#0088FF", borders="#002240", symbols="#FF9D00", dots="#FF9D00", lines="#FFEE80", text="white", na="grey60", null="grey40"),
								  aes.palette=list(seq="YlGn", div="RdYlGn", cat="Set3"),
								  attr.color="white", 
								  basemaps="CartoDB.DarkMatter",
								  basemaps.alpha=.5),
					col_blind = list(bg.color="white",
									 aes.color=c(fill="grey85", borders="black", symbols="#D55E00", dots="#0072B2", lines="#009E73", text="black", na="white", null="grey90"),
									 aes.palette=list(seq="Blues", div="RdBu", cat=c("#D55E00", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2","#CC79A7")),
									 attr.color="black"),
					albatross = list(bg.color="#00007F",
									aes.color=c(fill="#4C4C88", borders="#00004C", symbols="#BFBFFF", dots="#BFBFFF", lines="#BFBFFF", text="#FFE700", na="grey60", null="#4C4C88"),
									aes.palette=list(seq="YlOrRd", div="RdYlGn", cat="Set3"),
									attr.color="#BFBFFF",
									basemaps="CartoDB.DarkMatter",
									basemaps.alpha=.5),
					beaver = list(bg.color="#FFFFFF",
								  aes.color=c(fill="#FFE200", borders="#000000", symbols="#A30000", dots="#A30000", lines="#A30000", text="#000000", na="grey80", null="grey95"),
								  aes.palette=list(seq="YlOrBr", div="RdYlGn", cat="Dark2"),
								  attr.color="black"),
					bw = list(saturation=0),
					classic = list(sepia.intensity=.7, 
								   fontfamily="serif", 
								   frame.double.line=TRUE, 
								   compass.type="rose",
								   basemaps="Esri.WorldTopoMap",
								   basemaps.alpha=.5))


.defaultTmapFormats <- list(World = list(inner.margins=c(0, 0.05, 0.025, 0.01),
							  legend.position=c("left", "bottom"), 
							  attr.position=c("right", "bottom"),
							  scale=.8),
				 World_wide = list(inner.margins=c(0, 0.2, 0.025, 0.01),
							  legend.position=c("left", "bottom"), 
							  attr.position=c("right", "bottom"),
							  scale=.8),
				 NLD = list(frame=FALSE, 
				 		   inner.margins=c(.02, .2, .06, .02),
				 		   legend.position=c("left", "top"), 
				 		   attr.position=c("left", "bottom")),
				 NLD_wide = list(frame=FALSE, 
				 				inner.margins=c(.02, .3, .06, .02),
				 				legend.position=c("left", "top"), 
				 				attr.position=c("left", "bottom")))
				 

#' Options for tmap
#' 
#' Get or set global options for tmap. The behaviour of \code{tmap_options} is similar to \code{\link[base:options]{options}}: all tmap options are retrieved when this function is called without arguments. When arguments are specified, the corresponding options are set, and the old values are silently returned. The function \code{tmap_options_reset} is used to reset all options back to the default values (also the \code{style} is reset to \code{"white"}). Differences with the default values can be shown with \code{tmap_options_diff}.
#' 
#' @param unit This is the default value for the \code{unit} argument of \code{\link{tm_shape}}. It specifies the unit of measurement, which is used in the scale bar and the calculation of density values. By default (when loading the package), it is \code{"metric"}. Other valid values are \code{"imperial"}, \code{"km"}, \code{"m"}, \code{"mi"}, and \code{"ft"}.
#' @param limits This option determines how many facets (small multiples) are allowed for per mode. It should be a vector of two numeric values named \code{facets.view} and \code{facets.plot}. By default (i.e. when loading the package), it is set to \code{c(facets.view = 4, facets.plot = 64)}
#' @param ...  options from \code{\link{tm_layout}} or \code{\link{tm_view}}. Note that the difference with using \code{\link{tm_layout}} or \code{\link{tm_view}} directly, is that options set with \code{tmap_options} remain for the entire session (unless changed with \code{tmap_options} or \code{\link{tmap_style}}).
#' @example ./examples/tmap_options.R
#' @rdname tmap_options
#' @name tmap_options
#' @export
#' @seealso \code{\link{tm_layout}}, \code{\link{tm_view}}, and \code{\link{tmap_style}}
tmap_options <- function(unit, limits, ...) {

	.tmapOptions <- get(".tmapOptions", envir = .TMAP_CACHE)	
	current.style <- getOption("tmap.style")
	
	
	e1 <- parent.frame()
	args <- lapply(as.list(match.call()[-1]), eval, envir = e1)
	
	unknown_args <- setdiff(names(args), names(.defaultTmapOptions))
	if (length(unknown_args) == 1) {
		stop("the following option does not exist: ", unknown_args)
	} else if (length(unknown_args) > 1) {
		stop("the following options do not exist: ", paste(unknown_args, collapse = ", "))
	}
	
	
	if (!length(args)) {
		return(.tmapOptions)	
	} else {
		backup <- .tmapOptions
		.tmapOptions[names(args)] <- args
		
		newstyle <- if (substr(current.style, nchar(current.style) - 9, nchar(current.style)) == "(modified)") {
			current.style
		} else paste(current.style, "(modified)")	
	
		options(tmap.style=newstyle)
		attr(.tmapOptions, "style") <- newstyle
		assign(".tmapOptions", .tmapOptions, envir = .TMAP_CACHE)
		invisible(backup)
	}
}



#' @rdname tmap_options
#' @export
tmap_options_diff <- function() {
	.tmapOptions <- get(".tmapOptions", envir = .TMAP_CACHE)	
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
	assign(".tmapOptions", .defaultTmapOptions, envir = .TMAP_CACHE)
	options(tmap.style="white")
	invisible(NULL)
}
