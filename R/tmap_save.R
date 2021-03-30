#' Save tmap
#' 
#' Save tmap to a file. This can be either a static plot (e.g. png) or an interactive map (html).
#'
#' @param tm tmap object
#' @param filename filename including extension, and optionally the path. The extensions pdf, eps, svg, wmf (Windows only), png, jpg, bmp, tiff, and html are supported. If the extension is missing, the file will be saved as a static plot in \code{"plot"} mode and as an interactive map (html) in \code{"view"} mode (see details). The default format for static plots is png, but this can be changed using the option \code{"output.format"} in \code{\link{tmap_options}}. If \code{NA} (the default), the file is saved as "tmap01" in the default format, and the number incremented if the file already exists.
#' @param height,width The width and height of the plot (not applicable for html files). Units are set with the argument \code{units}. If one of them is not specified, this is calculated using the formula asp = width / height, where asp is the estimated aspect ratio of the map. If both are missing, they are set such that width * height is equal to the option \code{"output.size"} in \code{\link{tmap_options}}. This is by default 49, meaning that is the map is a square (so aspect ratio of 1) both width and height are set to 7.
#' @param units units for width and height (\code{"in"}, \code{"cm"}, or \code{"mm"}). By default, pixels (\code{"px"}) are used if either width or height is set to a value greater than 50. Else, the units are inches (\code{"in"})
#' @param dpi dots per inch. Only applicable for raster graphics. By default it is set to 300, but this can be changed using the option \code{"output.dpi"} in \code{\link{tmap_options}}.
#' @param outer.margins overrides the outer.margins argument of \code{\link{tm_layout}} (unless set to \code{NA})
#' @param asp if specified, it overrides the asp argument of \code{\link{tm_layout}}. Tip: set to \code{0} if map frame should be placed on the edges of the image.
#' @param scale overrides the scale argument of \code{\link{tm_layout}} (unless set to \code{NA})
#' @param insets_tm tmap object of an inset map, or a list of tmap objects of multiple inset maps. The number of tmap objects should be equal to the number of viewports specified with \code{insets_vp}.
#' @param insets_vp \code{\link[grid:viewport]{viewport}} of an inset map, or a list of \code{\link[grid:viewport]{viewport}}s of multiple inset maps. The number of viewports should be equal to the number of tmap objects specified with \code{insets_tm}.
#' @param add.titles add titles to leaflet object
#' @param in.iframe should an interactive map be saved as an iframe? If so, two HTML files will be saved; one small parent HTML file with the iframe container, and one large child HTML file with the actual widget. See \code{\link[widgetframe:saveWidgetframe]{saveWidgetframe}} for details. By default \code{FALSE} which means that one large HTML file is saved (see \code{\link[htmlwidgets:saveWidget]{saveWidget}}).
#' @param selfcontained when an interactive map is saved, should the resources (e.g. Javascript libraries) be contained in the HTML file? If \code{FALSE}, they are placed in an adjacent directory (see also \code{\link[htmlwidgets:saveWidget]{saveWidget}}). Note that the HTML file will often still be large when \code{selfcontained = FALSE}, since the map data (polygons and popups), which are also contained in the HTML file, usually take more space then the map resources.
#' @param verbose Deprecated. It is now controlled by the tmap option \code{show.messages} (see \code{\link{tmap_options}})
#' @param ... arguments passed on to device functions or to \code{\link[htmlwidgets:saveWidget]{saveWidget}} or \code{\link[widgetframe:saveWidgetframe]{saveWidgetframe}}
#' @importFrom htmlwidgets saveWidget
#' @import tmaptools
#' @example ./examples/tmap_save.R
#' @export
tmap_save <- function(tm=NULL, filename=NA, width=NA, height=NA, units = NA,
					  dpi=NA, outer.margins=NA, asp=NULL, scale=NA, insets_tm=NULL, insets_vp=NULL, add.titles = TRUE, in.iframe = FALSE, selfcontained = !in.iframe, verbose = NULL, ...) {
	.tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)
	show.warnings <-.tmapOptions$show.warnings
	if (!missing(verbose) && show.warnings) warning("The argument verbose is deprecated. Please use the option show.messages of tmap_options instead.")
	
	verbose <- .tmapOptions$show.messages
	
	
	lastcall <- x <- get("last_map", envir = .TMAP_CACHE)
	if (missing(tm)) {
		tm <- suppressWarnings(tmap_last())
		if (is.null(tm)) stop("A map has not been created yet")
		is.arrange <- FALSE
	} else if (inherits(tm, "tmap")) {
		is.arrange <- FALSE
	} else if (inherits(tm, "tmap_arrange")) {
		is.arrange <- TRUE
	} else {
		stop("Unknown format. tm should be either a tmap output, or a list of tmap outputs")
	}
	
	tmap.mode <- getOption("tmap.mode")
	default_ext <- ifelse(tmap.mode == "plot", .tmapOptions$output.format, "html")

	
	if (is.na(filename)) {
		filename_default <- paste("tmap01", default_ext, sep = ".")
		if (!file.exists(filename_default)) {
			filename <- filename_default
		} else {
			files <- list.files(pattern = paste0("^tmap[0-9]{2}\\.", default_ext, "$"))
			fid <- max(as.integer(substr(files, 5, 6))) + 1L
			filename <- paste0("tmap", sprintf("%02d", fid), ".",  default_ext)
		}
	}
	
	if (is.na(dpi)) dpi <- .tmapOptions$output.dpi
		
	on.exit({
		assign("last_map", lastcall, envir = .TMAP_CACHE)
	})
	
	
	
	get_ext <- function(filename, default_ext) {
		pieces <- strsplit(filename, "\\.")[[1]]
		if (length(pieces)==1) return(default_ext)
		tolower(pieces[length(pieces)])
	}
	
	convert_to_inches <- function(x, units) {
		x <- switch(units, px = x/dpi, `in` = x, cm = x/2.54, mm = x/2.54/10)
	}
	convert_to_pixels <- function(x, units) {
		x <- switch(units, px = x, `in` = dpi*x, cm = dpi*x/2.54, mm = dpi*x/2.54/10)
	}
	
	ext <- get_ext(filename, default_ext)
	
	interactive <- (ext=="html")
	
	options(tmap.mode=ifelse(interactive, "view", "plot"))
	
	if (interactive) {
		if (is.arrange) {
			lf <- print_tmap_arrange(tm, show = FALSE, add.titles=add.titles)
		} else {
			lf <- tmap_leaflet(tm, add.titles = add.titles)
		}
			
		tryCatch({
			wd <- getwd()
			on.exit(setwd(wd), add = TRUE)
			wd_new <- dirname(filename)
			base_filename <- basename(filename)
			setwd(wd_new)
			if (in.iframe) {
				widgetframe::saveWidgetframe(lf, file=base_filename, selfcontained = selfcontained, ...)
			} else {
				htmlwidgets::saveWidget(lf, file=base_filename, selfcontained = selfcontained, ...)
			}
		}, error = function(e) {
			stop("Unable to save the interactive map. Note that saving interactive small multiples is not supported yet. The error message from htmlwidgets::saveWidget is ", call. = FALSE)
		})
		
		options(tmap.mode=tmap.mode)
		if (verbose) {
			message("Interactive map saved to ", suppressWarnings(normalizePath(filename)))
		}
		return(invisible())
	}
	## impute missing w or h
	# if (is.na(width) && is.na(height)) {
	# 	
	# 	
	# 	
	# 	width <- dev.size()[1]
	# 	height <- dev.size()[2]
	# 	if (is.na(units)) units <- "in"
	if (is.na(width) || is.na(height)) {
		if (!is.na(width)) {
			if (is.na(units)) units = choose_unit(width)
			temp_size <- convert_to_pixels(width, units)
		} else if (!is.na(height)) {
			if (is.na(units)) units = choose_unit(height)
			temp_size <- convert_to_pixels(height, units)
		} else {
			units <- "px"
			temp_size <- 700
		}
		
		if (is.arrange) {
			sasp <- 1
		} else {
			show.messages <- tmap_options(show.messages = FALSE)
			on.exit(tmap_options(show.messages))
			sasp <- get_asp_ratio(tm, width = temp_size, height = temp_size, res = dpi)	
			tmap_options(show.messages)
		}
		
		if (is.na(width) && !is.na(height)) {
			width <- height * sasp
		} else if (is.na(height)  && !is.na(width)) {
			height <- width / sasp
		} else {
			units <- "in"
			height <- sqrt(.tmapOptions$output.size / sasp)
			width <- height * sasp
		}
	} else {
		if (is.na(units)) units = choose_unit(max(width, height))
	}
	units_target <- ifelse(units=="px" && ext %in% c("png", "jpg", "jpeg", "bmp", "tiff"), "px", "in")
	
		
	eps <- ps <- function(..., width, height) grDevices::postscript(..., 
																	width = width, height = height, onefile = FALSE, horizontal = FALSE, 
																	paper = "special")
	tex <- function(..., width, height) grDevices::pictex(..., 
														  width = width, height = height)
	pdf <- function(..., version = "1.4") grDevices::pdf(..., 
														 version = version)
	svg <- function(...) grDevices::svg(...)
	wmf <- function(..., width, height) grDevices::win.metafile(..., 
																width = width, height = height)
	emf <- function(..., width, height) grDevices::win.metafile(..., 
																width = width, height = height)
	png <- function(..., width, height) grDevices::png(..., width = width, 
													   height = height, res = dpi, units = units_target)
	jpg <- jpeg <- function(..., width, height) grDevices::jpeg(..., 
																width = width, height = height, res = dpi, units = units_target)
	bmp <- function(..., width, height) grDevices::bmp(..., width = width, 
													   height = height, res = dpi, units = units_target)
	tiff <- function(..., width, height) grDevices::tiff(..., 
														 width = width, height = height, res = dpi, units = units_target)

	if (units_target=="in") {
	  width <- convert_to_inches(width, units)
	  height <- convert_to_inches(height, units)
	  
	  if (ext=="pdf") {
	    round_to_1_72 <- function(x) x %/% (1/72) / 72
	    width <- round_to_1_72(width)
	    height <- round_to_1_72(height)
	  }
	} else {
		width <- convert_to_pixels(width, units)
		height <- convert_to_pixels(height, units)
	}
	
	do.call(ext, args = c(list(file = filename, width = width, height = height), list(...)))
#	curdev <- dev.cur()
	
	on.exit({
		capture.output(dev.off())
	}, add = TRUE)
	
	if (is.arrange) {
		opts <- attr(tm, "opts")
		if (!is.na(outer.margins[1])) opts$outer.margins <- outer.margins
		if (!missing(asp)) opts$asp <- asp
		
		attr(tm, "opts") <- opts
		print(tm)
	} else {
		args <- list()
		if (!is.na(outer.margins[1])) args$outer.margins <- outer.margins
		if (!missing(asp)) args$asp <- asp
		if (!is.na(scale)) args$scale <- scale
		print(tm + do.call("tm_layout", args))
	}

	if (!is.arrange && !missing(insets_tm) && !missing(insets_vp)) {
	  args_inset <- if (!is.na(scale)) list(scale = scale) else list()
	  if (class(insets_tm)=="list" && class(insets_vp)=="list") {
	    if (length(insets_tm) != length(insets_vp)) stop("Number of insets unequal to number of viewports")
	    mapply(function(tm_i, vp_i) {
	      print(tm_i + do.call("tm_layout", args_inset), vp=vp_i)
	    }, insets_tm, insets_vp)
	  } else if (inherits(insets_tm, "tmap") && inherits(insets_vp, "viewport")) {
	    print(insets_tm + do.call("tm_layout", args_inset), insets_vp)
	  } else {
	    stop("Insets and/or its viewports not in the correct format")
	  }
	}
	
	if (verbose) {
		message("Map saved to ", suppressWarnings(normalizePath(filename)))
		if (ext %in% c("png", "jpg", "jpeg", "bmp", "tiff")) {
			if (units_target == "px") {
				wp <- format(width)
				hp <- format(height)
				wi <- format(convert_to_inches(width, "px"))
				hi <- format(convert_to_inches(height, "px"))
			} else {
				wi <- format(width)
				hi <- format(height)
				wp <- format(convert_to_pixels(width, "in"))
				hp <- format(convert_to_pixels(height, "in"))
			}
			message("Resolution: ", format(wp), " by ", format(hp), " pixels") 
			message("Size: ", wi, " by ", hi, " inches (", format(dpi), " dpi)") 
		} else {
			wi <- format(width)
			hi <- format(height)
			message("Size: ", wi, " by ", hi, " inches") 
		}
	}
	options(tmap.mode=tmap.mode)
	invisible()
}

choose_unit = function(x) {
	units = ifelse(x > 50, "px", "in")
	if (x > 15 && x < 100) message("The argument 'units' has been set to \"", units, "\" since the specified width or height is ", ifelse(units == "px", "greater than ", "less than or equal to "), 50, ". Specify units = \"", ifelse(units == "px", "in", "px"),"\" to change this.")
	units
}
