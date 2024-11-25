#' Save tmap
#'
#' Save tmap to a file. This can be either a static plot (e.g. png) or an interactive map (html).
#'
#' @param tm tmap object
# The docs for filename could be put in tmap_mode.
#' @param filename filename including extension, and optionally the path.
#'   The extensions pdf, eps, svg, wmf (Windows only), png, jpg, bmp, tiff, and html are supported.
#'   If the extension is missing, the file will be saved as a static plot in `"plot"`
#'   mode and as an interactive map (html) in `"view"` mode (see details).
#'   The default format for static plots is png, but this can be changed using
#'   the option `"output.format"` in [tmap_options()]. If `NA` (the default),
#'   the file is saved as "tmap01" in the default format, and the number incremented
#'   if the file already exists.
#' @param device graphic device to use. Either a device function
#'   (e.g., [`png`][grDevices::png()] or [`cairo_pdf`][grDevices::cairo_pdf()])
#'   or a text indicating selected graphic device: "pdf", "eps", "svg", "wmf" (Windows only), "png", "jpg", "bmp", "tiff".
#'   If `NULL`, the graphic device is guessed based on the `filename` argument.
#' @param height,width The dimensions of the plot (not applicable for html files).
#'   Units are set with the argument `units`. If one of them is not specified,
#'   this is calculated using the formula asp = width / height, where asp is the
#'   estimated aspect ratio of the map. If both are missing, they are set such
#'   that `width * height` is equal to the option `"output.size"` in [tmap_options()].
#'   This is by default 49, meaning that is the map is a square (so aspect ratio of 1)
#'   both width and height are set to 7.
#' @param units units for width and height (`"in"`, `"cm"`, or `"mm"`).
#'   By default, pixels (`"px"`) are used if either width or height is set to a
#'   value greater than 50. Else, the units are inches (`"in"`).
#' @param dpi dots per inch. Only applicable for raster graphics. By default it
#'   is set to 300, but this can be changed using the option `"output.dpi"` in [tmap_options()].
#' @param outer.margins overrides the outer.margins argument of [tm_options()] (unless set to `NA`)
#' @param asp if specified, it overrides the asp argument of [tm_options()].
#'   **Tip**: set to `0` if map frame should be placed on the edges of the image.
#' @param scale overrides the scale argument of [tm_options()] (unless set to `NA`)
#' @param insets_tm tmap object of an inset map, or a list of tmap objects of
#'   multiple inset maps. The number of tmap objects should be equal to the number
#'   of viewports specified with `insets_vp`.
#' @param insets_vp [`viewport`][grid::viewport()] of an inset map, or a list
#'   of [`viewport`][grid::viewport()]s of multiple inset maps. The number of
#'   viewports should be equal to the number of tmap objects specified with `insets_tm`.
#' @param add.titles add titles to leaflet object.
#' @param in.iframe should an interactive map be saved as an iframe?
#'   If so, two HTML files will be saved; one small parent HTML file with the
#'   iframe container, and one large child HTML file with the actual widget.
#'   See [widgetframe::saveWidgetframe()] for details. By default `FALSE`,
#'   which means that one large HTML file is saved (see [saveWidget()][htmlwidgets::saveWidget()]).
#' @param selfcontained when an interactive map is saved, should the resources
#'   (e.g. JavaScript libraries) be contained in the HTML file? If `FALSE`, they
#'   are placed in an adjacent directory (see also [htmlwidgets::saveWidget()]).
#'   Note that the HTML file will often still be large when `selfcontained = FALSE`,
#'   since the map data (polygons and popups), which are also contained in the HTML file,
#'   usually take more space then the map resources.
#' @param verbose Deprecated. It is now controlled by the tmap option `show.messages`
#'    (see [tmap_options()])
#' @inheritDotParams htmlwidgets::saveWidget
#' @inheritDotParams widgetframe::saveWidgetframe
#' @returns the filename, invisibly, if export is successful.
#' @importFrom htmlwidgets saveWidget
#' @import tmaptools
#' @example ./examples/tmap_save.R
#' @export
tmap_save = function(tm=NULL, filename=NA, device=NULL, width=NA, height=NA, units = NA,
					  dpi=NA, outer.margins=NA, asp=NULL, scale=NA, insets_tm=NULL,
					 insets_vp=NULL, add.titles = TRUE, in.iframe = FALSE, selfcontained = !in.iframe, verbose = NULL, ...) {
	.tmapOptions = get("tmapOptions", envir = .TMAP)
	show.warnings =.tmapOptions$show.warnings
	if (!missing(verbose) && show.warnings) warning("The argument verbose is deprecated. Please use the option show.messages of tmap_options instead.")



	verbose = .tmapOptions$show.messages


	lastcall = x = get("last_map", envir = .TMAP)
	if (missing(tm)) {
		tm = suppressWarnings(tmap_last())
		if (is.null(tm)) stop("A map has not been created yet")
		is.arrange = FALSE
	} else if (inherits(tm, "tmap")) {
		is.arrange = FALSE
	} else if (inherits(tm, "tmap_arrange")) {
		is.arrange = TRUE
	} else {
		stop("Unknown format. tm should be either a tmap output, or a list of tmap outputs")
	}

	tmap.mode = getOption("tmap.mode")
	default_ext = ifelse(tmap.mode == "plot", .tmapOptions$output.format, "html")


	if (is.na(filename)) {
		filename_default = paste("tmap01", default_ext, sep = ".")
		if (!file.exists(filename_default)) {
			filename = filename_default
		} else {
			files = list.files(pattern = paste0("^tmap[0-9]{2}\\.", default_ext, "$"))
			fid = max(as.integer(substr(files, 5, 6))) + 1L
			filename = paste0("tmap", sprintf("%02d", fid), ".",  default_ext)
		}
	}

	if (is.na(dpi)) dpi = .tmapOptions$output.dpi

	on.exit({
		assign("last_map", lastcall, envir = .TMAP)
	})



	get_ext = function(filename, default_ext) {
		pieces = strsplit(filename, "\\.")[[1]]
		if (length(pieces)==1) return(default_ext)
		tolower(pieces[length(pieces)])
	}

	convert_to_inches = function(x, units) {
		x = switch(units, px = x/dpi, `in` = x, cm = x/2.54, mm = x/2.54/10)
	}
	convert_to_pixels = function(x, units) {
		x = switch(units, px = x, `in` = dpi*x, cm = dpi*x/2.54, mm = dpi*x/2.54/10)
	}

	ext = get_ext(filename, default_ext)

	interactive = (ext=="html")

	options(tmap.mode=ifelse(interactive, "view", "plot"))

	if (interactive) {
		if (is.arrange) {
			lf = print_tmap_arrange(tm, show = FALSE, add.titles=add.titles)
		} else {
			lf = print.tmap(tm, show = FALSE)
		}

		tryCatch({
			wd = getwd()
			on.exit(setwd(wd), add = TRUE)
			wd_new = dirname(filename)
			base_filename = basename(filename)
			setwd(wd_new)
			if (in.iframe) {
				rlang::check_installed("widgetframe", reason = "for option in.frame.")
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
		return(invisible(filename))
	}

	if (is.na(width) || is.na(height)) {
		if (!is.na(width)) {
			if (is.na(units)) units = choose_unit(width)
			temp_size = convert_to_pixels(width, units)
		} else if (!is.na(height)) {
			if (is.na(units)) units = choose_unit(height)
			temp_size = convert_to_pixels(height, units)
		} else {
			units = "px"
			temp_size = 700
		}

		if (is.arrange) {
			sasp = 1
		} else {
			show.messages = tmap_options(show.messages = FALSE)
			on.exit(tmap_options(show.messages))
			sasp = get_asp_ratio(tm, width = temp_size, height = temp_size, res = dpi)
			tmap_options(show.messages)
		}

		if (is.na(width) && !is.na(height)) {
			width = height * sasp
		} else if (is.na(height)  && !is.na(width)) {
			height = width / sasp
		} else {
			units = "in"
			height = sqrt(.tmapOptions$output.size / sasp)
			width = height * sasp
		}
	} else {
		if (is.na(units)) units = choose_unit(max(width, height))
	}
	units_target = ifelse(units=="px" && ext %in% c("png", "jpg", "jpeg", "bmp", "tiff"), "px", "in")

	if (units_target=="in") {
	  width = convert_to_inches(width, units)
	  height = convert_to_inches(height, units)

	  if (ext=="pdf") {
	    round_to_1_72 = function(x) x %/% (1/72) / 72
	    width = round_to_1_72(width)
	    height = round_to_1_72(height)
	  }
	} else {
		width = convert_to_pixels(width, units)
		height = convert_to_pixels(height, units)
	}
	old_dev = grDevices::dev.cur()
	dev = plot_device(device = device, ext = ext, filename = filename,
					   dpi = dpi, units_target = units_target)

	dev(filename = filename, width = width, height = height, ...)

	on.exit(capture.output({
			dev.off()
			if (old_dev > 1) grDevices::dev.set(old_dev) # restore old device unless null device
			}), add = TRUE)

	if (is.arrange) {
		opts = attr(tm, "opts")
		if (!is.na(outer.margins[1])) opts$outer.margins = outer.margins
		if (!missing(asp)) opts$asp = asp

		attr(tm, "opts") = opts
		print(tm)
	} else {
		args = list()
		if (!is.na(outer.margins[1])) args$outer.margins = outer.margins
		if (!missing(asp)) args$asp = asp
		if (!is.na(scale)) args$scale = scale
		print(tm + do.call("tm_options", args))
	}

	if (!is.arrange && !missing(insets_tm) && !missing(insets_vp)) {
	  args_inset = if (!is.na(scale)) list(scale = scale) else list()
	  if (inherits(insets_tm, "list") && inherits(insets_vp, "list")) {
	    if (length(insets_tm) != length(insets_vp)) stop("Number of insets unequal to number of viewports")
	    mapply(function(tm_i, vp_i) {
	      print(tm_i + do.call("tm_options", args_inset), vp=vp_i)
	    }, insets_tm, insets_vp)
	  } else if (inherits(insets_tm, "tmap") && inherits(insets_vp, "viewport")) {
	    print(insets_tm + do.call("tm_options", args_inset), vp = insets_vp)
	  } else {
	    stop("Insets and/or its viewports not in the correct format")
	  }
	}

	if (verbose) {
		message("Map saved to ", suppressWarnings(normalizePath(filename)))
		if (ext %in% c("png", "jpg", "jpeg", "bmp", "tiff")) {
			if (units_target == "px") {
				wp = format(width)
				hp = format(height)
				wi = format(convert_to_inches(width, "px"))
				hi = format(convert_to_inches(height, "px"))
			} else {
				wi = format(width)
				hi = format(height)
				wp = format(convert_to_pixels(width, "in"))
				hp = format(convert_to_pixels(height, "in"))
			}
			message("Resolution: ", format(wp), " by ", format(hp), " pixels")
			message("Size: ", wi, " by ", hi, " inches (", format(dpi), " dpi)")
		} else {
			wi = format(width)
			hi = format(height)
			message("Size: ", wi, " by ", hi, " inches")
		}
	}
	options(tmap.mode=tmap.mode)
	invisible(filename)
}

plot_device = function(device, ext, filename, dpi, units_target){

	force(filename)
	force(dpi)
	force(units_target)

	if (is.function(device)) {
		args = formals(device)
		call_args = list()
		if ("file" %in% names(args)) {
			call_args$file = filename
		}
		if ("res" %in% names(args)) {
			call_args$res = dpi
		}
		if ("units" %in% names(args)) {
			call_args$units = units_target
		}
		dev = function(...) do.call(device, utils::modifyList(list(...), call_args))
		return(dev)
	}
	ps = function(..., filename, width, height)
		grDevices::postscript(
			...,
			file = filename,
			width = width,
			height = height,
			onefile = FALSE,
			horizontal = FALSE,
			paper = "special"
		)
	jpeg = function(..., width, height)
		grDevices::jpeg(
			...,
			width = width,
			height = height,
			res = dpi,
			units = units_target
		)
	devices = list(
		ps = ps,
		eps = ps,
		tex = function(..., filename, width, height)
			grDevices::pictex(..., file = filename, width = width, height = height),
		pdf = function(..., filename, version = "1.4")
			grDevices::pdf(..., file = filename, version = version),
		svg = function(..., filename)
			grDevices::svg(..., file = filename),
		wmf = function(..., width, height)
			grDevices::win.metafile(..., width = width, height = height),
		emf = function(..., width, height)
			grDevices::win.metafile(..., width = width, height = height),
		png = function(..., width, height)
				grDevices::png(
					...,
					width = width,
					height = height,
					res = dpi,
					units = units_target
				),
		jpeg = jpeg,
		jpg = jpeg,
		bmp =
			function(..., width, height)
				grDevices::bmp(
					...,
					width = width,
					height = height,
					res = dpi,
					units = units_target
				),
		tiff = function(..., width, height)
			grDevices::tiff(
				...,
				width = width,
				height = height,
				res = dpi,
				units = units_target
			)
	)
	if (is.null(device)) {
		dev = devices[[ext]]
		if (is.null(dev)) {
			stop("'", dev, "'", " graphic device does not exist", call. = FALSE)
		}
		return(dev)
	}
	if (!device %in% names(devices)) {
		stop('"', device, '"', " graphic device does not exist", call. = FALSE)
	}
}

choose_unit = function(x) {
	units = ifelse(x > 50, "px", "in")
	if (x > 15 && x < 100) {
		message("The argument 'units' has been set to \"",
				units, "\" since the specified width or height is ",
				ifelse(units == "px", "greater than ", "less than or equal to "), 50, ". Specify units = \"", ifelse(units == "px", "in", "px"),"\" to change this.")
	}
	units
}
