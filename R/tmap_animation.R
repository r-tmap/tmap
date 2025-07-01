#' Create animation
#'
#' Create a gif animation or video from an animated tmap plot. First use [tm_animate()] or [tm_animate_fast()] to animate the plot, and then apply [tmap_animation()] to save it as a gif or video file (e.g. mp4).
#'
#' @param tm tmap or a list of tmap objects. If `tm` is a tmap object,
#'   animation frames should be created using either [tm_animate()] or
#'   [tm_animate_fast()].
#' @param filename filename. If omitted (default), the animation will be shown
#'   in the viewer or browser. If specified, it should be a gif file or a video
#'   file (i.e. mp4). The package `gifski` is required to create a gif animation.
#'   The package `av` (which uses the `FFmpeg` library) is required for video formats.
#'   The mp4 format is recommended but many other video formats are supported,
#'   such as wmv, avi, and mkv.
#' @param width,height Dimensions of the animation file (in pixels).
#'   Required when `tm` is a list, and recommended to specify in advance when
#'   `tm` is a `tmap` object. If not specified in the latter case, it will be
#'   determined by the aspect ratio of the map.
#' @param dpi dots per inch. By default 100, but this can be set with the option
#'   `animation.dpi` in [tmap_options()].
#' @param outer.margins (passed on to [tmap_save()]) overrides the outer.margins
#'   argument of `tm_layout()` (unless set to `NA`)
#' @param asp (passed on to [tmap_save()]) if specified, it overrides the `asp`
#'   argument of `tm_layout()`. Tip: set to `0` if map frame should be placed on
#'   the edges of the image.
#' @param scale (passed on to [tmap_save()]) overrides the scale argument of
#'   [tm_layout()] (unless set to `NA`)
#' @param ... arguments passed on to [av::av_encode_video()]
#' @note Not only tmap plots are supported, but any series of R plots.
#' @concept animation
#' @example ./examples/tmap_animation.R
#' @import tmaptools
#' @export
tmap_animation <- function(tm, filename = NULL, width = NA, height = NA, dpi = NA, outer.margins = NA, asp = NULL, scale = NA, ...) {
	args = list(...)
	args_called = names(rlang::call_match()[-1])

	if (any(c("fps", "delay") %in% args_called)) {
		cli::cli_inform("{.filed [{.fun tmap_animation}]} please specify the frames per second {.arg fps} in {.fun tm_animate}")
		args$fps = NULL
		args$delay = NULL
	}

	if ("loop" %in% names(args)) {

		if (args$loop) {
			cli::cli_inform("{.filed [{.fun tmap_animation}]} please use {.code play = {.str loop}} in {.fun tm_animate} instead of {.code loop = TRUE}")
		} else {
			cli::cli_inform("{.filed [{.fun tmap_animation}]} please use {.code play = {.str once}} in {.fun tm_animate} instead of {.code loop = FALSE}")
		}
		args$loop = NULL
	}

	.tmapOptions <- get("tmapOptions", envir = .TMAP)

	showAni = missing(filename)
	if (showAni) filename = tempfile(fileext = ".gif")

	progress = .tmapOptions$show.messages

	if (is.na(dpi)) dpi <- .tmapOptions$animation.dpi

	gif = substr(filename, nchar(filename) - 2, nchar(filename)) == "gif"

	#syscall <- if (.Platform$OS.type == "unix") system else shell ## macOS == unix

	# check system requirements
	if (gif) {
		rlang::check_installed("gifski", reason = "for creating gif animations.")
	} else {
		rlang::check_installed("av", reason = "for ffmpeg animations")
	}


	# create plots
	d <- paste(tempdir(), "/tmap_plots", sep="/")
	dir.create(d, showWarnings = FALSE)

	if (progress) cat("Creating frames\n")

	even = if (gif) round else {
		function(x) round(x / 2) *2
	} # av requires dimensions to be divisible by 2


	if (inherits(tm, "tmap_arrange") || (is.list(tm) && !inherits(tm, "tmap"))) {

		if (progress) pb = txtProgressBar()

		if (is.na(width) || is.na(height)) stop("The arguments width and height need to be specified both.")

		width = even(width)
		height = even(height)

		for (i in 1:length(tm)) {
			if (progress) setTxtProgressBar(pb, i/length(tm))
			tmi = tm[[i]]
			if (!inherits(tmi, "tmap")) stop("List item ", i, " of tm is not a tmap object.")
			suppressMessages(tmap_save(tm[[i]], filename = paste0(d, "/plot", sprintf("%03d", i), ".png"), width=width, height=height, dpi=dpi, outer.margins=outer.margins, asp=asp, scale=scale))
		}
	} else if (inherits(tm, "tmap")) {
		if (is.na(width) || is.na(height)) {
			sasp <- get_asp_ratio(tm, width = 700, height = 700, res = dpi)

			if (is.na(width) && is.na(height)) {
				height <- sqrt(.tmapOptions$output.size / sasp) * dpi
				width <- height * sasp
			} else if (is.na(width)) {
				width = height * sasp
			} else {
				height = width / sasp
			}
		}

		# round and in case of av to even numbers
		width = even(width)
		height = even(height)

		suppressMessages(tmap_save(tm, filename = paste(d, "plot%03d.png", sep="/"), width=width, height=height, dpi=dpi, outer.margins=outer.margins, asp=asp, scale=scale))
	} else {
		stop("tm should be a tmap object or a list of tmap objects")
	}

	fps = .TMAP$animation$fps
	play = .TMAP$animation$play

	files <- list.files(path = d, pattern = "^plot[0-9]{3}\\.png$", full.names = TRUE)

	if (play == "pingpong") files = c(files, rev(files))

	if (play != "once" && !gif) {
		cli::cli_inform("{.field [tmap_animation]} looped animations are only supported for gif videos")
	}

	do.call(create_animation, c(list(filename = filename, files, width = width, height = height, fps = fps, loop = (play != "once"), progress = progress, gif = gif, showAni = showAni), args))
}

create_animation = function(filename, files, width = NA, height = NA, fps = 2, loop = TRUE, progress = FALSE, gif = TRUE, showAni = FALSE, dpr = 2, knit = FALSE, knit_opts = NULL, ...) {

	k <- length(files)

	if (is.na(width) || is.na(height)) {
		x = stars::read_stars(files[1])
		width = dim(x)[1]
		height = dim(x)[2]
	}
	if (!is.na(fps)) delay = 100 / fps

	if (progress) cat("\nCreating animation\n")

	if (gif) {
		gifski::gifski(files,
					   delay = delay / 100,
					   width = width, height = height,
					   gif_file  = filename,
					   progress = progress,
					   loop = loop)
	} else {
		args = list(...)
		args = args[intersect(names(args), names(formals(av::av_encode_video)))]
		if (!("verbose" %in% names(args))) args$verbose = FALSE
		do.call(av::av_encode_video,
				c(list(files,
					   output = filename,
					   framerate = 100/delay),
				  args))
	}


	# cleaning up plots
	unlink(dirname(files[1]), recursive = TRUE)

	if (showAni) {
		file_base64 <- base64enc::dataURI(file = filename, mime = "image/gif")
		print(htmltools::browsable(
			htmltools::tagList(list(
				htmltools::tags$div(
					style = paste0(
						"width: auto",
						"height: auto"
					),
					htmltools::tags$img(
						src = file_base64,
						alt = "Animation",
						style = paste0(
							"max-width: 99%; ",
							"min-width: 99%; ",
							"width: auto; ",
							"height: auto; ",
							"margin: 0 auto; "
						)
					)
				)
			))
		)
		)
	} else {
		if (progress) cat("Animation saved to", suppressWarnings(normalizePath(filename)), "\n")
		invisible()
	}
}
