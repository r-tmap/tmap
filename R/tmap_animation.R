#' Create animation
#' 
#' Create a gif animation or video from a tmap plot.
#' 
#' @param tm tmap or a list of tmap objects. If \code{tm} is a tmap object, facets should be created, where nrow and ncol in \code{\link{tm_facets}} have to be set to 1 in order to create one map per frame.
#' @param filename filename. If omitted (default), the animation will be shown in the viewer or browser. If specified, it should be a gif file or a video file (i.e. mp4). The package \code{gifski} is required to create a gif animation. The package \code{av} (which uses the \code{FFmpeg} library) is required for video formats. The mp4 format is recommended but many other video formats are supported, such as wmv, avi, and mkv.
#' @param width,height width and height of the animation file (in pixels). Required when \code{tm} is a list, and recommended to specify in advance when \code{tm} is a \code{tmap} object. If not specified in the latter case, it will be determined by the aspect ratio of the map.
#' @param dpi dots per inch. By default 100, but this can be set with the option \code{output.dpi.animation} in \code{\link{tmap_options}}.
#' @param delay delay time between images (in 1/100th of a second). See also \code{fps}
#' @param fps frames per second, calculated as \code{100 / delay}. If \code{fps} is specified, the \code{delay} will be set to \code{100/fps}.
#' @param loop logical that determined whether the animation is looped, or an integer value that determines how many times the animation is looped.
#' @param outer.margins (passed on to \code{\link{tmap_save}}) overrides the outer.margins argument of \code{\link{tm_layout}} (unless set to \code{NA})
#' @param asp (passed on to \code{\link{tmap_save}}) if specified, it overrides the asp argument of \code{\link{tm_layout}}. Tip: set to \code{0} if map frame should be placed on the edges of the image.
#' @param scale (passed on to \code{\link{tmap_save}}) overrides the scale argument of \code{\link{tm_layout}} (unless set to \code{NA})
#' @param restart.delay not used anymore
#' @param ... arguments passed on to \code{\link[av:av_encode_video]{av_encode_video}}
#' @note Not only tmap plots are supported, but any series of R plots.
#' @concept animation
#' @example ./examples/tmap_animation.R
#' @import tmaptools
#' @importFrom utils browseURL
#' @export
tmap_animation <- function(tm, filename = NULL, width = NA, height = NA, dpi = NA, delay = 40, fps = NA, loop = TRUE, outer.margins=NA, asp=NULL, scale=NA, restart.delay = NULL, ...) {
	.tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)
	
	showAni = missing(filename)
	if (showAni) filename = tempfile(fileext = ".gif")

	progress = .tmapOptions$show.messages
	
	if (!is.numeric(delay) || !(length(delay) == 1L)) stop("delay must be a numeric value", call. = FALSE)
	if ((!is.numeric(loop) && !is.logical(loop)) || !(length(loop) == 1L)) stop("loop must be a logical or numeric value", call. = FALSE)
	#if (!is.numeric(restart.delay) || !(length(restart.delay) == 1L)) stop("restart.delay must be a numeric value", call. = FALSE)

	if (!is.na(fps)) delay = 100 / fps
	
	gif = substr(filename, nchar(filename) - 2, nchar(filename)) == "gif"

	#syscall <- if (.Platform$OS.type == "unix") system else shell ## macOS == unix

	# check system requirements
	if (gif) {
		if (!requireNamespace("gifski", quietly = TRUE)) stop("Package gifski is required for gif animations but not installed.") 
	} else {
		if (!requireNamespace("av", quietly = TRUE)) stop("Package av is required for ffmpeg animations but not installed.") 
	}

	if (is.na(dpi)) dpi <- .tmapOptions$output.dpi.animation
	
	# create plots
	d <- paste(tempdir(), "/tmap_plots", sep="/")
	dir.create(d, showWarnings = FALSE)
	
	if (progress) cat("Creating frames\n")
	
	even = if (gif) round else {
		function(x) round(x / 2) *2
	} # av requires height to be divisible by 2
	
	
	if (inherits(tm, "tmap_arrange") || (is.list(tm) && !inherits(tm, "tmap"))) {
		
		if (progress) pb = txtProgressBar()
		
		if (is.na(width) || is.na(height)) stop("The arguments width and height need to be specified both.")
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
				height <- even(sqrt(.tmapOptions$output.size / sasp) * dpi)
				width <- round(height * sasp)
			} else if (is.na(width)) {
				width = round(height * sasp)
			} else {
				height = even(width / sasp)
			}
		}
		if (!gif) height = even(height)
		
		suppressMessages(tmap_save(tm, filename = paste(d, "plot%03d.png", sep="/"), width=width, height=height, dpi=dpi, outer.margins=outer.margins, asp=asp, scale=scale))
	} else {
		stop("tm should be a tmap object or a list of tmap objects")
	}

	files <- list.files(path = d, pattern = "^plot[0-9]{3}\\.png$")
	k <- length(files)
	
	if (progress) cat("\nCreating animation\n")
	
	if (gif) {
		gifski::gifski(file.path(d, files), 
					   delay = delay / 100, 
					   width = width, height = height,
					   gif_file  = filename,
					   progress = progress,
					   loop = loop)
	} else {
		args = list(...)
		if (!("verbose" %in% names(args))) args$verbose = FALSE
		do.call(av::av_encode_video,
				c(list(file.path(d, files), 
							output = filename,
							framerate = 100/delay),
				  args))
	}
	
	
	# cleaning up plots
	unlink(d, recursive = TRUE)

	if (showAni) {
		viewer = getOption("viewer", utils::browseURL)
		if (is.function(viewer)) {
			viewer(filename)
		} else {
			if (progress) cat("Unable to open animation in viewer nor browser. Animation saved to", suppressWarnings(normalizePath(filename)), "\n")
			invisible()	
		}
	} else {
		if (progress) cat("Animation saved to", suppressWarnings(normalizePath(filename)), "\n")
		invisible()	
	}

}
