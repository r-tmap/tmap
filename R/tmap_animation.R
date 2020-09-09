#' Create animation
#' 
#' Create a gif or mpeg animation from a tmap plot.
#' 
#' @param tm tmap or a list of tmap objects. If \code{tm} is a tmap object, facets should be created, where nrow and ncol in \code{\link{tm_facets}} have to be set to 1 in order to create one map per frame.
#' @param filename filename of the video, typically a gif or mp4 file. The package \code{gifski} is required to create a gif animation. The package \code{av} (which uses the \code{FFmpeg} library) is required for animation file formats. The mp4 format is recommended but many other file formats are supported, such as wmv, avi, and mkv.
#' @param width,height width and height of the animation file (in pixels). Required when \code{tm} is a list, and recommended to specify in advance when \code{tm} is a \code{tmap} object. If not specified in the latter case, it will be determined by the aspect ratio of the map.
#' @param dpi dots per inch. By default 100, but this can be set with the option \code{output.dpi.animation} in \code{\link{tmap_options}}.
#' @param delay delay time between images (in 1/100th of a second). See also \code{fps}
#' @param fps frames per second, calculated as \code{100 / delay}. If \code{fps} is specified, the \code{delay} will be set to \code{100/fps}.
#' @param loop logical that determined whether the animation is looped, or an integer value that determines how many times the animation is looped.
#' @param restart.delay not used anymore
#' @param ... arguments passed on to \code{\link[av:av_encode_video]{av_encode_video}}
#' @note Not only tmap plots are supported, but any series of R plots.
#' @concept animation
#' @example ./examples/tmap_animation.R
#' @import tmaptools
#' @export
tmap_animation <- function(tm, filename = "animation.gif", width = NA, height = NA, dpi = NA, delay = 40, fps = NA, loop = TRUE, restart.delay = NULL, ...) {
	.tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)
	
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
	
	if (inherits(tm, "tmap_arrange") || (is.list(tm) && !inherits(tm, "tmap"))) {
		
		if (progress) pb = txtProgressBar()
		
		if (is.na(width) || is.na(height)) stop("The arguments width and height need to be specified both.")
		for (i in 1:length(tm)) {
			if (progress) setTxtProgressBar(pb, i/length(tm))
			tmi = tm[[i]]
			if (!inherits(tmi, "tmap")) stop("List item ", i, " of tm is not a tmap object.")
			suppressMessages(tmap_save(tm[[i]], filename = paste0(d, "/plot", sprintf("%03d", i), ".png"), width=width, height=height, dpi=dpi))
		}
	} else if (inherits(tm, "tmap")) {
		if (is.na(width) || is.na(height)) {
			asp <- get_asp_ratio(tm, width = 700, height = 700, res = dpi)	
			if (is.na(height)) {
				width = 700 * asp
			}
			if (is.na(height)) height = round((width / asp) / 2) * 2 # av requires height to be divisible by 2
		}
		suppressMessages(tmap_save(tm, filename = paste(d, "plot%03d.png", sep="/"), width=width, height=height, dpi=dpi))
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

	if (progress) cat("Animation saved to", suppressWarnings(normalizePath(filename)), "\n")

	invisible()	
}
