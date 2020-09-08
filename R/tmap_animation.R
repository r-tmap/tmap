#' Create animation
#' 
#' Create a gif or mpeg animation from a tmap plot. The free tool ImageMagick is required.
#' 
#' @param tm tmap or \code{\link{tmap_arrange}} object. If \code{tm} is a tmap object, facets should be created, where nrow and ncol in \code{\link{tm_facets}} have to be set to 1 in order to create one map per frame.
#' @param filename filename of the video (should be a .gif or .mpg file)
#' @param width width of the animation file (in pixels)
#' @param height height of the animation file (in pixels)
#' @param dpi dots per inch. Only applicable for raster graphics. By default 300, but this can be set with the option \code{output.dpi} in \code{\link{tmap_options}}.
#' @param delay delay time between images (in 1/100th of a second)
#' @param loop logical that determined whether the animation is looped, or an integer value that determines how many times the animation is looped.
#' @param restart.delay delay time between the loops (in 1/100th of a second)
#' @param ffmpeg use ffmpeg via the package av
#' @note Not only tmap plots are supported, but any series of R plots.
#' @concept animation
#' @example ./examples/tmap_animation.R
#' @import tmaptools
#' @export
tmap_animation <- function(tm, filename="animation.gif", width=NA, height=NA, dpi=NA, delay=40, loop = TRUE, restart.delay = 0, ffmpeg = TRUE) {
	.tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)
	
	if (!is.numeric(delay) || !(length(delay) == 1L)) stop("delay must be a numeric value", call. = FALSE)
	if ((!is.numeric(loop) && !is.logical(loop)) || !(length(loop) == 1L)) stop("loop must be a logical or numeric value", call. = FALSE)
	if (!is.numeric(restart.delay) || !(length(restart.delay) == 1L)) stop("restart.delay must be a numeric value", call. = FALSE)

	syscall <- if (.Platform$OS.type == "unix") system else shell ## macOS == unix

	# check system requirements
	if (ffmpeg) {
		if (!requireNamespace("av", quietly = TRUE)) stop("Package av required but not installed") 
	} else {
		checkIM <- syscall("convert -version")
		if (checkIM==0) {
			program <- "convert"
		} else {
			# For Windows, convert -version does not work if the box "install legacy files (e.g., convert.exe)" hasn't been checked.
			checkIM2 <- syscall("magick convert -version")
			if (checkIM2!=0) {
				stop("Could not find ImageMagick. Make sure it is installed and included in the systems PATH")	
			} else program <- "magick convert"
		}
	}

	if (is.na(dpi)) dpi <- .tmapOptions$output.dpi
	
	# create plots
	d <- paste(tempdir(), "/tmap_plots", sep="/")
	dir.create(d, showWarnings = FALSE)
	
	if (inherits(tm, "tmap_arrange")) {
		for (i in 1:length(tm)) {
			suppressMessages(tmap_save(tm[[i]], filename = paste0(d, "/plot", sprintf("%03d", i), ".png"), width=width, height=height, dpi=dpi))
		}
	} else {
		suppressMessages(tmap_save(tm, filename = paste(d, "plot%03d.png", sep="/"), width=width, height=height, dpi=dpi))
	}

	files <- list.files(path = d, pattern = "^plot[0-9]{3}\\.png$")
	k <- length(files)
	
	if (ffmpeg) {
		av::av_encode_video(file.path(d, files), framerate = 100/delay,
							output = filename)
	} else {
		# convert pngs to one gif using ImageMagick
		if (is.logical(loop)) loop <- 1L - as.integer(loop)
		
		if ((loop == 1) || (restart.delay == 0)) {
			output <- syscall(paste(program, " -loop ", loop, " -delay ", delay, " ", d, "/*.png \"", filename, "\"", sep=""))
		} else {
			part1 <- paste(paste("-delay ", delay, " ", d, "/", files[1L:(k-1)], sep = ""), collapse = " ")
			part2 <- paste("-delay ", restart.delay, " ", d, "/", files[k], sep = "")
			output <- syscall(paste(program, " -loop ", loop, " ", part1, " ", part2, " \"", filename, "\"", sep=""))
		}
	}
	
	
	# cleaning up plots
	unlink(d, recursive = TRUE)

	if (.tmapOptions$show.messages) {
		message("Animation saved to ", suppressWarnings(normalizePath(filename)))
	}
	
	invisible()	
}
