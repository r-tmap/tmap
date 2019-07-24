#' Create animation
#' 
#' Create a gif or mpeg animation from a tmap plot. The free tool ImageMagick is required.
#' 
#' @param tm tmap object. In order to create a series of tmap plots, which will be the frames of the animation, it is important to set nrow and ncol in \code{\link{tm_facets}}, for otherwise a small multiples plot is generated. Commonly, where one map is shown at a time, both nrow and ncol are set to 1.
#' @param filename filename of the video (should be a .gif or .mpg file)
#' @param width width of the animation file (in pixels)
#' @param height height of the animation file (in pixels)
#' @param dpi dots per inch. Only applicable for raster graphics. By default 300, but this can be set with the option \code{output.dpi} in \code{\link{tmap_options}}.
#' @param delay delay time between images (in 1/100th of a second)
#' @param loop logical that determined whether the animation is looped, or an integer value that determines how many times the animation is looped.
#' @param restart.delay delay time between the loops (in 1/100th of a second)
#' @note Not only tmap plots are supported, but any series of R plots.
#' @concept animation
#' @example ./examples/tmap_animation.R
#' @import tmaptools
#' @export
tmap_animation <- function(tm, filename="animation.gif", width=NA, height=NA, dpi=NA, delay=40, loop = TRUE, restart.delay = 0) {
	.tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)
	
	if (!is.numeric(delay) || !(length(delay) == 1L)) stop("delay must be a numeric value", call. = FALSE)
	if ((!is.numeric(loop) && !is.logical(loop)) || !(length(loop) == 1L)) stop("loop must be a logical or numeric value", call. = FALSE)
	if (!is.numeric(restart.delay) || !(length(restart.delay) == 1L)) stop("restart.delay must be a numeric value", call. = FALSE)

	syscall <- if (.Platform$OS.type == "unix") system else shell ## macOS == unix

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

	if (is.na(dpi)) dpi <- .tmapOptions$output.dpi
	
	# create plots
	d <- paste(tempdir(), "/tmap_plots", sep="/")
	dir.create(d, showWarnings = FALSE)
	suppressMessages(tmap_save(tm, filename = paste(d, "plot%03d.png", sep="/"), width=width, height=height, dpi=dpi))

	files <- list.files(path = d, pattern = "^plot[0-9]{3}\\.png$")
	k <- length(files)
	
	# convert pngs to one gif using ImageMagick
	if (is.logical(loop)) loop <- 1L - as.integer(loop)
	
	if ((loop == 1) || (restart.delay == 0)) {
		output <- syscall(paste(program, " -loop ", loop, " -delay ", delay, " ", d, "/*.png \"", filename, "\"", sep=""))
	} else {
		part1 <- paste(paste("-delay ", delay, " ", d, "/", files[1L:(k-1)], sep = ""), collapse = " ")
		part2 <- paste("-delay ", restart.delay, " ", d, "/", files[k], sep = "")
		output <- syscall(paste(program, " -loop ", loop, " ", part1, " ", part2, " \"", filename, "\"", sep=""))
	}
	
	# cleaning up plots
	unlink(d, recursive = TRUE)

	if (.tmapOptions$show.messages) {
		message("Animation saved to ", suppressWarnings(normalizePath(filename)))
	}
	
	invisible()	
}
