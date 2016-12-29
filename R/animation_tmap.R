#' Create animation
#' 
#' Create a gif or mpeg animation from a tmap plot. The free tool ImageMagick is required.
#'
#' @param tm tmap object. In order to create a series of tmap plots, which will be the frames of the animation, it is important to set nrow and ncol in \code{\link{tm_facets}}, for otherwise a small multiples plot is generated. Commonly, where one map is shown at a time, both nrow and ncol are set to 1.
#' @param filename filename of the video (should be a .gif or .mpg file)
#' @param width width of the animation file (in pixels)
#' @param height height of the animation file (in pixels)
#' @param delay delay time between images (in 1/100th of a second)
#' @note Not only tmap plots are supported, but any series of R plots.
#' @keywords animation
#' @example ./examples/animation_tmap.R
#' @import tmaptools
#' @export
animation_tmap <- function(tm, filename="animation.gif", width=NA, height=NA, delay=40) {
	# determine OS to pass on system vs. shell command
	if (.Platform$OS.type == "unix") {         
		syscall <- system
    	} else {
        	syscall <- shell
    	}
	checkIM <- syscall("convert -version")
	if (checkIM!=0) stop("Could not find ImageMagick. Make sure it is installed and included in the systems PATH")

	# create plots
	d <- paste(tempdir(), "/tmap_plots", sep="/")
	dir.create(d, showWarnings = FALSE)
	save_tmap(tm, filename = paste(d, "plot%03d.png", sep="/"), width=width, height=height)

	# convert pngs to one gif using ImageMagick
	output <- syscall(paste("convert -delay ", delay, " ", d, "/*.png \"", filename, "\"", sep=""))
	
	# cleaning up plots and temporary variables
	unlink(d, recursive = TRUE)
	rm(syscall)
	
	invisible()	
}
