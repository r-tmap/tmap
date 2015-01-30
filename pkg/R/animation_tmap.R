#' Create animations
#' 
#' This function creates a gif or mpeg animation from a tmap plot. The free tool ImageMagick is required.
#'
#' @param expr R expression to create series of tmap plots. In order to create a series of tmap plots, which will be the frames of the animation, it is important to set nrow and ncol in \code{\link{tm_facets}}, for otherwise a small multiples plot is generated. Commonly, where one map is shown at a time, both nrow and ncol are set to 1.
#' @param width width of the animation file (in pixels)
#' @param height height of the animation file (in pixels)
#' @param delay delay time between images (in 1/100th of a second)
#' @param filename filename of the video (should be a .gif or .mpg file)
#'
#' @keywords animation
#' @examples
#' \dontrun{
#' data(Europe)
#' 
#' animation_tmap({
#' 	tm_shape(Europe) + 
#' 		tm_fill("yellow") + 
#' 		tm_borders() + 
#' 		tm_facets(by = "name", nrow=1,ncol=1) + 
#' 		tm_layout(scale=2)
#' }, width=1200, height=800, delay=100, filename="European countries.gif")
#' }
#' @export
animation_tmap <- function(expr, width=1000, height=1000, delay=40, filename="animation.gif") {
	
	checkIM <- shell("convert -version")
	if (checkIM!=0) stop("Could not find ImageMagick. Make sure it is installed and included in the systems PATH")
	
	# create plots
	d <- paste(tempdir(), "/tmap_plots", sep="/")
	dir.create(d)
	png(filename=paste(d, "plot%03d.png", sep="/"), width=width, height=height)
	print(expr)
	dev.off()

	# convert pngs to one gif using ImageMagick
	output <- shell(paste("convert -delay ", delay, " ", d, "/*.png ", filename, sep=""))
	
	# cleaning up plots
	unlink(d, recursive = TRUE)
	
	invisible()	
}