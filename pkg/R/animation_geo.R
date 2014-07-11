#' Create animations
#' 
#' This function creates a gif or mpeg animation from a geo plot. The free tool ImageMagick is required.
#'
#' @param expr R expression to create series of geo plots. This can be achieved by small multiples while setting the nrow and ncol parameters of \code{\link{geo_facets}} to 1.
#' @param width width of the animation file (in pixels)
#' @param height height of the animation file (in pixels)
#' @param delay delay time between images
#' @param filename filename of the video (should be a .gif or .mpg file)
#'
#' @keywords animation
#' @export
animation_geo <- function(expr, width=1000, height=1000, delay=40, filename="animation.gif") {
	
	checkIM <- shell("convert -version")
	if (checkIM!=0) stop("Could not find ImageMagick. Make sure it is installed and included in the systems PATH")
	
	png(filename="plot%03d.png", width=width, height=height)
	print(expr)
	dev.off()
	# convert pngs to one gif using ImageMagick
	output <- shell(paste("convert -delay ", delay, " *.png ", filename, sep=""))
	
	### ffmeg
	#output <- shell(paste("c:\ffmpeg\bin\ffmpeg -f image2 -r 1/", delay, " -i plot%03d.png -c:v libx264 -r 30 ", filename, sep=""))
	
	# cleaning up
	file.remove(list.files(pattern=".png"))
	
	invisible()	
}