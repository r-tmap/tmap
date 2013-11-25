#' Function to animate maps
#' 
#' This function create a gif animation. The free tool ImageMagick is required.
#'
#' @param expr R function call, e.g. with \code{choropleth}
#' @param width width of the gif file (in pixels)
#' @param height height of the gif file (in pixels)
#' @param delay delay time between images
#' @param filename filename of the video (should be a .gif or .mpg file)
#'
#' @keywords animation
#' @export
#' 
animateMaps <- function(expr, width=1000, height=1000, delay=40, filename="animation.gif") {

	checkIM <- shell("convert")
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

#ffmpeg