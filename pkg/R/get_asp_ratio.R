#' Get aspect ratio
#' 
#' Get the aspect ratio of a shape object, i.e., the map width devided by the map height.
#' 
#' The arguments \code{width}, \code{height}, and \code{res} are passed on to \code{\link[grDevices:png]{png}}. If \code{x} is a tmap object, a temporarily png image is created to calculate the aspect ratio of a tmap object. The default size of this image is 700 by 700 pixels at 100 dpi.
#' 
#' @param x shape object (either \code{\link[sp:Spatial]{Spatial}} or a \code{\link[raster:Raster-class]{Raster}}) or a tmap object. If \code{x} is not specified, the \code{\link{last_map}} is taken.
#' @param width See details; only applicable if \code{x} is a tmap object.
#' @param height See details; only applicable if \code{x} is a tmap object.
#' @param res See details; only applicable if \code{x} is a tmap object.
#' @return aspect ratio
#' @import sp
#' @importFrom raster couldBeLonLat
#' @export
get_asp_ratio <- function(x=NULL, width=700, height=700, res=100) {
	lastcall <- get(".last_map", envir = .TMAP_CACHE)
	if (missing(x)) {
		x <- suppressWarnings(last_map())
		if (is.null(x)) stop("A map has not been created yet")
	}

	on.exit({
		assign(".last_map", lastcall, envir = .TMAP_CACHE)
	})
	
	if (inherits(x, c("Spatial", "Raster"))) {
		bbx <- bbox(x)
		asp <- calc_asp_ratio(bbx[1, ], bbx[2, ], !is_projected(x))
	} else if (inherits(x, "tmap")) {
		tmp <- tempfile(fileext = ".png")
		png(tmp, width=width, height=height, res = res)
		asp <- print(x, return.asp = TRUE)
		dev.off()
	} else {
		stop("x is neither a shape nor a spatial object")
	}
	asp
}

calc_asp_ratio <- function(xlim, ylim, longlat) {
	if (is.na(longlat)) longlat <- TRUE
	if (diff(xlim)==0 || diff(ylim)==0) {
		1
	} else unname((diff(xlim)/diff(ylim)) * ifelse(longlat, cos((mean(ylim) * pi)/180), 1))
}