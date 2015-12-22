#' Get aspect ratio
#' 
#' Get the aspect ratio of a shape object, i.e., the map width devided by the map height.
#' @param x shape object (either \code{\link[sp:Spatial]{Spatial}} or a \code{\link[raster:Raster-class]{Raster}}) or a tmap object.
#' @return aspect ratio
#' @import sp
#' @importFrom raster couldBeLonLat
#' @export
get_asp_ratio <- function(x) {
	if (inherits(x, c("Spatial", "Raster"))) {
		bbx <- bbox(x)
		asp <- calc_asp_ratio(bbx[1, ], bbx[2, ], !is_projected(x))
	} else if (inherits(x, "tmap")) {
		tmp <- tempfile()
		png(tmp, width=800, height=800)
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