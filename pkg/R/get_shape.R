#' Get shape file
#' 
#' This function loads a shape file.
#'
#' @param file a shape file name (including directory).
#' @return shape object
#' @import rgdal
#' @export
get_shape <- function(file){
	
	# determine region ID
	if (file.exists(file)) {
		fullfilename <- file
	} else stop("unknown filename")

	dir <- dirname(fullfilename)
	base <- basename(fullfilename)
	if (substr(base, nchar(base)-3, nchar(base))==".shp")
		base <- substr(base, 1, nchar(base)-4)
	
	readOGR(dir, base, verbose=FALSE)
}


save_shape <- function(shp, file) {
	shpname <- deparse(substitute(shp))
	dir <- dirname(file)
	base <- basename(file)
	if (!file.exists(dir)) stop("unknown directory")

	if (substr(base, nchar(base)-3, nchar(base))==".shp")
		base <- substr(base, 1, nchar(base)-4)
	if (!inherits(shp, "Spatial")) stop("shpname is not a Spatial object")
	writeOGR(shp, dir, base, driver = "ESRI Shapefile", overwrite_layer=TRUE)
}