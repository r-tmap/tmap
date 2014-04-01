#' Get shape file
#' 
#' This function loads a shape file, either directly, or from a (network) directory with shape files.
#'
#' @param id either a region label (see \code{regions}) or a shape file name (including directory).
#' @param year when \code{id} is specified as a region, then year must also be specified. Together, they define the shape file "region_year.shp".
#' @param dir the directory of the shape files. Only applicable when \code{id} is specifies as region label. It should have a subdirectory for each year, and the shape files should be names according to the region label (see \code{regions}), followed by an underscore and the year.
#' @return shape object
#' @import rgdal
#' @export
get_shape <- function(id, year=NULL, dir=getOption("shp_dir")){
	
	# determine region ID
	if (missing(year)) {
		if (file.exists(id)) {
			fullfilename <- id
		} else stop("unknown filename")
	} else {
		regionID <- which(regions==id)
		if (length(regionID)!=0) {         
			if (regionID > nrow(regions)) 
				regionID <- regionID - nrow(regions)
			
			regionCode <- regions$code[regionID]
		} else regionCode <- id
		# construct filename
		filename <- paste(regionCode, "_", year, ".shp", sep="")
		if (filename %in% list.files(paste(dir, year, sep="/"))) {
			fullfilename <- paste(dir, year, filename, sep="/")
		} else stop("unknown filename")
	}
	dir <- dirname(fullfilename)
	base <- basename(fullfilename)
	if (substr(base, nchar(base)-3,nchar(base))==".shp")
		base <- substr(base, 1, nchar(base)-4)
	
	readOGR(dir, base, verbose=FALSE)
	# readShapePoly(fullfilename) does not import projection info
}
