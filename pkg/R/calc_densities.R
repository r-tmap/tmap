#' Calculate densities
#' 
#' Transpose quantitative variables to densitiy variables, which are often needed for choroplets. For example, the colors of a population density map should correspond population density counts rather than absolute population numbers.
#'
#' @param shp a shape object, i.e. a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}
#' @param var name(s) of a qualtity variable name contained in the \code{map} data
#' @param total.area.km2 total area size of \code{shp} in number of squared kilometers. If \code{NA}, and \code{unit="km2"}, then the polygon coordinates are assumed to be in meters.
#' @param units the units of the area sizes, either \code{"m2"} or \code{"km2"}. In other words, either \code{var/m2} or \code{var/km2} density values are calculated.
#' @param suffix character that is appended to the variable names. The resulting names are used as column names of the returned data.frame.
#' @param drop boolean that determines whether an one-column data-frame should be returned as a vector
#'
#' @keywords densities
#' @return Vector or data.frame (depending on whether \code{length(var)==1} with density values. This can be appended directly to the shape file with \code{\link{append_data}} with \code{fixed.order=TRUE}.
#' @example  ../examples/calc_densities.R
#' @export
#' 
calc_densities <- function(shp, var, total.area.km2=NA, units="km2", suffix="", drop=TRUE) {
	## calculate densities
	areas <- approx_areas(shp, total.area.km2=total.area.km2)
	
	if (units=="km2" && is.na(total.area.km2)) areas <- areas / 1e6
	if (units=="m2" && !is.na(total.area.km2)) areas <- areas * 1e6
	
	
	## calculate and return densities
    if (length(var)==1 && drop) return(shp@data[[var]] / areas)

    data <- as.data.frame(lapply(shp@data[, var, drop=FALSE], function(x)x/areas))
	names(data) <- paste(var, suffix, sep="")
	data
}

