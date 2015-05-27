#' Calculate densities
#' 
#' Transpose quantitative variables to densitiy variables, which are often needed for choroplets. For example, the colors of a population density map should correspond population density counts rather than absolute population numbers.
#'
#' @param shp a shape object, i.e. a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}
#' @param var name(s) of a qualtity variable name contained in the \code{map} data
#' @param unit the preferred unit, for instance, "km", "m", or "miles". Density values in \code{var/unit^2} are calculated.
#' @param unit.size size in the coordinate system that corresponds to one \code{unit}. The coordinate system of many projections is in meters while thematic maps typically scan many kilometers, so by default \code{unit="km"} and \code{unit.size=1000} (meaning 1 kilometer equals 1000 meters).
#' @param total.area total area size of \code{shp} in number of squared units.
#' @param suffix character that is appended to the variable names. The resulting names are used as column names of the returned data.frame.
#' @param drop boolean that determines whether an one-column data-frame should be returned as a vector
#' @keywords densities
#' @return Vector or data.frame (depending on whether \code{length(var)==1} with density values. This can be appended directly to the shape file with \code{\link{append_data}} with \code{fixed.order=TRUE}.
#' @example  ../examples/calc_densities.R
#' @export
#' 
calc_densities <- function(shp, var, unit="km", unit.size=1000, total.area=NA, suffix="", drop=TRUE) {
	## calculate densities
	areas <- approx_areas(shp, unit = unit, unit.size=unit.size, total.area=total.area)
	
	## calculate and return densities
    if (length(var)==1 && drop) return(shp@data[[var]] / areas)

    data <- as.data.frame(lapply(shp@data[, var, drop=FALSE], function(x)x/areas))
	names(data) <- paste(var, suffix, sep="")
	data
}

