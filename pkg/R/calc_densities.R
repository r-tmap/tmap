#' Calculate densities
#' 
#' This function transposes quantitiy variables to densitiy variables, which are needed for choroplets.
#'
#' @param shp a shape object
#' @param var name(s) of a qualtity variable name contained in the \code{map} data
#' @param total.area.km2 total area size of NL in km2. According to Wikipedia, this is 33893.
#' @param drop boolean that determines whether an one-column data-frame should be returned as a vector
#'
#' @keywords densities
#' @return vector or data.frame (depending on whether \code{length(var)==1} with density values. This can be appended directly to the shape file.
#' @export
#' 
calc_densities <- function(shp, var, total.area.km2=NA, drop=TRUE) {
	## calculate densities
	areas <- approx_areas(shp, total.area.km2=total.area.km2)

	
	## calculate and return densities
    if (length(var)==1 && drop) return(shp@data[[var]] / areas)

    as.data.frame(lapply(shp@data[, var, drop=FALSE], function(x)x/areas))
}

