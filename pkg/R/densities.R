#' Calculate densities
#' 
#' This function transposes quantitiy variables to densitiy variables, which are needed for choroplets.
#'
#' @param shp a shape object
#' @param var name(s) of a qualtity variable name contained in the \code{map} data
#' @param total.area.km2 total area size of NL in km2. According to Wikipedia, this is 33893.
#'
#' @keywords densities
#' @return vector or data.frame (depending on whether \code{length(var)==1} with density values. This can be appended directly to the shape file.
#' @export
#' 
densities <- function(shp, var, total.area.km2=33893) {
	## calculate densities
	areas <- getAreas(shp)

	## correction to total.area.km2 
	areasKM2 <- areas * (total.area.km2 / sum(areas))
	
	## calculate and return densities
    if (length(var)==1) return(shp@data[[var]] / areasKM2)

    as.data.frame(lapply(shp@data[, var], function(x)x/areasKM2))
}