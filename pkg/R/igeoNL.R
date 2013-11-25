#' Interactive cartography
#' 
#' Start the user interface
#' 
#' @examples
#' \dontrun{
#' 		# load shape files
#' 		options(shp_dir=system.file("shapes", package="geoNL"))
#' 		
#'		## load shape
#' 		shp.gm <- getShape("gm", 2012)
#' 		shp.pv <- getShape("pv", 2012)
#'
#'		## load data
#'		data(NLpop)
#'
#'		## add derived variables
#'		NLpop <- transform(NLpop, perc.male=total.male/total*100, perc.female=total.female/total*100)
#'
#'		## append data to shape file
#'		shp.gm <- appendData(shp.gm, data=NLpop, key.data="gm.name",key.shp="GM_NAAM")
#'
#'		## map gender ratio to color
#'		shp.gm$gender <- ((shp.gm$perc.male/100)-.5) / sqrt(shp.gm$total) * 10000
#' 		
#' 		igeoNL()
#' 		
#' }
#'
#' @export
igeoNL <- function() {
	obs <- ls(envir=.GlobalEnv)
	shps <- sapply(obs, function(x)inherits(get(x), c("SpatialPolygonsDataFrame")))
	if (any(shps)) {
		shiny::runApp(system.file("shinyapp", package="geoNL"))
	} else {
		stop("Load shape objects with data, i.e. class SpatialPolygonsDataFrame, first.")
	}
	invisible()
}