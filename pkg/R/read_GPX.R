#' Read GPX file
#' 
#' Read a GPX file. By default, it reads all possible GPX layers, and only returns shapes for layers that have any features.
#' 
#' @param file a GPX filename (including directory)
#' @param layers vector of GPX layers. Possible options are \code{"waypoints"}, \code{"tracks"}, \code{"routes"}, \code{"track_points"}, \code{"route_points"}. By dedault, all those layers are read.
#' @return for each defiend layer, a shape is returned (only if the layer has any features). If only one layer is defined, the corresponding shape is returned. If more than one layer is defined, a list of shape objects, one for each layer, is returned.
#' @importFrom rgdal readOGR ogrInfo
#' @export
#' @example ../examples/read_GPX.R
read_GPX <- function(file, layers=c("waypoints", "tracks", "routes", "track_points", "route_points")) {
	if (!all(layers %in% c("waypoints", "tracks", "routes", "track_points", "route_points"))) stop("Incorrect layer(s)", call. = FALSE)
	
	# check if features exist per layer
	suppressWarnings(hasF <- sapply(layers, function(l) {
		ogrInfo(dsn = file, layer=l)$have_features
	}))
	
	if (!any(hasF)) stop("None of the layer(s) has any features.", call. = FALSE)
	
	res <- lapply(layers[hasF], function(l) {
		readOGR(dsn = file, layer=l, verbose=FALSE)
	})
	names(res) <- layers[hasF]
	
	if (sum(hasF)==1) {
		res[[1]]
	} else {
		res
	}
}
