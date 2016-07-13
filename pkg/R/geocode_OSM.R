#' Geocodes a location using OpenStreetMap Nominatim
#' 
#' Geocodes a location (based on a search query) to coordinates and a bounding box. Similar to geocode from the ggmap package. It uses OpenStreetMap Nominatim.
#' 
#' @param q a character that specifies a search query. For instance \code{"India"} or \code{"CBS Weg 11, Heerlen, Netherlands"}.
#' @param projection projection in which the coordinates and bounding box are returned. Either a \code{PROJ.4} character string or a shortcut. See \code{\link{get_proj4}} for a list of shortcut values.
#' @export
#' @importFrom XML xmlChildren xmlRoot xmlAttrs xmlTreeParse
#' @return A list of two items: \code{"coords"}, a vector containing the longitude and latitude coordinates, and \code{"bbox"}, the corresponding bounding box.
#' @example ../examples/geocode_OSM.R
#' @seealso \code{\link{bb}}
geocode_OSM <- function(q, projection="longlat") {
	q <- gsub(" ", "+", enc2utf8(q), fixed = TRUE)
	addr <- paste0("http://nominatim.openstreetmap.org/search?q=", q, "&format=xml&polygon=0&addressdetails=0")
	
	tmpfile <- tempfile()
	suppressWarnings(download.file(addr, destfile = tmpfile, mode= "wb", quiet = TRUE))
	
	doc <- xmlTreeParse(tmpfile)
	unlink(tmpfile)
	if (length(xmlChildren(xmlRoot(doc)))==0) stop(paste("No results found for \"", q, "\".", sep=""))
	first_search_result <- xmlChildren(xmlRoot(doc))[[1]]
	bbx <- xmlAttrs(first_search_result)["boundingbox"]
	coords <- c(lon=as.numeric(xmlAttrs(first_search_result)["lon"]),
				lat=as.numeric(xmlAttrs(first_search_result)["lat"]))
	
	b <- matrix(as.numeric(unlist(strsplit(bbx, ","))), ncol=2, byrow=TRUE)[2:1,]
	dimnames(b) <- list(c("x", "y"), c("min", "max"))
	
	if (projection!="longlat") {
		current.projection <- get_proj4("longlat")
		prj <- get_proj4(projection)
		single_point <- SpatialPoints(matrix(coords, nrow=1), proj4string=CRS(current.projection))
		coords <- as.vector(attr(set_projection(single_point, projection = prj), "coords"))
		names(coords) <- c("x", "y")
		b <- bb(b, current.projection = current.projection, projection = prj)
	}
	
	list(coords=coords, bbox=b)
}
