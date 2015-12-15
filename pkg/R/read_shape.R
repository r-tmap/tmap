#' Read shape file
#' 
#' Read an ESRI shape file. Optionally, set the current projection if it is missing.
#' 
#' This function is a convenient wrapper of rgdal's \code{\link[rgdal:readOGR]{readOGR}}. It is possible to set the current projection, if it is undefined in the shape file. If a reprojection is required, use \code{\link{set_projection}}.
#' 
#' For the Netherlands: often, the Dutch Rijksdriehoekstelsel (Dutch National Grid) projection is provided in the shape file without proper datum shift parameters to wgs84. This functions automatically adds these parameters. See \url{http://www.qgis.nl/2011/12/05/epsg28992-of-rijksdriehoekstelsel-verschuiving/} (in Dutch) for details.
#' 
#' @param file a shape file name (including directory)
#' @param current.projection the current projection of the shape object, if it is missing in the shape file. It should be either a \code{PROJ.4} character string (see \url{http://trac.osgeo.org/proj/}), of one of the following shortcuts: 
#' \describe{
#'    	\item{\code{"longlat"}}{Not really a projection, but a plot of the longitude-latitude coordinates (WGS84 datum).} 
#'    	\item{\code{"wintri"}}{Winkel Tripel (1921). Popular projection that is useful in world maps. It is the standard of world maps made by the National Geographic Society. Type: compromise} 
#'    	\item{\code{"robin"}}{Robinson (1963). Another popular projection for world maps. Type: compromise}
#'    	\item{\code{"eck4"}}{Eckert IV (1906). Projection useful for world maps. Area sizes are preserved, which makes it particularly useful for truthful choropleths. Type: equal-area}
#'    	\item{\code{"hd"}}{Hobo-Dyer (2002). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"gall"}}{Gall (Peters) (1855). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"merc"}}{Mercator (1569). Projection in which shapes are locally preserved. However, areas close to the poles are inflated. Google Maps uses a close variant of the Mercator. Type: conformal}
#'    	\item{\code{"utmXX(s)"}}{Universal Transverse Mercator. Set of 60 projections where each projection is a traverse mercator optimized for a 6 degree longitude range. These ranges are called UTM zones. Zone \code{01} covers -180 to -174 degrees (West) and zone \code{60} 174 to 180 east. Replace XX in the character string with the zone number. For southern hemisphere, add \code{"s"}. So, for instance, the Netherlands is \code{"utm31"} and New Zealand is \code{"utm59s"}}
#'    	\item{\code{"mill"}}{Miller (1942). Projetion based on Mercator, in which poles are displayed. Type: compromise}
#'    	\item{\code{"eqc0"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The equator is the standard parallel. Also known as Plate Carr\'ee. Type: equidistant}
#'    	\item{\code{"eqc30"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 30 is the standard parallel. Type: equidistant}
#'    	\item{\code{"eqc45"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 45 is the standard parallel. Also known as Gall isographic. Type: equidistant}
#'    	\item{\code{"rd"}}{Rijksdriehoekstelsel. Triangulation coordinate system used in the Netherlands.}}
#'    	See \url{http://en.wikipedia.org/wiki/List_of_map_projections} for a overview of projections.
#' Use \code{\link{set_projection}} to reproject the shape object.
#' @param ... other parameters, such as \code{stringsAsFactors}, are passed on to \code{\link[rgdal:readOGR]{readOGR}}
#' @return shape object
#' @importFrom rgdal readOGR
#' @import sp
#' @export
read_shape <- function(file, current.projection=NULL, ...){
	
	# determine region ID
	if (file.exists(file)) {
		fullfilename <- file
	} else stop("unknown filename", call. = FALSE)

	dir <- dirname(fullfilename)
	base <- basename(fullfilename)
	if (substr(base, nchar(base)-3, nchar(base))==".shp")
		base <- substr(base, 1, nchar(base)-4)
	
	shp <- readOGR(dir, base, verbose=FALSE, ...)
	
	prj <- proj4string(shp)
	
	if (is.na(prj)) {
		if (missing(current.projection)) {
			warning("Current projection missing. Set the parameter current.project, or use set_projection", call. = FALSE)
		} else {
			shp <- set_projection(shp, current.projection=current.projection)
		}
	} else {
		if (!missing(current.projection)) {
			warning("Projection already specified in shape file. Use set_projection for reprojection.", call. = FALSE)
		}
			
		## rd projection correction: add towgs84 parameter to frequently used rd projection strings
		if (prj %in% c("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs",
					   "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs",
					   "+proj=sterea +lat_0=52.156161 +lon_0=5.387639 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")) {
			shp <- suppressWarnings(set_projection(shp, current.projection="rd", overwrite.current.projection=TRUE))
		}
	}
			
	shp
}

#' Write shape file
#' 
#' Write a shape object to an ESRI shape file.
#' 
#' This function is a convenient wrapper of rgdal's \code{\link[rgdal:writeOGR]{writeOGR}}.
#'
#' @param shp a shape object
#' @param file file name (including directory)
#' @import sp
#' @importFrom rgdal writeOGR
#' @export
write_shape <- function(shp, file) {
	shpname <- deparse(substitute(shp))
	dir <- dirname(file)
	base <- basename(file)
	if (!file.exists(dir)) stop("unknown directory", call. = FALSE)

	if (substr(base, nchar(base)-3, nchar(base))==".shp")
		base <- substr(base, 1, nchar(base)-4)
	if (!inherits(shp, "Spatial")) stop("shpname is not a Spatial object", call. = FALSE)
	writeOGR(shp, dir, base, driver = "ESRI Shapefile", overwrite_layer=TRUE)
}