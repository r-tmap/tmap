#' Set and get the map projection
#' 
#' The function \code{set_projection} sets the projection of a shape file. It is a convenient wrapper of \code{\link[sp:spTransform]{spTransform}} with shortcuts for commonly used projections. The projection can also be set directly in the plot call with \code{\link{tm_shape}}. This function is also used to set the current projection information without transformation of the shape object, which is useful when this information is missing in the shape object. The function \code{get_projection} is used to get the projection information.
#'
#' @param shp shape object, which is one of 
#' \itemize{
#'  \item{"1)"}\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}
#'  \item{"2)"}\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}
#'  \item{"3)"}\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}
#' }
#' @param projection character that determines the projection. Either a \code{PROJ.4} character string or one of the following shortcuts: 
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
#'    	See \url{http://trac.osgeo.org/proj/} for the \code{PROJ.4} project home page. An extensive list of \code{PROJ.4} codes can be created with rgdal's \code{\link[rgdal:make_EPSG]{make_EPSG}}.
#'    	By default, the projection is used that is defined in the \code{shp} object itself.
#' @param current.projection the current projection of \code{shp}. Only use this if the current projection is missing.
#' @param transform Logical that determines whether to transform the shape file into the specified projection. By default \code{TRUE}. If the current shape projection is missing, longitude latitude coordinates (WGS84) are assumed. If \code{FALSE}, then the specified projection is simply written to the shape file without transforming it (use this at your own risk!). 
#' @param overwrite.current.projection logical that determines whether the current projection is overwritten if it already has a projection that is different.
#' @name set_projection
#' @rdname set_projection
#' @import sp
#' @return \code{set_projection} returns a (transformed) shape object with updated projection information. 
#' \code{get_projection} returns the \code{PROJ.4} character string of \code{shp}.
#' @export
set_projection <- function(shp, projection=NULL, current.projection=NULL, transform=!is.null(projection), overwrite.current.projection=FALSE) {
	shp.name <- deparse(substitute(shp))
	shp.proj <- proj4string(shp)

	current.proj4 <- get_proj4_code(current.projection)

	if (is.na(shp.proj)) {
		if (missing(current.projection)) {
			stop("Currect projection of shape object unknown. Please specify the argument current.projection. The value \"longlat\", which stands for Longitude-latitude (WGS84), is most commonly used.")
		} else {
			shp@proj4string <- CRS(current.proj4)
		}
	} else {
		if (!missing(current.projection)) {
			if (current.proj4==shp.proj) {
				warning(paste("Current projection of", shp.name, "already known."))
			}else {
				if (overwrite.current.projection) {
					warning(paste("Current projection of", shp.name, "differs from", current.projection, ", but is overwritten."))
					shp@proj4string <- CRS(current.proj4)
				} else {
					stop(paste(shp.name, "already has projection:", shp.proj, "This is different from the specified current projection", current.projection, ". If the specified projection is correct, use overwrite.current.projection=TRUE."))
				}
			} 
		}
	}
	
	if (transform) {
		if (missing(projection)) stop("Please specify projection when transform=TRUE")
		proj4 <- get_proj4_code(projection)
		shp <- spTransform(shp, CRS(proj4))
	}
		
	shp
}


#' @name get_projection
#' @rdname set_projection
#' @import sp
#' @export
get_projection <- function(shp) {
	proj4string(shp)
}



