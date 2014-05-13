#' Set projection
#' 
#' This function sets the projection of a shape file. It is a convenient wrapper of \code{\link[sp:]{spTransform}} with shortcuts for commonly used projections. The projection can also be set during the plot call in \code{\link{geo_shape}}.
#'
#' @param shp shape object. For \code{\link{geo_choropleth}} and \code{\link{geo_bubblemap}}, a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} or a \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} is requied. \code{\link[sp:SpatialPoints]{SpatialPoints}} and \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} are only used for \code{\link{geo_bubblemap}} and \code{\link{geo_bubbles}}.
#' @param projection character that determines the projectino. Either a \code{PROJ.4} character string (see \url{http://trac.osgeo.org/proj/}), of one of the following shortcuts: 
#' \describe{
#'    	\item{\code{"longlat"}}{Not really a projection, but a plot of the longitude-latitude coordinates.} 
#'    	\item{\code{"wintri"}}{Winkel Tripel (1921). Popular projection that is useful in world maps. It is the standard of world maps made by the National Geographic Society. Type: compromise} 
#'    	\item{\code{"robin"}}{Robinson (1963). Another popular projection for world maps. Type: compromise}
#'    	\item{\code{"eck4"}}{Eckert IV (1906). Projection useful for world maps. Area sizes are preserved, which makes it particularly useful for truthful choropleths. Type: equal-area}
#'    	\item{\code{"hd"}}{Hobo-Dyer (2002). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"gall"}}{Gall (Peters) (1855). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"merc"}}{Mercator (1569). Projection in which shapes are locally preserved. However, areas close to the poles are inflated. Google Maps uses a close variant of the Mercator. Type: conformal}
#'    	\item{\code{"mill"}}{Miller (1942). Projetion based on Mercator, in which poles are displayed. Type: compromise}
#'    	\item{\code{"eqc0"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The equator is the standard parallel. Also known as Plate Carr\'ee. Type: equidistant}
#'    	\item{\code{"eqc30"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 30 is the standard parallel. Type: equidistant}
#'    	\item{\code{"eqc45"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 45 is the standard parallel. Also known as Gall isographic. Type: equidistant}
#'    	\item{\code{"rd"}}{Rijksdriehoekstelsel. Triangulation coordinate system used in the Netherlands.}}
#'    	See \url{http://en.wikipedia.org/wiki/List_of_map_projections} for a overview of projections.
#'    	By default, the projection is used that is defined in the \code{shp} object itself.
#' @param transform Logical that determines whether to transform the shape file into the specified projection. By default \code{TRUE}. If the current shape projection is missing, longitude latitude coordinates (WGS84) are assumed. If \code{FALSE}, then the specified projection is simply written to the shape file without transforming it (use this at your own risk!). 
#' @import sp
#' @export
set_projection <- function(shp, projection, transform=TRUE) {
	projection <- switch(projection,
						 longlat="+proj=longlat +datum=WGS84",
						 wintri="+proj=wintri",
						 robin="+proj=robin",
						 eck4="+proj=eck4",
						 hd="+proj=cea +lat_ts=37.5",
						 gall="+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45",
						 merc="+proj=merc",
						 mill="+proj=mill",
						 eqc0="+proj=eqc",
						 eqc30="+proj=cea +lat_ts=30",
						 eqc45="+proj=cea +lat_ts=45",
						 rd="+init=epsg:28992 +towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812",
						 projection)
	if (transform) {
		shp.proj <- proj4string(shp)
		if (is.na(shp.proj)) {
			warning("Currect projection of shape object unknown. Long-lat (WGS84) is assumed.")
			shp@proj4string <- CRS("+proj=longlat +datum=WGS84")
		}
		spTransform(shp, CRS(projection))
	} else {
		shp@proj4string <- CRS(projection)
		shp
	}
}
