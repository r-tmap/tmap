#' Statistical geographic maps
#'
#' \tabular{ll}{
#' Package: \tab geo\cr
#' Type: \tab Package\cr
#' Version: \tab 0.4\cr
#' Date: \tab 2014-04-25\cr
#' License: \tab GPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Tools for creating thematic cartograpic maps.
#' \itemize{
#' \item{ggplot2-style:
#' \itemize{
#' \item Stack elements intuitively (like ggplot) with great flexibility. The name of each element starts with \code{geo_}. The foundation is \code{geo_shape}, the simple drawing elements are \code{geo_borders}, \code{geo_fill}, and \code{geo_borders}, the statistical elements are \code{geo_choropleth} and \code{geo_bubblemap}, the theme element is \code{geo_theme} and the element to control small multiples is \code{geo_grid}.
#' \item Create quick maps (like qplot) with \code{geo}. All the elements above can be called within one function call.
#' }}
#' \item{Handy functions for working with shape files (SpatialPolygonsDataFrames)
#'\itemize{
#' \item read shape files directly or from a network repository: \code{\link{get_shape}};
#' \item append data to shape files: \code{\link{append_data}};
#' \item crop shape files while retaining the data with \code{\link{crop_shape}}.
#' \item convert from RD (rijksdriehoekstelsel) to wgs84 coordinates: \code{\link{rd2wgs84}};
#' \item (not working yet) export to kml format (for Google Earth): \code{shp2kml};
#' \item (not working yet) interactive cartography: \code{igeo}.
#' }}
#' \item{Example shape files of the World, Europe and the Netherlands have been included}}
#' 
#' @name geo-package
#' @aliases geo-package
#' @docType package
#' @author Martijn Tennekes \email{mtennekes@@gmail.com}
#' @keywords GIS cartography
NULL

#' World map
#' 
#' World map. The projection that is chosen for this world map is Eckhart IV since area sizes are preserved, which is a very important property for statistical purposes.
#'
#' @name World
#' @docType data
NULL

#' Europe map
#' 
#' Europe map. Lambert azimuthal equal-area projection is used by default for this map. Several countries are transcontinental and are partly located in Asia. From these countries, only Russia and Turkey have been included in this map as part of Europe since they are widely considered as European countries. Other transcontinental countries Azerbaijan, Georgia, and Kazakhstan, are also included in the map, but only passively. From the other surrounding countries, only Greenland is removed from the map, since it interferes with the map title.
#'
#' @name Europe
#' @docType data
NULL


#' Netherlands map (provinces)
#' 
#' Netherlands map (provinces)
#'
#' @name NLD_prov
#' @docType data
NULL

#' Netherlands map (municipalities)
#' 
#' Netherlands map (municipalities)
#'
#' @name NLD_muni
#' @docType data
NULL


#' Netherlands municipalities data (age groups)
#' 
#' Netherlands municipalities data (age groups)
#'
#' @name NLD_ageGroups
#' @docType data
NULL

#' Geo layer
#'
#' Geo layer created by any \code{geo_} function.
#'
#' @name geo-object
NULL