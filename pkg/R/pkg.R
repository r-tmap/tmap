#' Tools to create thematic maps
#'
#' \tabular{ll}{
#' Package: \tab geo\cr
#' Type: \tab Package\cr
#' Version: \tab 0.6\cr
#' Date: \tab 2014-07-23\cr
#' License: \tab GPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' This packages offers a complete workaround to create thematic maps. Maps are plotted in \pkg{ggplot2} style.
#' This package includes ready to use shape files from the World, Europe and the Netherlands (both provinces as municipalities), which are used in the examples.
#' 
#' To get started, see \href{../doc/geo-nutshell.html}{\code{vignette("geo-nutshell")}}.
#' 
#' @name geo-package
#' @aliases geo-package
#' @docType package
#' @author Martijn Tennekes \email{mtennekes@@gmail.com}
#' @keywords GIS cartography
NULL

#' World, Europe and Netherlands map
#' 
#' Maps of the world, Europe and the Netherlands (province and municipality level).
#' 
#' \code{World} World map. The projection that is chosen for this world map is Eckhart IV since area sizes are preserved, which is a very important property for statistical purposes.
#' 
#' \code{Europe} Europe map. Lambert azimuthal equal-area projection is used by default for this map. Several countries are transcontinental and are partly located in Asia. From these countries, only Russia and Turkey have been included in this map as part of Europe since they are widely considered as European countries. Other transcontinental countries Azerbaijan, Georgia, and Kazakhstan, are also included in the map, but only passively. From the other surrounding countries, only Greenland is removed from the map, since it interferes with the map title.
#' 
#' \code{NLD_prov} and \code{NLD_muni}, maps of the Netherlands at province and municipality level of 2013. The used projection is the Rijksdriehoekstelsel projection. \strong{Important:} publication of these maps is only allowed when cited to Statistics Netherlands (CBS) and Kadaster Nederland as source.
#' 
#' @name World
#' @rdname Shapes
#' @docType data
NULL


#' @name Europe
#' @rdname Shapes
#' @docType data
NULL

#' @name NLD_prov
#' @rdname Shapes
#' @docType data
NULL

#' @name NLD_muni
#' @rdname Shapes
#' @docType data
NULL



#' Spatial data of cities and rivers
#' 
#' Spatial data of main world cities and rivers.
#' 
#' @name rivers
#' @rdname Shapes2
#' @docType data
#' @source http://www.naturalearthdata.com/
NULL

#' @name cities
#' @rdname Shapes2
#' @docType data
NULL



#' Geo element
#'
#' Building block to draw thematic maps.
#' 
#' The only fundamental, and hence required element is
#' \itemize{
#' \item \code{\link{geo_shape}} that specifies the shape object, and also controls the projection and bounding box}
#' 
#' The elements that serve as drawing layers are
#' \itemize{
#' \item \code{\link{geo_borders}} to draw polygon borders
#' \item \code{\link{geo_fill}} to color the polygons
#' \item \code{\link{geo_bubbles}} to draw bubbles
#' \item \code{\link{geo_lines}} to draw lines}
#' 
#' The layers can be stacked by simply adding them with the + symbol. The combination of the elements described above form one group. Multiple groups can be stacked. Each group should start with \code{\link{geo_shape}}.
#' 
#' The layout elements are
#' \itemize{
#' \item \code{\link{geo_theme}} to change the appearance of the map, for instance titles and legend positions. Predefined themes for the example shape files are \code{\link{geo_theme_World}}, \code{\link{geo_theme_Europe}}, and \code{\link{geo_theme_NLD}}.
#' \item \code{\link{geo_facets}} that specifies how small multiples are created, i.e. how many rows and colums, and whether the statistical data variables have free scales or not.
#' \item \code{\link{geo_grid}} that specifies grid lines
#' }
#'    
#' @name geo-element
NULL