#' Tools to create thematic maps
#'
#' \tabular{ll}{
#' Package: \tab geo\cr
#' Type: \tab Package\cr
#' Version: \tab 0.5\cr
#' Date: \tab 2014-05-22\cr
#' License: \tab GPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' This packages offers a complete workaround to create thematic maps. Maps are plotted in \pkg{ggplot2} style:
#' \code{data(World)}
#' \code{geo_shape(World) +}
#' \code{geo_fill("pop_est_dens", style="kmeans", palette="YlOrRd") +}
#' \code{geo_borders() +}
#' \code{geo_text("iso_a3", cex="AREA", cex.lowerbound=.4, bg.alpha=0) +}
#' \code{geo_theme_World(title="Population density per km2")}
#'
#' The name of each element starts with \code{geo_}. The required element is \code{\link{geo_shape}}, which load the shape object (in this case \code{\link{World}}). 
#' 
#' The possible drawing elements are \code{\link{geo_borders}}, \code{\link{geo_fill}}, \code{\link{geo_bubbles}}, \code{\link{geo_lines}}, and \code{\link{geo_text}}. One or more of these elements stacked form one layer. Multiple layers can be stacked.
#'
#' The other elements are \code{\link{geo_theme}} to control the layout, \code{\link{geo_grid}} to draw grid lines, and \code{\link{geo_facets}} to control small multiples.
#' 
#' Create quick maps (like qplot) with \code{\link{geo}}. All the elements above can be called within one function call (although only one layer is supported):
#' \code{geo(World, fill="pop_est_dens", theme="World", style="kmeans", title="Population per km")}
#' 
#' Note: a crucial difference with ggplot2 is that the elements are functional building blocks rather than layers from the grammar of graphics.
#' 
#' Moreover, this package offers easy to use functions to
#' \itemize{
#' \item read ESRI shape files with \code{\link{read_shape}};
#' \item append data with \code{\link{append_data}};
#' \item change map projections with \code{\link{set_projection}};
#' \item crop maps with \code{\link{crop_shape}};
#' \item and approximate area sizes with \code{\link{approx_areas}}}. 
#' 
#' This package includes ready to use shape files from the World, Europe and the Netherlands (both provinces as municipalities), which are used in the examples.
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



#' rivers, cities and airports
#' 
#' Spatial data of main world cities, rivers, and airports.
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

#' @name airports
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