#' Thematic Maps
#'
#' Thematic maps are geographical maps in which statistical data are visualized. This package offers a flexible, layer-based, way to create thematic maps, such as choropleths and bubble maps. It is based on the grammar of graphics, and resembles the syntax of ggplot2.
#' 
#' This page provides a brief overview of all package functions. See \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}} for a short manual with examples.
#'
#' @section Quick plotting:
#' \tabular{ll}{
#' \code{\link{qtm}}\tab To plot a thematic map \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#'
#' @section Main plotting method:
#' Shape specification:
#' \tabular{ll}{
#' \code{\link{tm_shape}}\tab To specify a shape object \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Aesthetics layers:
#' \tabular{ll}{
#' \code{\link{tm_fill}}\tab To create a polygon layer (without borders) \cr
#' \code{\link{tm_borders}}\tab To create polygon borders \cr
#' \code{\link{tm_polygons}}\tab To create a polygon layer with borders \cr
#' \code{\link{tm_bubbles}}\tab To create a layer of bubbles \cr
#' \code{\link{tm_dots}}\tab To create a layer of dots \cr
#' \code{\link{tm_lines}}\tab To create a layer of lines \cr
#' \code{\link{tm_raster}}\tab To create a raster layer \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Attribute layers:
#' \tabular{ll}{
#' \code{\link{tm_grid}}\tab To create grid lines \cr
#' \code{\link{tm_scale_bar}}\tab To create a scale bar \cr
#' \code{\link{tm_compass}}\tab To create a map compass \cr
#' \code{\link{tm_credits}}\tab To create a text for credits \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Layout element:
#' \tabular{ll}{
#' \code{\link{tm_layout}}\tab To adjust the layout (main function)\cr
#' \code{\link{tm_legend}}\tab Shortcut to adjust the legend \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' @section Handy tool functions: 
#' \tabular{ll}{
#' \code{\link{bb}}\tab To create, extract or modify a bounding box \cr
#' \code{\link{get_asp_ratio}}\tab To get the aspect ratio of a shape object \cr
#' \code{\link{get_IDs}}\tab To get ID values of a shape object \cr
#' \code{\link{append_data}}\tab To append a data frame to a shape object \cr
#' \code{\link{approx_areas}}\tab To approximate area sizes of polygons \cr
#' \code{\link{calc_densities}}\tab To calculate density values \cr
#' \code{\link{get_projection}}\tab To get the map projection \cr
#' \code{\link{set_projection}}\tab To set the map projection \cr
#' \code{\link{split}}\tab To split a shape object \cr
#' \code{\link{sbind}}\tab To bind shape objects \cr
#' \code{\link{map_coloring}}\tab To color polygons with different colors for adjacent polygons \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' @section Generate spatial objects: 
#' \tabular{ll}{
#' \code{\link{smooth_map}}\tab To create a smooth map (raster, contour lines and dasymetric polygons \cr
#' \code{\link{smooth_raster_cover}}\tab To create a smoothed cover of a raster object \cr
#' \code{\link{sample_dot}}\tab To sample dots from polygons \cr
#' \code{\link{points_to_raster}}\tab To bin spatial points to a raster \cr
#' \code{\link{poly_to_raster}}\tab To convert polygons to a raster \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' @section Input functions: 
#' \tabular{ll}{
#' \code{\link{read_shape}}\tab To read a shape object \cr
#' \code{\link{read_GPX}}\tab To read a GPX file \cr
#' \code{\link{read_osm}}\tab To read Open Street Map data \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' @section Output functions: 
#' \tabular{ll}{
#' \code{\link{itmap}}\tab Interactive tmap widget \cr
#' \code{\link{animation_map}}\tab Create an animation \cr
#' \code{\link{save_tmap}}\tab To save thematic maps \cr
#' \code{\link{write_shape}}\tab To write a shape object \cr
#' \code{\link{get_projection}}\tab To get the map projection \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' @section Spatial datasets: 
#' \tabular{ll}{
#' \code{\link{World}}\tab World country data (spatial polygons) \cr
#' \code{\link{Europe}}\tab European country data (spatial polygons) \cr
#' \code{\link{NLD_prov}}\tab Netherlands province data (spatial polygons) \cr
#' \code{\link{NLD_muni}}\tab Netherlands municipal data (spatial polygons) \cr
#' \code{\link{metro}}\tab Metropolitan araes (spatial points) \cr
#' \code{\link{rivers}}\tab Rivers (spatial lines) \cr
#' \code{\link{land}}\tab Global land cover (spatial grid)\cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' @name tmap-package
#' @aliases tmap
#' @docType package
#' @author Martijn Tennekes \email{mtennekes@@gmail.com}
#' @keywords GIS, thematic maps, statistical maps, choropleth, bubble map
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
NULL

#' World, Europe, and Netherlands map
#' 
#' Maps of the world, Europe, and the Netherlands (province and municipality level), class \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}
#' 
#' The default projections for these maps are Eckhart IV (World), Lambert azimuthal (Europe), and Rijksdriehoekstelsel (Netherlands). See below. To change the projection, use \code{\link{set_projection}}. Alternatively, the projection can be changed temporarily for plotting purposes by using the projection arugment of \code{\link{tm_shape}} (or \code{\link{qtm}}).
#' 
#' \code{World} World map. The default projection for this world map is Eckhart IV since area sizes are preserved, which is a very important property for choropleths.
#' 
#' \code{Europe} Europe map. Lambert azimuthal equal-area projection is used by default for this map. Several countries are transcontinental and are partly located in Asia. From these countries, only Russia and Turkey have been included in this map as part of Europe since they are widely considered as European countries. Other transcontinental countries Azerbaijan, Georgia, and Kazakhstan, are also included in the map, but only as background (so without data). From the other surrounding countries, only Greenland is removed from the map, since it interferes with the prefered map legend position at the lop left.
#' 
#' \code{NLD_prov} and \code{NLD_muni}, maps of the Netherlands at province and municipality level of 2013. The used projection is the Rijksdriehoekstelsel projection. \strong{Important:} publication of these maps is only allowed when cited to Statistics Netherlands (CBS) and Kadaster Nederland as source.
#' 
#' @usage data(World)
#' @name World
#' @rdname Shapes
#' @docType data
#' @source \url{http://www.naturalearthdata.com} for \code{World} and \code{Europe}
#' @source \url{http://www.happyplanetindex.org} for \code{World} and \code{Europe}
#' @source \url{http://www.cbs.nl} for \code{NLD_prov} and \code{NLD_muni}. 
#' @references Statistics Netherlands (2014), The Hague/Heerlen, Netherlands, \url{http://www.cbs.nl}.
#' @references Kadaster, the Netherlands' Cadastre, Land Registry, and Mapping Agency (2014), Apeldoorn, Netherlands, \url{http://www.kadaster.nl}.
#' @references Statistics Netherlands (2014), The Hague/Heerlen, Netherlands, \url{http://www.cbs.nl}.
NULL


#' @usage data(Europe)
#' @name Europe
#' @rdname Shapes
#' @docType data
NULL

#' @usage data(NLD_prov)
#' @name NLD_prov
#' @rdname Shapes
#' @docType data
NULL

#' @usage data(NLD_muni)
#' @name NLD_muni
#' @rdname Shapes
#' @docType data
NULL



#' Spatial data of rivers
#' 
#' Spatial data of rivers, of class \code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}}
#' 
#' @usage data(rivers)
#' @name rivers
#' @docType data
#' @source \url{http://www.naturalearthdata.com}
NULL

#' Spatial data of metropolitan areas
#' 
#' Spatial data of metropolitan areas, of class \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}}. The data includes a population times series from 1950 to (forecasted) 2030. All metro areas with over 1 million inhabitants in 2010 are included.
#' 
#' @usage data(metro)
#' @name metro
#' @docType data
#' @references United Nations, Department of Economic and Social Affairs, Population Division (2014). World Urbanization Prospects: The 2014 Revision, CD-ROM Edition.
#' @source \url{http://esa.un.org/unpd/wup/CD-ROM/}
NULL

#' Spatial data of global land cover
#' 
#' Spatial data of global land cover, of class \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}}. The data includes a population times series from 1950 to (forecasted) 2030. All metro areas with over 1 million inhabitants in 2010 are included.
#' 
#' \strong{Important:} publication of these maps is only allowed when cited to Tateishi et al. (2014), and when "Geospatial Information Authority of Japan, Chiba University and collaborating organizations." is shown. See \url{http://www.iscgm.org/gm/glcnmo.html#use}.
#' 
#' @usage data(land)
#' @name land
#' @docType data
#' @references Tateishi, R., Thanh Hoan, N., Kobayashi, T., Alsaaideh, B., Tana, G., Xuan Phong, D. (2014), Journal of Geography and Geology, 6 (3).
#' @source \url{http://www.iscgm.org/gm/glcnmo.html}
NULL


#' tmap element
#'
#' Building block for drawing thematic maps.
#' 
#' The only fundamental, and hence required element is
#' \itemize{
#' \item \code{\link{tm_shape}} that specifies the shape object, and also controls the projection and bounding box}
#' 
#' The elements that serve as drawing layers are
#' \itemize{
#' \item \code{\link{tm_fill}} to fill the polygons
#' \item \code{\link{tm_borders}} to draw polygon borders
#' \item \code{\link{tm_polygons}} to draw polygons (it is a combination of \code{\link{tm_fill}} and \code{\link{tm_borders}})
#' \item \code{\link{tm_bubbles}} to draw bubbles
#' \item \code{\link{tm_lines}} to draw lines
#' \item \code{\link{tm_text}} to add text annotations
#' \item \code{\link{tm_raster}} to draw a raster}
#' 
#' The layers can be stacked by simply adding them with the + symbol. The combination of the elements described above form one group. Multiple groups can be stacked. Each group should start with \code{\link{tm_shape}} (see examples below).
#' 
#' The layout elements are
#' \itemize{
#' \item \code{\link{tm_layout}} to change the appearance of the map, for instance, legend position, background color, and margins. Predefined formatting themes for the example shape objects are \code{\link{tm_format_World}}, \code{\link{tm_format_Europe}}, and \code{\link{tm_format_NLD}}.
#' \item \code{\link{tm_facets}} that specifies how small multiples are created, i.e. how many rows and colums, and whether the statistical data variables have free scales or not.
#' \item \code{\link{tm_grid}} that specifies coordinate grid lines
#' \item \code{\link{tm_credits}} that adds a credits/acknowledgements text label
#' \item \code{\link{tm_scale_bar}} that adds a measurement scale bar
#' }
#'    
#' @name tmap-element
#' @example ../examples/tmap.R
NULL