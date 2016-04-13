#' Thematic Maps
#'
#' Thematic maps are geographical maps in which spatial data distributions are visualized. This package offers a flexible, layer-based, and easy to use approach to create thematic maps, such as choropleths and bubble maps. It is based on the grammar of graphics, and resembles the syntax of ggplot2.
#' 
#' This page provides a brief overview of all package functions. See \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}} for a short introduction with examples. See \href{../doc/tmap-modes.html}{\code{vignette("tmap-modes")}} for a short demo of the two output modes: plot and interactive view.
#'
#' @section Quick plotting method:
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
#' \code{\link{tm_text}}\tab To create a layer of text labels \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Facetting (small multiples)
#' \tabular{ll}{
#' \code{\link{tm_facets}}\tab To define facets \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Attributes:
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
#' \code{\link{tm_view}}\tab Options for the interactive view mode \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Change options:
#' \tabular{ll}{
#' \code{\link{tmap_mode}}\tab To set the tmap mode: \code{"plot"} or \code{"view"}\cr
#' \code{\link{ttm}}\tab To toggle between the modes\cr
#' \code{\link{tmap_style}}\tab To set the default style \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' @section Handy tool functions: 
#' \tabular{ll}{
#' \code{\link{bb}}\tab To create, extract or modify a bounding box \cr
#' \code{\link{geocode_OSM}}\tab To get geocode based on location \cr
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
#' \code{\link{smooth_map}}\tab To create a smooth map (raster, contour lines and dasymetric polygons) \cr
#' \code{\link{smooth_raster_cover}}\tab To create a smooth cover of a raster object \cr
#' \code{\link{sample_dots}}\tab To sample dots from polygons \cr
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
#' \code{\link{print}}\tab Plot in graphics device or view interactively in web browser or RStudio's viewer pane \cr
#' \code{\link{tmap_leaflet}}\tab Obtain a leaflet widget object \cr
#' \code{\link{animation_tmap}}\tab Create an animation \cr
#' \code{\link{save_tmap}}\tab To save thematic maps (both in plot and view mode) \cr
#' \code{\link{write_shape}}\tab To write a shape object \cr
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
#' @seealso \href{../doc/tmap-modes.html}{\code{vignette("tmap-modes")}}
NULL

#' World, Europe, and Netherlands map
#' 
#' Maps of the world, Europe, and the Netherlands (province and municipality level), class \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}
#' 
#' The default projections for these maps are Eckhart IV (World), Lambert azimuthal (Europe), and Rijksdriehoekstelsel (Netherlands). See below. To change the projection, use \code{\link{set_projection}}. Alternatively, the projection can be changed temporarily for plotting purposes by using the projection arugment of \code{\link{tm_shape}} (or \code{\link{qtm}}).
#' 
#' \code{World} World map. The default projection for this world map is Eckhart IV since area sizes are preserved, which is a very important property for choropleths.
#' 
#' \code{Europe} Europe map. The ETRS-LAEA projection is used by default for this map. This projection, as well as the bounding box, are also used in the maps published by Eurostat. Several countries are transcontinental and are partly located in Asia. From these countries, only data from Russia and Turkey have been included in this shape file since they are widely considered as European countries. The data is taken from the \code{World} data, where variables \code{"part"} and \code{"EU_Schengen"} have been added.
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
#' @references Production of Global Land Cover Data - GLCNMO2008, Tateishi, R., Thanh Hoan, N., Kobayashi, T., Alsaaideh, B., Tana, G., Xuan Phong, D. (2014), Journal of Geography and Geology, 6 (3).
#' @source \url{http://www.iscgm.org/gm/glcnmo.html}
NULL


#' tmap element
#'
#' Building block for drawing thematic maps. All element functions have the prefix \code{tm_}.
#' 
#' The fundamental, and hence required element is
#' \itemize{
#' \item \code{\link{tm_shape}} that specifies the shape object, and also specifies the projection and bounding box}
#' 
#' The elements that serve as aesthetics layers are
#' \itemize{
#' \item \code{\link{tm_fill}} to fill the polygons
#' \item \code{\link{tm_borders}} to draw polygon borders
#' \item \code{\link{tm_polygons}} to draw polygons (it is a combination of \code{\link{tm_fill}} and \code{\link{tm_borders}})
#' \item \code{\link{tm_bubbles}} to draw bubbles
#' \item \code{\link{tm_lines}} to draw lines
#' \item \code{\link{tm_text}} to add text annotations
#' \item \code{\link{tm_raster}} to draw a raster}
#' 
#' The layers can be stacked by simply adding them with the + symbol. The combination of the elements described above form one group. Multiple groups can be stacked. Each group should start with \code{\link{tm_shape}}.
#' 
#' The attribute elements are
#' \itemize{
#' \item \code{\link{tm_grid}} to specify coordinate grid lines
#' \item \code{\link{tm_credits}} to add a credits/acknowledgements text label
#' \item \code{\link{tm_scale_bar}} to add a measurement scale bar
#' \item \code{\link{tm_compass}} to add a map compass
#' }
#' 
#' The element \code{\link{tm_facets}} specifies facets (small multiples). The element \code{\link{tm_layout}} is used to change the layout of the map.
#' 
#' @name tmap-element
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @seealso \href{../doc/tmap-modes.html}{\code{vignette("tmap-modes")}}
#' @seealso The examples in each of the element functions
NULL