#' Thematic Map Visualization
#'
#' Thematic maps are geographical maps in which spatial data distributions are visualized. This package offers a flexible, layer-based, and easy to use approach to create thematic maps, such as choropleths and bubble maps. It is based on the grammar of graphics, and resembles the syntax of ggplot2.
#' 
#' This page provides a brief overview of all package functions. See \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}} for a short introduction with examples.
#'
#' @section Quick plotting method:
#' \tabular{ll}{
#' \code{\link{qtm}}\tab Plot a thematic map \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#'
#' @section Main plotting method:
#' Shape specification:
#' \tabular{ll}{
#' \code{\link{tm_shape}}\tab Specify a shape object \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Aesthetics base layers:
#' \tabular{ll}{
#' \code{\link{tm_polygons}}\tab Create a polygon layer (with borders) \cr
#' \code{\link{tm_symbols}}\tab Create a layer of symbols \cr
#' \code{\link{tm_lines}}\tab Create a layer of lines \cr
#' \code{\link{tm_raster}}\tab Create a raster layer \cr
#' \code{\link{tm_text}}\tab Create a layer of text labels \cr
#' \code{\link{tm_basemap}}\tab Create a layer of basemap tiles \cr
#' \code{\link{tm_tiles}}\tab Create a layer of overlay tiles \cr
#' }
#' 
#' Aesthetics derived layers:
#' \tabular{ll}{
#' \code{\link{tm_fill}}\tab Create a polygon layer (without borders) \cr
#' \code{\link{tm_borders}}\tab Create polygon borders \cr
#' \code{\link{tm_bubbles}}\tab Create a layer of bubbles \cr
#' \code{\link{tm_squares}}\tab Create a layer of squares \cr
#' \code{\link{tm_dots}}\tab Create a layer of dots \cr
#' \code{\link{tm_markers}}\tab Create a layer of markers \cr
#' \code{\link{tm_iso}}\tab Create a layer of iso/contour lines \cr
#' \code{\link{tm_rgb}}\tab Create a raster layer of an image \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Faceting (small multiples)
#' \tabular{ll}{
#' \code{\link{tm_facets}}\tab Define facets \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Attributes:
#' \tabular{ll}{
#' \code{\link{tm_grid}}\tab Create grid lines \cr
#' \code{\link{tm_scale_bar}}\tab Create a scale bar \cr
#' \code{\link{tm_compass}}\tab Create a map compass \cr
#' \code{\link{tm_credits}}\tab Create a text for credits \cr
#' \code{\link{tm_logo}}\tab Create a logo \cr
#' \code{\link{tm_xlab} and \link{tm_ylab}}\tab Create axis labels \cr
#' \code{\link{tm_minimap}}\tab Create a minimap (view mode only) \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Layout element:
#' \tabular{ll}{
#' \code{\link{tm_layout}}\tab Adjust the layout (main function)\cr
#' \code{\link{tm_legend}}\tab Adjust the legend \cr
#' \code{\link{tm_view}}\tab Configure the interactive view mode \cr
#' \code{\link{tm_style}}\tab Apply a predefined style \cr
#' \code{\link{tm_format}}\tab Apply a predefined format \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Change options:
#' \tabular{ll}{
#' \code{\link{tmap_mode}}\tab Set the tmap mode: \code{"plot"} or \code{"view"}\cr
#' \code{\link{ttm}}\tab Toggle between the modes \cr
#' \code{\link{tmap_options}}\tab Set global tmap options (from \code{\link{tm_layout}}, \code{\link{tm_view}}, and a couple of others) \cr
#' \code{\link{tmap_style}}\tab Set the default style \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' Create icons:
#' \tabular{ll}{
#' \code{\link{tmap_icons}}\tab Specify icons for markers or proportional symbols \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' 
#' @section Output functions: 
#' \tabular{ll}{
#' \code{\link{print}}\tab Plot in graphics device or view interactively in web browser or RStudio's viewer pane \cr
#' \code{\link{tmap_last}}\tab Redraw the last map \cr
#' \code{\link{tmap_leaflet}}\tab Obtain a leaflet widget object \cr
#' \code{\link{tmap_animation}}\tab Create an animation \cr
#' \code{\link{tmap_arrange}}\tab Create small multiples of separate maps \cr
#' \code{\link{tmap_save}}\tab Save thematic maps (either as image or HTML file) \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' @section Spatial datasets: 
#' \tabular{ll}{
#' \code{\link{World}}\tab World country data (\code{\link[sf:sf]{sf}} object of polygons) \cr
#' \code{\link{NLD_prov}}\tab Netherlands province data (\code{\link[sf:sf]{sf}} object of  polygons) \cr
#' \code{\link{NLD_muni}}\tab Netherlands municipal data (\code{\link[sf:sf]{sf}} object of  polygons) \cr
#' \code{\link{metro}}\tab Metropolitan areas (\code{\link[sf:sf]{sf}} object of points) \cr
#' \code{\link{rivers}}\tab Rivers (\code{\link[sf:sf]{sf}} object of lines) \cr
#' \code{\link{land}}\tab Global land cover (\code{\link[raster:brick]{RasterBrick}} object)\cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' @name tmap-package
#' @aliases tmap
#' @docType package
#' @author Martijn Tennekes \email{mtennekes@@gmail.com}
#' @concept GIS
#' @concept thematic maps
#' @concept statistical maps
#' @concept choropleth
#' @concept bubble map
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
NULL

#' World and Netherlands map
#' 
#' Maps of the world and the Netherlands (province and municipality level), class \code{\link[sf:sf]{sf}}
#' 
#' The default projections for these maps are Eckhart IV (World) and Rijksdriehoekstelsel (Netherlands). See below. The projection can be changed temporarily for plotting purposes by using the projection argument of \code{\link{tm_shape}} (or \code{\link{qtm}}).
#' 
#' \code{World} World map. The default projection for this world map is Eckhart IV since area sizes are preserved, which is a very important property for choropleths.
#' 
#' \code{NLD_prov} and \code{NLD_muni}, maps of the Netherlands at province and municipality level of 2013. The used projection is the Rijksdriehoekstelsel projection. \strong{Important:} publication of these maps is only allowed when cited to Statistics Netherlands (CBS) and Kadaster Nederland as source.
#' 
#' @usage data(World)
#' @name World
#' @rdname Shapes
#' @docType data
#' @source \url{http://www.naturalearthdata.com} for \code{World}
#' @source \url{http://www.happyplanetindex.org} for \code{World}
#' @source \url{http://www.cbs.nl} for \code{NLD_prov} and \code{NLD_muni}. 
#' @references Statistics Netherlands (2014), The Hague/Heerlen, Netherlands, \url{http://www.cbs.nl}.
#' @references Kadaster, the Netherlands' Cadastre, Land Registry, and Mapping Agency (2014), Apeldoorn, Netherlands, \url{http://www.kadaster.nl}.
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
#' Spatial data of rivers, of class \code{\link[sf:sf]{sf}}
#' 
#' @usage data(rivers)
#' @name rivers
#' @docType data
#' @source \url{http://www.naturalearthdata.com}
NULL

#' Spatial data of metropolitan areas
#' 
#' Spatial data of metropolitan areas, of class \code{\link[sf:sf]{sf}}. The data includes a population times series from 1950 to (forecasted) 2030. All metro areas with over 1 million inhabitants in 2010 are included.
#' 
#' @usage data(metro)
#' @name metro
#' @docType data
#' @references United Nations, Department of Economic and Social Affairs, Population Division (2014). World Urbanization Prospects: The 2014 Revision, CD-ROM Edition.
#' @source \url{https://esa.un.org/unpd/wup/}
NULL

#' Spatial data of global land cover
#' 
#' Spatial data of global land cover, of class \code{\link[raster:brick]{RasterBrick}}. The data includes a population times series from 1950 to (forecasted) 2030. All metro areas with over 1 million inhabitants in 2010 are included.
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
#' The fundamental, and hence required element is \code{\link{tm_shape}}, which specifies the shape object, and also specifies the projection and bounding box.
#' 
#' The elements that serve as aesthetics layers are
#' 
#' Base layers:
#' \tabular{ll}{
#' \code{\link{tm_polygons}}\tab Create a polygon layer (with borders) \cr
#' \code{\link{tm_symbols}}\tab Create a layer of symbols \cr
#' \code{\link{tm_lines}}\tab Create a layer of lines \cr
#' \code{\link{tm_raster}}\tab Create a raster layer \cr
#' \code{\link{tm_text}}\tab Create a layer of text labels \cr
#' \code{\link{tm_basemap}}\tab Create a layer of basemap tiles \cr
#' \code{\link{tm_tiles}}\tab Create a layer of overlay tiles \cr
#' }
#' 
#' Derived layers:
#' \tabular{ll}{
#' \code{\link{tm_fill}}\tab Create a polygon layer (without borders) \cr
#' \code{\link{tm_borders}}\tab Create polygon borders \cr
#' \code{\link{tm_bubbles}}\tab Create a layer of bubbles \cr
#' \code{\link{tm_squares}}\tab Create a layer of squares \cr
#' \code{\link{tm_dots}}\tab Create a layer of dots \cr
#' \code{\link{tm_markers}}\tab Create a layer of markers \cr
#' \code{\link{tm_iso}}\tab Create a layer of iso/contour lines \cr
#' \code{\link{tm_rgb}}\tab Create a raster layer of an image \cr
#' }
#' 
#' The layers can be stacked by simply adding them with the + symbol. The combination of the elements described above form one group. Multiple groups can be stacked. Each group should start with \code{\link{tm_shape}}.
#' 
#' Attributes layers:
#' \tabular{ll}{
#' \code{\link{tm_grid}}\tab Create grid lines \cr
#' \code{\link{tm_scale_bar}}\tab Create a scale bar \cr
#' \code{\link{tm_compass}}\tab Create a map compass \cr
#' \code{\link{tm_credits}}\tab Create a text for credits \cr
#' \code{\link{tm_logo}}\tab Create a logo \cr
#' \code{\link{tm_xlab} and \link{tm_ylab}}\tab Create axis labels \cr
#' \code{\link{tm_minimap}}\tab Create a minimap (view mode only) \cr
#' }
#' 
#' Layout element:
#' \tabular{ll}{
#' \code{\link{tm_layout}}\tab Adjust the layout (main function)\cr
#' \code{\link{tm_legend}}\tab Adjust the legend \cr
#' \code{\link{tm_view}}\tab Configure the interactive view mode \cr
#' \code{\link{tm_style}}\tab Apply a predefined style \cr
#' \code{\link{tm_format}}\tab Apply a predefined format \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#' 
#' @name tmap-element
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
#' @seealso The examples in each of the element functions
NULL
