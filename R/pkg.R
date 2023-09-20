#' Thematic Map Visualization
#'
#' Thematic maps are geographical maps in which spatial data distributions are visualized. This package offers a flexible, layer-based, and easy to use approach to create thematic maps, such as choropleths and bubble maps. It is based on the grammar of graphics, and resembles the syntax of ggplot2.
#' 
#' This page provides a brief overview of all package functions. See `vignette("tmap_sneek_peek")` for a short introduction with examples.
#'
#' @section Quick plotting method:
#' \tabular{ll}{
#' [qtm()]\tab Plot a thematic map \cr
#' --------------------------- \tab ------------------------------------------- \cr
#' }
#'
#' @section Main plotting method:
#' Shape specification:
#' \tabular{ll}{
#' [tm_shape()]\tab Specify a shape object \cr
#' --------------------------- \tab ------------------------------------------- \cr
#' }
#' 
#' Aesthetics base layers:
#' \tabular{ll}{
#' [tm_polygons()]\tab Create a polygon layer (with borders) \cr
#' [tm_symbols()]\tab Create a layer of symbols \cr
#' [tm_lines()]\tab Create a layer of lines \cr
#' [tm_raster()]\tab Create a raster layer \cr
#' [tm_text()]\tab Create a layer of text labels \cr
#' [tm_basemap()]\tab Create a layer of basemap tiles \cr
#' [tm_tiles()]\tab Create a layer of overlay tiles \cr
#' }
#' 
#' Aesthetics derived layers:
#' \tabular{ll}{
#' [tm_fill()]\tab Create a polygon layer (without borders) \cr
#' [tm_borders()]\tab Create polygon borders \cr
#' [tm_bubbles()]\tab Create a layer of bubbles \cr
#' [tm_squares()]\tab Create a layer of squares \cr
#' [tm_dots()]\tab Create a layer of dots \cr
#' [tm_markers()]\tab Create a layer of markers \cr
#' [tm_iso()]\tab Create a layer of iso/contour lines \cr
#' [tm_rgb()]\tab Create a raster layer of an image \cr
#' --------------------------- \tab ------------------------------------------- \cr
#' }
#' 
#' Faceting (small multiples)
#' \tabular{ll}{
#' [tm_facets()]\tab Define facets \cr
#' --------------------------- \tab ------------------------------------------- \cr
#' }
#' 
#' Attributes:
#' \tabular{ll}{
#' [tm_grid()]\tab Create grid lines \cr
#' [tm_scale_bar()]\tab Create a scale bar \cr
#' [tm_compass()]\tab Create a map compass \cr
#' [tm_credits()]\tab Create a text for credits \cr
#' [tm_logo()]\tab Create a logo \cr
#' [tm_xlab()] and [tm_ylab()]\tab Create axis labels \cr
#' [tm_minimap()]\tab Create a minimap (view mode only) \cr
#' --------------------------- \tab ------------------------------------------- \cr
#' }
#' 
#' Layout element:
#' \tabular{ll}{
#' [tm_layout()]\tab Adjust the layout (main function)\cr
#' [tm_legend()]\tab Adjust the legend \cr
#' [tm_view()]\tab Configure the interactive view mode \cr
#' [tm_style()]\tab Apply a predefined style \cr
#' [tm_format()]\tab Apply a predefined format \cr
#' --------------------------- \tab ------------------------------------------- \cr
#' }
#' 
#' Change options:
#' \tabular{ll}{
#' [tmap_mode()]\tab Set the tmap mode: `"plot"` or `"view"`\cr
#' [ttm()]\tab Toggle between the modes \cr
#' [tmap_options()]\tab Set global tmap options (from [tm_layout()], [tm_view()], and a couple of others) \cr
#' [tmap_style()]\tab Set the default style \cr
#' --------------------------- \tab ------------------------------------------- \cr
#' }
#' 
#' Create icons:
#' \tabular{ll}{
#' [tmap_icons()]\tab Specify icons for markers or proportional symbols \cr
#' --------------------------- \tab ------------------------------------------- \cr
#' }
#' 
#' 
#' @section Output functions: 
#' \tabular{ll}{
#' [print()]\tab Plot in graphics device or view interactively in web browser or RStudio's viewer pane \cr
#' [tmap_last()]\tab Redraw the last map \cr
#' [tmap_leaflet()]\tab Obtain a leaflet widget object \cr
#' [tmap_animation()]\tab Create an animation \cr
#' [tmap_arrange()]\tab Create small multiples of separate maps \cr
#' [tmap_save()]\tab Save thematic maps (either as image or HTML file) \cr
#' --------------------------- \tab ------------------------------------------- \cr
#' }
#' 
#' @section Spatial datasets: 
#' \tabular{ll}{
#' [`World`]\tab World country data ([`sf`][`sf::sf`] object of polygons) \cr
#' [`NLD_prov`]\tab Netherlands province data ([`sf`][`sf::sf`] object of polygons) \cr
#' [`NLD_muni`]\tab Netherlands municipal data ([`sf`][`sf::sf`] object of polygons) \cr
#' [`metro`]\tab Metropolitan areas ([`sf`][`sf::sf`] object of points) \cr
#' [`rivers`]\tab Rivers ([`sf`][`sf::sf`] object of lines) \cr
#' [`land`]\tab Global land cover ([`stars`][stars::st_as_stars()] object)\cr
#' --------------------------- \tab ------------------------------------------- \cr
#' }
#' 
#' @author Martijn Tennekes \email{mtennekes@@gmail.com}
#' @concept GIS
#' @concept thematic maps
#' @concept statistical maps
#' @concept choropleth
#' @concept bubble map
#' @seealso `vignette("tmap_sneek_peek")`, <https://r-tmap.github.io/tmap/>
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \doi{10.18637/jss.v084.i06}
"_PACKAGE"
