#' World and Netherlands map
#' 
#' Maps of the world and the Netherlands (province and municipality level),
#' class [`sf`][`sf::sf`]
#' 
#' The default projections for these maps are Eckhart IV (World) and
#' Rijksdriehoekstelsel (Netherlands). See below. The projection can be changed
#' temporarily for plotting purposes by using the projection argument of [tm_shape()].
#' 
#' `World` World map. The default projection for this world map is Eckhart IV
#' since area sizes are preserved, which is a very important property for choropleths.
#' 
#' `NLD_prov` and `NLD_muni`, maps of the Netherlands at province and municipality
#' level of 2013. The used projection is the Rijksdriehoekstelsel projection.
#' 
#' **Important:** publication of these maps is only allowed when citing
#' Statistics Netherlands (CBS) and Kadaster Nederland as source.
#' 
#' @rdname Shapes
#' @format NULL
#' @source <https://www.naturalearthdata.com/> for `World`
#' @source <https://happyplanetindex.org/> for `World`
#' @source <https://www.cbs.nl/> for `NLD_prov` and `NLD_muni`. 
#' @references Statistics Netherlands (2014), The Hague/Heerlen, Netherlands, <https://www.cbs.nl/>.
#' @references Kadaster, the Netherlands' Cadastre, Land Registry, and Mapping Agency (2014), Apeldoorn, Netherlands, <https://www.kadaster.nl/>.
"World"

#' @rdname Shapes
#' @format NULL

"NLD_prov"

#' @rdname Shapes
#' @format NULL
"NLD_muni"

#' Spatial data of rivers
#' 
#' @source <https://www.naturalearthdata.com>
"rivers"

#' Spatial data of metropolitan areas
#' 
#' `metro` includes a population time series from 1950 to (forecasted) 2030. All metro areas with over 1 million inhabitants in 2010 are included.
#' 
#' @references United Nations, Department of Economic and Social Affairs, Population Division (2014). World Urbanization Prospects: The 2014 Revision, CD-ROM Edition.
#' @source <https://population.un.org/wup/>
"metro"

#' Spatial data of global land cover
#' 
#' Spatial data of global land cover, percent tree cover, and elevation of class [`stars`][stars::st_as_stars()]. 
#' Two attributes in this object relates to global land cover.
#' The cover layer classifies the status of land cover of the whole globe into 20 categories, while 
#' the cover_cls layer uses 8 simplified categories.
#' Percent Tree Cover (trees) represents the density of trees on the ground, and the last attribute represents elevation.
#' 
#' **Important:** publication of these maps is only allowed when cited to Tateishi et al. (2014), and when "Geospatial Information Authority of Japan, Chiba University and collaborating organizations." is shown.
#' 
#' @references Production of Global Land Cover Data - GLCNMO2008, Tateishi, R., Thanh Hoan, N., Kobayashi, T., Alsaaideh, B., Tana, G., Xuan Phong, D. (2014), Journal of Geography and Geology, 6 (3).
"land"
