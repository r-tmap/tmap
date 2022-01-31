#' tmap palettes
#' 
#' tmap palettes
#' 
#' @usage data(tmap_pals)
#' @name tmap_pals
#' @docType data
NULL


#' World and Netherlands map
#' 
#' Maps of the world and the Netherlands (province and municipality level), class \code{\link[sf:sf]{sf}}
#' 
#' The default projections for these maps are Eckhart IV (World) and Rijksdriehoekstelsel (Netherlands). See below. The projection can be changed temporarily for plotting purposes by using the projection argument of \code{\link{tm_shape}}.
#' 
#' \code{World} World map. The default projection for this world map is Eckhart IV since area sizes are preserved, which is a very important property for choropleths.
#' 
#' \code{NLD_prov} and \code{NLD_muni}, maps of the Netherlands at province and municipality level of 2013. The used projection is the Rijksdriehoekstelsel projection. \strong{Important:} publication of these maps is only allowed when cited to Statistics Netherlands (CBS) and Kadaster Nederland as source.
#' 
#' @usage data(World)
#' @name World
#' @rdname Shapes
#' @docType data
#' @source \url{https://www.naturalearthdata.com/} for \code{World}
#' @source \url{https://happyplanetindex.org/} for \code{World}
#' @source \url{https://www.cbs.nl/} for \code{NLD_prov} and \code{NLD_muni}. 
#' @references Statistics Netherlands (2014), The Hague/Heerlen, Netherlands, \url{https://www.cbs.nl/}.
#' @references Kadaster, the Netherlands' Cadastre, Land Registry, and Mapping Agency (2014), Apeldoorn, Netherlands, \url{https://www.kadaster.nl/}.
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
#' @source \url{https://www.naturalearthdata.com}
NULL

#' Spatial data of metropolitan areas
#' 
#' Spatial data of metropolitan areas, of class \code{\link[sf:sf]{sf}}. The data includes a population times series from 1950 to (forecasted) 2030. All metro areas with over 1 million inhabitants in 2010 are included.
#' 
#' @usage data(metro)
#' @name metro
#' @docType data
#' @references United Nations, Department of Economic and Social Affairs, Population Division (2014). World Urbanization Prospects: The 2014 Revision, CD-ROM Edition.
#' @source \url{https://population.un.org/wup/}
NULL

#' Spatial data of global land cover
#' 
#' Spatial data of global land cover, percent tree cover, and elevation of class \code{\link[stars:st_as_stars]{stars}}. 
#' Two attributes in this object relates to global land cover.
#' The cover layer classifies the status of land cover of the whole globe into 20 categories, while 
#' the cover_cls layer uses 8 simplified categories.
#' Percent Tree Cover (trees) represents the density of trees on the ground, and the last attribute represents elevation.
#' 
#' \strong{Important:} publication of these maps is only allowed when cited to Tateishi et al. (2014), and when "Geospatial Information Authority of Japan, Chiba University and collaborating organizations." is shown.
#' 
#' @usage data(land)
#' @name land
#' @docType data
#' @references Production of Global Land Cover Data - GLCNMO2008, Tateishi, R., Thanh Hoan, N., Kobayashi, T., Alsaaideh, B., Tana, G., Xuan Phong, D. (2014), Journal of Geography and Geology, 6 (3).
NULL
