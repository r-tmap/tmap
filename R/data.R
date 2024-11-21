#' World dataset
#'
#' World dataset, class [`sf`][`sf::sf`]
#'
#' @details
#' | variable        | Source 	| description |
#' | ------         	| -----------    	| ----------- |
#' | `iso_a3`		    | NED | ISO 3166-1 alpha-3 three-letter country code (see below) |
#' | `name`		    	| NED | Country name |
#' | `sovereignt`		| NED | Sovereignt country name |
#' | `continent`		| NED | Continent |
#' | `area`				| NED | Area in km2 |
#' | `pop_est`			| NED | Population estimation |
#' | `pop_est_dens`		| NED | Population estimation per km2|
#' | `economy`			| NED | Economy class |
#' | `income_grp`		| NED | Income group |
#' | `gdp_cap_est`		| NED | GDP per capita (estimated) |
#' | `life_exp`			| HPI | Life expectancy. The average number of years an infant born in that country is expected to live  |
#' | `well_being`		| HPI | Well being. Self-reported from 0 (worst) to 10 (best) |
#' | `footprint`		| HPI | Carbon footprint. Per capita greenhouse gas emissions associated with consumption and economic activity |
#' | `HPI`				| HPI | Happy Planet Indicator. An index of human well-being and environmental impact that was introduced by the New Economics Foundation in 2006. Approximate formula: `(life_exp * well_being) / footprint` |
#' | `inequality`		| WB | Income inequality: Gini coefficient (World Bank variable SI.POV.GINI) A value of 0 represents perfect equality, while 100 implies perfect inequality |
#' | `gender`			| UNDP/OWiD | Gender Inequality Index (GII) Composite metric using reproductive health, empowerment and the labour market. A low value indicates low inequality between women and men, and vice-versa |
#' | `press`			| RSF | World Press Freedom Index. Degree of freedom that journalists, news organizations and netizens have |
#'
#' See sources for more in-depth information of the variables.
#'
#' This dataset, created Noveber 2024, is an update from the old version, which has been created around 2016. All variables from the old version are included, but updated. Furthermore, gender ineuqlity and press freedom have been added.
#'
#' ISO country-code: XKX is used for Kosovo (conform European Union and World Bank) (was UNK in the old version. CYN is used for Northern Cyprus (as in the old version).
#'
#' For some variables data were available from multiple years, but availability was different across countries. In those cases, the most recent values were taken.
#' @rdname World
#' @docType data
#' @format NULL
#' @source NED: Natural Earth Data <https://www.naturalearthdata.com/>
#' @source HPI: Happy Planet Index <https://happyplanetindex.org/>
#' @source UNDP: Human Development Report (2024) <https://hdr.undp.org/content/human-development-report-2023-24>
#' @source WB: World Bank <https://data.worldbank.org>
#' @source OWiD: Our World in Data <https://ourworldindata.org>
#' @source RSF: Reporters Without Borders <https://rsf.org/en/index>
"World"



#' Netherlands datasets
#'
#' Datasets of the Netherlands (province and municipality level),
#' class [`sf`][`sf::sf`]
#'
#' The default projections for these objects are
#' Rijksdriehoekstelsel (Netherlands).
#'
#' **Important:** publication of these maps is only allowed when citing
#' Statistics Netherlands (CBS) and Kadaster Nederland as source.
#'
#' @rdname Netherlands
#' @docType data
#' @format NULL
#' @source <https://www.cbs.nl/> for `NLD_prov` and `NLD_muni`.
#' @references Statistics Netherlands (2014), The Hague/Heerlen, Netherlands, <https://www.cbs.nl/>.
#' @references Kadaster, the Netherlands' Cadastre, Land Registry, and Mapping Agency (2014), Apeldoorn, Netherlands, <https://www.kadaster.nl/>.
"NLD_prov"

#' @rdname Netherlands
#' @docType data
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
