#' World dataset
#'
#' World dataset, class [`sf`][`sf::sf`]
#'
#' @details
#' | **Variable**        | **Source**	| **Description** |
#' | ------         	| -----------    	| ----------- |
#' | `iso_a3`		    | NED | ISO 3166-1 alpha-3 three-letter country code (see below) |
#' | `name`		    	| NED | Country name |
#' | `sovereignt`		| NED | Sovereignt country name |
#' | `continent`		| NED | Continent (primary; some countries are transcontinental) |
#' | `area`				| NED | Area in km2 |
#' | `pop_est`			| NED | Population estimation |
#' | `pop_est_dens`		| NED | Population estimation per km2|
#' | `economy`			| NED | Economy class |
#' | `income_grp`		| NED | Income group |
#' | `gdp_cap_est`		| NED | GDP per capita (estimated) |
#' | `life_exp`			| HPI | Life expectancy. The average number of years an infant born in that country is expected to live  |
#' | `well_being`		| HPI | Well being. Self-reported from 0 (worst) to 10 (best) |
#' | `footprint`		| HPI | Carbon footprint. Per capita greendwelling gas emissions associated with consumption and economic activity |
#' | `HPI`				| HPI | Happy Planet Indicator. An index of human well-being and environmental impact that was introduced by the New Economics Foundation in 2006. Approximate formula: `(life_exp * well_being) / footprint` |
#' | `inequality`		| WB | Income inequality: Gini coefficient (World Bank variable SI.POV.GINI) A value of 0 represents perfect equality, while 100 implies perfect inequality |
#' | `gender`			| UNDP/OWiD | Gender Inequality Index (GII) Composite metric using reproductive health, empowerment and the labour market. A low value indicates low inequality between women and men, and vice-versa |
#' | `press`			| RSF | World Press Freedom Index. Degree of freedom that journalists, news organizations and netizens have |
#'
#' See sources for more detailed information about the variables.
#'
#' This dataset, created Noveber 2024, is an update from the old version, which has been created around 2016. All variables from the old version are included, but updated. Furthermore, gender ineuqlity and press freedom have been added.
#'
#' ISO country-code: two countries have user-assigned codes, namely: XKX is used for Kosovo (conform European Union and World Bank) (was UNK in the old version); XNC is used for Northern Cyprus (was CYN in the old version).
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
#' Datasets of the Netherlands for 2022 at three levels: `NLD_prov` (12) provinces, `NLD_muni` (345) municipalities and `NLD_dist` (3340) districts , all class [`sf`][`sf::sf`]
#'
#' @details
#' The data variables for `NLD_muni` and `NLD_dist` are identical:
#'
#' | **Variable**        | **Description** |
#' | ------          | ----------- |
#' | `code`		     | Code. Format is "GMaaaa" (municipality/'**g**e**m**eente') and "WKaaaabb" (district/**w**ij**k**). Here, "aaaa" represents the municipality id number, and "bb" the additional district number.
#' | `name`		     | Name. |
#' | `province`	 | Province name. |
#' | `area`		     | Total area in km2. This area corresponds to the area of the polygons (including inland waters, excluding coastal waters), but is more precise because it is based on non-simplified geometries. |
#' | `urbanity`		 | Level of urbanity. Five classes, determined by the number of addresses per km2 (break values are 2500, 1500, 1000, and 500). |
#' | `population`	 | The total population count at 2022-01-01. |
#' | `pop_0_14`		 | Percentage (rounded) of people between 0 and 15. |
#' | `pop_15_24`	 | Percentage (rounded) of people between 15 and 25.  |
#' | `pop_25_44`	 | Percentage (rounded) of people between 25 and 45.  |
#' | `pop_45_64`	 | Percentage (rounded) of people between 45 and 65.  |
#' | `pop_65plus`	 | Percentage (rounded) of people of 65 and older. |
#' | `dwelling_total`	 | Number of dwellings. |
#' | `dwelling_value`	 | Average dwelling value (Dutch: WOZ-value). |
#' | `dwelling_ownership`| Percentage of dwellings owned by the residents. |
#' | `employment_rate`|  Share of the employed population within the total population from 15 to 75 years old. |
#' | `income_low`	 | Percentage of individuals in private households belonging to the lowest 40% of personal income nationwide. |
#' | `income_high`	 | Percentage of individuals in private households belonging to the highest 20% of personal income nationwide. |
#' | `edu_appl_sci`	 | Percentage of people aged 15 to 75 with a university of applied sciences (Dutch: HBO) or university (Dutch: WO) degree. |
#'
#' See source for detailed information about the variables.
#'
#' This dataset, created Noveber 2024, is an update from the datasets `NLD_muni` and `NLD_prov` used in tmap <= 3, which has been created around 2016. Note that the number of municipalities have been reduced (due to mergings). All old variables are included, except for variables related to ethnicity. Many new variable have been added, and moreover, district (Dutch: wijk) level data have added: `NLD_dist`.
#'
#' The CRS (coordinate reference system) used is the Rijksdriekhoekstelsel New, EPSG 28992. Coordinates have been rounded to meters to reduce file size.
#'
#' @rdname Netherlands
#' @docType data
#' @format NULL
#' @source https://www.cbs.nl/nl-nl/maatwerk/2024/11/kerncijfers-wijken-en-buurten-2022
#' @references Statistics Netherlands (2024), The Hague/Heerlen, Netherlands, <https://www.cbs.nl/>.
"NLD_prov"

#' @rdname Netherlands
#' @docType data
#' @format NULL
"NLD_muni"

#' @rdname Netherlands
#' @docType data
#' @format NULL
"NLD_dist"

#' Spatial data of rivers
#'
#' @source <https://www.naturalearthdata.com>
#' @note In tmap <= 3, this dataset was called `rivers`.
"World_rivers"

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
