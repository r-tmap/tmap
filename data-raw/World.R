#devtools::install_github("ropensci/rnaturalearth")

library(rnaturalearth)
library(sf)
library(lwgeom)
library(wbstats)
library(readxl)
library(dplyr)
library(s2)

## STEP 1 Download main shape
W <- ne_countries(returnclass = "sf")


W$a3 = W$adm0_iso

W$a3[W$a3 == "PR1"] = "PRT" # portugal
W$a3[W$a3 == "SDZ"] = "SDN" # Sudan
W$a3[W$a3 == "PN1"] = "PNG" # Papua New Guinea
W$a3[W$name == "Kosovo"] = "XKX" # Kosovo
W$a3[W$name == "W. Sahara"] = "ESH"
W$a3[W$name == "Palestine"] = "PSE" # Palestine: PSX?
W$a3[W$name == "Falkland Is."] = "FLK"
W$a3[W$name == "Somaliland"] = "SOM"

# 15 and 18
which(!sf::st_is_valid(W))

# fixed 18 with:
W = st_make_valid(W)

#manually fix 15: Sudan
if (FALSE) {
	# inspect
	tmap_mode("view")
	qtm(W[15,1]) + tm_mouse_coordinates()
}

co = st_coordinates(W$geometry[[15]])
# remove 2 spikes
co2 = co[setdiff(2:80, 48), ]
co2 = rbind(co2, co2[1,])
W$geometry[[15]][[1]][[1]] = co2[,1:2]

all(st_is_valid(W))

# also problems with Fiji (1) and Russia (19) because they cross the 180 meridian
split_multipolyon_at_180 = function(mp) {
	co = st_coordinates(mp)

	colist = unname(split.data.frame(co[,1:2], co[,4]))

	colist = lapply(colist, function(col) {
		col[,1][col[,1] < 0] = col[,1][col[,1] < 0] + 360
		col
	})

	sfc1 = do.call(st_sfc, c(lapply(colist, function(c) {
		st_polygon(list(c))
	}), list(crs = 4326)))

	m180 = st_sfc(st_linestring(cbind(180, c(80,0,-80))), crs = 4326)
	sfc1b = st_split(sfc1, m180)

	colist2 = unlist(lapply(sfc1b, function(x) {
		lapply(x, function(x2) {
			col = st_coordinates(x2)[,1:2]

			if (any(col[,1] > 180)) {
				col[,1] = col[,1] - 360
			}

			col
		})
	}), recursive = FALSE)

	colist2 = lapply(colist2, function(x) {
		list(x)
	})

	st_multipolygon(colist2)
}
W1 = split_multipolyon_at_180(W$geometry[[1]])

W$geometry[[1]] = split_multipolyon_at_180(W$geometry[[1]])
W$geometry[[19]] = split_multipolyon_at_180(W$geometry[[19]])


##################
# area

W_detail <- ne_countries(returnclass = "sf", scale = 10)
W_detail = st_make_valid(W_detail)
W_detail = W_detail[match(W$name_long, W_detail$name_long), ]
W$area <- units::set_units(units::set_units(s2_area(W_detail), m^2), km^2)


## STEP 2 Download detailed shape to extract area size (these areas will be used as fallback source. The main source of area sizes is the World Bank (see STEP 5 below)

## STEP 3 Subset variables, check/fix ASCII incompatibility
keepVars <- c("a3", "name", "sovereignt",
			  "continent", "pop_est", "gdp_md", "economy", "income_grp", "area")

setdiff(keepVars, names(W))

W2 <- W[, keepVars]
nonASCII <- lapply(as.data.frame(W2[, c("name", "sovereignt",
										"continent")]), function(x){
											grep("I_WAS_NOT_ASCII", iconv(x, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
										})
W2$name[61] <- "Cote d'Ivoire"

W2$pop_est_dens <- W2$pop_est / as.numeric(W2$area)
#qtm(W2, fill="pop_est_dens", fill.style="kmeans")

## Clean data, and add gdp_cap_est
W2 <- W2 %>%
	mutate(area_diff = NULL,
		   area_from_shp = NULL,
		   area_from_WB = NULL,
		   gdp_cap_est = gdp_md / pop_est * 1e6,
		   gdp_md_est = NULL
	)


##############################################
############ Add other data sources
##############################################

##################
## HPI data
##################

# interactive map: https://happyplanetindex.org/countries/

tmpfile <- tempfile(fileext = ".xlsx")
download.file("https://happyplanetindex.org/HPI_2024_public_dataset.xlsx", destfile = tmpfile)
hpi <- read_excel(tmpfile, sheet="1. All countries", skip=8, n_max = 149)

hpi = hpi[,c(3,7:10)]
names(hpi) = c("a3", "life_exp", "well_being", "footprint", "HPI")

##################
## World Bank -  income inequality
##################

# https://data.worldbank.org/indicator/SI.POV.GINI
# https://blogs.worldbank.org/en/opendata/inside-the-world-bank-s-new-inequality-indicator--the-number-of-

library(WDI)
library(tidyverse)

income_inequality =  WDI(indicator = "SI.POV.GINI") |>
	dplyr::filter(!is.na(SI.POV.GINI)) |>
	select(iso3c, year, SI.POV.GINI) |>
	group_by(iso3c) |>
	dplyr::filter(year == max(year)) |>
	ungroup() |>
	transmute(a3 = iso3c,
			  inequality = SI.POV.GINI)

##################
## gender inequality
##################

library(jsonlite)

# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/gender-inequality-index-from-the-human-development-report.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
metadata <- fromJSON("https://ourworldindata.org/grapher/gender-inequality-index-from-the-human-development-report.metadata.json?v=1&csvType=full&useColumnShortNames=true")

# UNDP, Human Development Report (2024) – with minor processing by Our World in Data

gender_inequality = df |>
	dplyr::filter(Code != "") |>
	group_by(Code) |>
	dplyr::filter(Year == max(Year)) |>
	ungroup() |>
	transmute(a3 = Code,
			  gender = gii)

##################
## press freedom
##################
# https://rsf.org/en/index

# World Press Freedom Index
tmpfile <- tempfile(fileext = ".csv")
download.file("https://rsf.org/sites/default/files/import_classement/2024.csv", destfile = tmpfile)
rsf_source <- read_csv2(tmpfile)

rsf = rsf_source |>
	transmute(a3 = ISO,
			  press = Score)





W3 = W2 |>
	left_join(hpi) |>
	left_join(income_inequality) |>
	left_join(gender_inequality) |>
	left_join(rsf)


W3$press[W3$a3 == "GRL"] = W3$press[W3$a3 == "DNK"] # according to map, Danmark data is used
W3$press[W3$name == "N. Cyprus"] = rsf_source$Score[rsf_source$Country_EN == "Northern Cyprus"] # joint country in this dataset
W3$press[W3$name == "W. Sahara"] = W3$press[W3$name == "Morocco"] # joint country in this dataset

## Tidy up

W4 <- W3 %>%
	rename(iso_a3 = a3) |>
	dplyr::select(iso_a3, name, sovereignt, continent, area, pop_est, pop_est_dens, economy, income_grp, gdp_cap_est,
				  life_exp, well_being, footprint, HPI,
				  inequality, gender, press, geometry) %>%
	mutate(continent = factor(continent),
		   economy = factor(economy),
		   income_grp = factor(income_grp)) |>
	arrange(name)



# checking data join results:

if (FALSE) {

	######## HPI ###############
	tm_shape(W4) +
		tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "rd_yl_gn"))


	tm_shape(W4) +
		tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "classic_orange_white_blue_light", value.na = "#000000"),id= "name", hover=FALSE)

	# compare with interactive map: https://happyplanetindex.org/countries/
	# diff: French Guiana is apart from France

	# missings:
	W4[is.na(W4$HPI), ]

	######## gender ###############

	tm_shape(W4) +
		tm_polygons("gender", fill.scale = tm_scale_intervals(breaks = seq(0,1,by =.1), values = "yl_or_rd", value.na = "#000000"))
	# compare with https://ourworldindata.org/grapher/gender-inequality-index-from-the-human-development-report
	# diff: French Guiana is apart from France


	# missings
	W4[is.na(W4$gender), ]

	######## inequality ###############


	tm_shape(W4) +
		tm_polygons("inequality", fill.scale = tm_scale_intervals(values = "-classic_orange_white_blue_light", value.na = "#000000"),id= "name", hover=FALSE)

	# compare with https://blogs.worldbank.org/en/opendata/inside-the-world-bank-s-new-inequality-indicator--the-number-of-


	# missings
	W4[is.na(W4$inequality), ]

	######## press ###############


	tm_shape(W4) +
		tm_polygons("press")

	# missings
	W4[is.na(W4$press), ]
}

W4$iso_a3[W4$name == "N. Cyprus"] = "CYN" # just to make unique iso a3 codes


attr(W4, "agr")[] = c("identity", "identity", "constant", "constant", "aggregate",
					  "aggregate", "aggregate", "constant", "constant", "constant",
					  "constant", "constant", "constant", "constant", "constant",
					  "constant", "constant")


World = W4


save(World, file="data/World.rda", compress="xz")
