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

W[W$a3 %in% c("CYP", "CYN"), ]

W$a3 = W$adm0_iso
W$a3[W$a3 == "PR1"] = "PRT" # portugal
W$a3[W$a3 == "SDZ"] = "SDN" # Sudan
W$a3[W$a3 == "PN1"] = "PNG" # Papua New Guinea
W$a3[W$name == "Kosovo"] = "XKX" # Kosovo
W$a3[W$name == "W. Sahara"] = "ESH"
W$a3[W$name == "Palestine"] = "PSE" # Palestine: PSX?

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
			  income_inequality = SI.POV.GINI)

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
			  gender_inequality = gii)

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
			  press_freedom = Score)





W3 = W2 |>
	left_join(hpi) |>
	left_join(income_inequality) |>
	left_join(gender_inequality) |>
	left_join(rsf)

# according to map, Danmark data is used
W3$press_freedom[W3$a3 == "GRL"] = W3$press_freedom[W3$a3 == "DNK"]
W3$press_freedom[W3$name == "N. Cyprus"] = rsf_source$Score[rsf_source$Country_EN == "Northern Cyprus"]

## Tidy up

W4 <- W3 %>%
	rename(iso_a3 = a3) |>
	dplyr::select(iso_a3, name, sovereignt, continent, area, pop_est, pop_est_dens, economy, income_grp, gdp_cap_est,
				  life_exp, well_being, footprint, HPI,
				  income_inequality, gender_inequality, press_freedom, geometry) %>%
	mutate(sovereignt = factor(sovereignt),
		   continent = factor(continent),
		   economy = factor(economy),
		   income_grp = factor(income_grp)) |>
	arrange(name)

World = W4
save(World, file="data/World.rda", compress="xz")


# checking data join results:
tm_shape(World) +
	tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "rd_yl_gn"))
# compare with interactive map: https://happyplanetindex.org/countries/

# missings:
World[is.na(World$HPI), ]


tm_shape(World) +
	tm_polygons("gender_inequality", fill.scale = tm_scale_intervals(breaks = seq(0,1,by =.1), values = "yl_or_rd"))
# compare with https://ourworldindata.org/grapher/gender-inequality-index-from-the-human-development-report

# missings
World[is.na(World$gender_inequality), ]


tm_shape(World) +
	tm_polygons("income_inequality")

# missings
World[is.na(World$income_inequality), ]


tm_shape(World) +
	tm_polygons("press_freedom")

# missings
World[is.na(World$press_freedom), ]

