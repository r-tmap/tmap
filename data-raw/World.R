library(rnaturalearth)
library(sf)
library(lwgeom)
library(wbstats)
library(readxl)
library(dplyr)

## STEP 1 Download main shape
W <- ne_countries(returnclass = "sf")
all(st_is_valid(W))


## STEP 2 Download detailed shape to extract area size (these areas will be used as fallback source. The main source of area sizes is the World Bank (see STEP 5 below)
W_detail <- ne_countries(returnclass = "sf", scale = 10)
W_detail_GallPeters <- st_transform(W_detail, crs = 3410)
W_detail_GallPeters$area <- st_area(W_detail_GallPeters)
W$area_from_shp <- units::set_units(W_detail_GallPeters$area[match(W$name_long, W_detail_GallPeters$name_long)], km^2)


## STEP 3 Subset variables, check/fix ASCII incompatibility
keepVars <- c("iso_a3", "name", "sovereignt", 
			  "continent", "pop_est", "gdp_md_est", "economy", "income_grp", "area_from_shp")
W2 <- W[, keepVars]
nonASCII <- lapply(as.data.frame(W2[, c("name", "sovereignt", 
										"continent")]), function(x){
	grep("I_WAS_NOT_ASCII", iconv(x, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
})
W2$name[32] <- "Cote d'Ivoire"

## STEP 4 Impute missing iso a3 codes with unofficial ones
isoNA <- which(is.na(W2$iso_a3))
countryNames <- W2$name[isoNA]
isoA3 <- c("XTX", "UNK", "SOL")
W2$iso_a3[match(countryNames, W2$name)] <- isoA3


## STEP 5 Add area from World Bank
tmpfile2 <- tempfile(fileext = ".zip")
tmpdir <- tempdir()
download.file("http://api.worldbank.org/v2/en/indicator/AG.LND.TOTL.K2?downloadformat=csv", tmpfile2, cacheOK = FALSE, mode="wb")
unzip(tmpfile2, exdir=tmpdir)
WBareas <- read.csv(file.path(tmpdir, "API_AG.LND.TOTL.K2_DS2_en_csv_v2_9945323.csv"), skip=4, stringsAsFactors=FALSE)
WBareas <- WBareas %>% select(Country.Code, X2017)
WBareas <- WBareas[WBareas$Country.Code %in% W2$iso_a3 & !is.na(WBareas$X2017), ]

W2$area_from_WB <- NA
W2$area_from_WB[match(WBareas$Country.Code, W2$iso_a3)] <- WBareas$X2017
W2$area_diff <- ((W2$area_from_WB - as.numeric(W2$area_from_shp)) / as.numeric(W2$area_from_shp)) * 100

head(W2 %>% select(area_from_WB, area_from_shp, area_diff))

# check
summary(W2$area_diff)
qtm(W2, fill="area_diff", fill.id = "name", fill.popup.vars = c("iso_a3", "area_from_WB", "area_from_shp", "area_diff"),
	fill.breaks = c(-100, -50, -10, 10, 50, 100))

# Impuation strategy:
# - When area sizes from World Band and shape differ less than 10 percent, take area WB, otherwise from shape
# - Why? large differences are often caused by different definitions, e.g. France +/- F. Guyana, Morocco +/- Western Sahara, Serbia +/- Kosovo. Small differences area often caused by small islands, lakes that are not contained in the shape (even scale = 10).

W2$area <- W2$area_from_shp
W2$area[which(abs(W2$area_diff) <= 10)] <- W2$area_from_WB[which(abs(W2$area_diff) <= 10)]


## Impute population and calculate population density
W2$pop_est[W2$name =="W. Sahara"] <- 40000  ## Source: https://en.wikipedia.org/wiki/Free_Zone_(region)
W2$pop_est_dens <- W2$pop_est / as.numeric(W2$area)
qtm(W2, fill="pop_est_dens", fill.style="kmeans")

## Clean data, and add gdp_cap_est
W2 <- W2 %>%
	mutate(area_diff = NULL,
		   area_from_shp = NULL,
		   area_from_WB = NULL,
		   gdp_cap_est = gdp_md_est / pop_est * 1e6,
		   gdp_md_est = NULL
	)


## Add HPI data
tmpfile <- tempfile(fileext = ".xlsx")
download.file("http://happyplanetindex.org/s/hpi-data-2016.xlsx", destfile = tmpfile)
hpi <- read_excel(tmpfile, sheet="Complete HPI data", skip=5, n_max = 140)

setdiff(hpi$Country, as.character(W2$name))
setdiff(as.character(W2$name), hpi$Country)

replace_from <- c("Bosnia and Herzegovina", "Czech Republic", "Dominican Republic", "Republic of Congo", "South Korea", "United States of America")
replace_to <- c("Bosnia and Herz.", "Czech Rep.", "Dominican Rep.", "Dem. Rep. Congo", "Korea", "United States")

hpi$Country[match(replace_from, hpi$Country)] <- replace_to
setdiff(hpi$Country, as.character(W2$name))

hpi <- hpi[hpi$Country %in% as.character(W2$name), ]

NAs <- rep(NA, nrow(W2))
hpidata <- data.frame(life_exp = NAs, well_being = NAs, footprint = NAs, inequality = NAs, HPI = NAs)
hpidata[match(hpi$Country, as.character(W2$name)), ] <- as.data.frame(hpi[, c(4, 5, 7, 8, 11)])

W3 <- bind_cols(W2, hpidata)

tm1 <- qtm(W3, fill="HPI", fill.id = "name", fill.palette = "RdYlGn")
tm2 <- qtm(World, fill="HPI", fill.id = "name", fill.palette = "RdYlGn")

tmap_arrange(tm1, tm2, sync = TRUE)

## Tidy up

W4 <- W3 %>% 
	select(iso_a3, name, sovereignt, continent, area, pop_est, pop_est_dens, economy, income_grp, gdp_cap_est, life_exp, well_being, footprint, inequality, HPI, geometry) %>% 
	mutate(iso_a3 = factor(iso_a3),
		   name = factor(name),
		   sovereignt = factor(sovereignt),
		   continent = factor(continent),
		   economy = factor(economy),
		   income_grp = factor(income_grp))

World <- st_transform(W4, crs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
World <- st_make_valid(World)

all(st_is_valid(World))

save(World, file="data/World.rda", compress="xz")
