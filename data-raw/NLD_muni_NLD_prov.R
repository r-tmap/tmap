library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)


# http://www.cbs.nl/nl-NL/menu/themas/dossiers/nederland-regionaal/publicaties/geografische-data/archief/2014/2013-wijk-en-buurtkaart-art.htm

shp <- read_shape("./shapes/wijk_buurt_2014/gem_2014.shp")
shp <- shp[shp$WATER=="NEE", ]

NLD_muni <- read_shape("./shapes/gm_2014.shp", current.projection="rd")
NLD_prov <- read_shape("./shapes/pv_2014.shp", current.projection="rd")

# process NLD_prov
NLD_prov$PV_NAAM <- NLD_prov$PV_LABEL
levels(NLD_prov$PV_NAAM) <- substr(levels(NLD_prov$PV_NAAM), 5, nchar(levels(NLD_prov$PV_NAAM)))
NLD_prov$STATCODE <- NULL
NLD_prov$PV2014 <- NULL
NLD_prov$PV_LABEL <- NULL

names(NLD_prov) <- c("code", "name")

# process NLD_muni
gm1 <- substr(as.character(shp$GM_CODE), 3,7)
gm2 <- as.character(NLD_muni$GM_2014)

identical(gm1, gm2)

NLD_muni@data <- NLD_muni@data[, c("GM_2014", "GM_NAAM")]
names(NLD_muni) <- c("code", "name")

id <- grep("I_WAS_NOT_ASCII", iconv(levels(NLD_muni$name), "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
levels(NLD_muni$name)[id] <- "Sudwest-Fryslan" #c("Gaasterlan-Sleat", "Sudwest-Fryslan", "Skarsterlan")

load_all("../spatialToolbox/pkg")
x <- intersection_shapes(NLD_muni, NLD_prov)
NLD_muni$province <- factor(levels(NLD_prov$name)[apply(x, MARGIN = 1, function(a) which(a>.999))], levels=levels(NLD_prov$name))

# check: qtm(NLD_muni, fill="province")
shp$P_NATIVE <- 100 - shp$P_N_W_AL - shp$P_WEST_AL

data <- shp@data[, c("AANT_INW", "AANT_MAN", "AANT_VROUW", "P_00_14_JR", "P_15_24_JR", 
					 "P_25_44_JR", "P_45_64_JR", "P_65_EO_JR", "P_NATIVE", "P_WEST_AL", "P_N_W_AL")]

names(data) <- c("population", "pop_men", "pop_women", 
				 "pop_0_14", "pop_15_24", "pop_25_44",
				 "pop_45_64", "pop_65plus", "origin_native", "origin_west", "origin_non_west")


NLD_muni <- append_data(NLD_muni, data=data, fixed.order=TRUE)

#NLD_prov <- convert_shape_data(NLD_muni, NLD_prov)

library(dplyr)
data2 <- NLD_muni@data %>% 
	group_by(province) %>% 
	summarise(pop_0_14=round(weighted.mean(pop_0_14, population)),
			  pop_15_24=round(weighted.mean(pop_15_24, population)),
			  pop_25_44=round(weighted.mean(pop_25_44, population)),
			  pop_45_64=round(weighted.mean(pop_45_64, population)),
			  origin_west=round(weighted.mean(origin_west, population)),
			  origin_non_west=round(weighted.mean(origin_non_west, population)),
			  population=sum(population),
			  pop_men=sum(pop_men),
			  pop_women=sum(pop_women)) %>% 
	mutate(pop_65plus=100-pop_0_14-pop_15_24-pop_25_44-pop_45_64,
		   origin_native=100-origin_west-origin_non_west) %>%
	select(population, pop_men, pop_women, 
			 pop_0_14, pop_15_24, pop_25_44,
			 pop_45_64, pop_65plus, origin_native, origin_west, origin_non_west)
	

NLD_prov <- append_data(NLD_prov, data2, fixed.order = TRUE)


## check data
gIsValid(NLD_muni)
gIsValid(NLD_prov)

save(NLD_muni, file="./pkg/data/NLD_muni.rda", compress="xz")
save(NLD_prov, file="./pkg/data/NLD_prov.rda", compress="xz")

# convert to sf
library(sf)
data(NLD_muni)
data(NLD_prov)

NLD_muni <- as(NLD_muni, "sf")
NLD_prov <- as(NLD_prov, "sf")

NLD_muni <- st_transform(NLD_muni, crs = 28992)
NLD_prov <- st_transform(NLD_prov, crs = 28992)

save(NLD_muni, file="data/NLD_muni.rda", compress="xz")
save(NLD_prov, file="data/NLD_prov.rda", compress="xz")
