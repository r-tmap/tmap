library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)
devtools::load_all(".")


# http://www.cbs.nl/nl-NL/menu/themas/dossiers/nederland-regionaal/publicaties/geografische-data/archief/2014/2013-wijk-en-buurtkaart-art.htm

shp <- get_shape("../shapes/gem_2013_v1.shp")
shp <- shp[shp$WATER=="NEE", ]

NLD_muni <- get_shape("../shapes/gm_2013.shp")
NLD_muni <- set_projection(NLD_muni, current.projection="rd")
NLD_prov <- get_shape("../shapes/pv_2013.shp")
NLD_prov <- set_projection(NLD_prov, current.projection="rd")


nrow(NLD_muni)
nrow(shp)

gm1 <- substr(as.character(shp$GM_CODE), 3,7)
gm2 <- as.character(NLD_muni$GM_2013)

identical(gm1, gm2)

NLD_muni@data <- NLD_muni@data[, c("GM_2013", "GM_NAAM")]
names(NLD_muni) <- c("code", "name")

id <- grep("I_WAS_NOT_ASCII", iconv(levels(NLD_muni$name), "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
levels(NLD_muni$name)[id] <- c("Gaasterlan-Sleat", "Sudwest-Fryslan", "Skarsterlan")

data <- shp@data[, c("AANT_INW", "AANT_MAN", "AANT_VROUW", "P_00_14_JR", "P_15_24_JR", 
					 "P_25_44_JR", "P_45_64_JR", "P_65_EO_JR")]

names(data) <- c("population", "pop_men", "pop_women", 
				 "pop_0_14", "pop_15_24", "pop_25_44",
				 "pop_45_64", "pop_65plus")

NLD_muni <- append_data(NLD_muni, data=data, fixed.order=TRUE)


names(NLD_muni)



head(NLD_muni@data)


NLD_prov <- convert_shape_data(NLD_muni, NLD_prov)

NLD_prov$STATCODE <- NULL
NLD_prov$PV2013 <- NULL
NLD_prov$PV_LABEL <- NULL

names(NLD_prov)[1:2] <- c("code", "name")

## check data
gIsValid(NLD_muni)
gIsValid(NLD_prov)

save(NLD_muni, file="./data/NLD_muni.rda", compress="xz")
save(NLD_prov, file="./data/NLD_prov.rda", compress="xz")



