library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)
devtools::load_all(".")


###########################################################################
## download world shape files from http://www.naturalearthdata.com/features/
###########################################################################

# download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries_lakes.zip", "../shapes/ne_50m_admin_0_countries_lakes.zip")
# unzip("../shapes/ne_50m_admin_0_countries_lakes.zip", exdir="../shapes")
world50 <- readOGR("../shapes", "ne_50m_admin_0_countries_lakes")

# download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries_lakes.zip", "../shapes/ne_110m_admin_0_countries_lakes.zip")
# unzip("../shapes/ne_110m_admin_0_countries_lakes.zip", exdir="../shapes")
world110 <- readOGR("../shapes", "ne_110m_admin_0_countries_lakes")




## 
isV50 <- gIsValid(world50)
isV110 <- gIsValid(world110)

if (!isV50) world50 <- gBuffer(world50, byid=TRUE, width=0)
if (!isV110) world110 <- gBuffer(world110, byid=TRUE, width=0)

gIsValid(world50)
gIsValid(world110)



############ process data
keepVars <- c("iso_a3", "name", "name_long", "formal_en", "sovereignt", 
			  "continent", "subregion",
			  "pop_est", "gdp_md_est", "economy", "income_grp"
			  )

eur_sel <- world50$continent=="Europe" | world50$name %in% 
	c("Morocco", "Algeria", "Tunesia", "Libya", "Egypt", "Israel", "Palestine", "Lebanon",
	  "Syria", "Iraq", "Kuwait", "Turkey", "Jordan", "Saudi Arabia", "Iran", "Armenia", 
	  "Azerbaijan", "Georgia", "Kazakhstan", "Turkmenistan", "Uzbekistan")



identical(world50@data[, keepVars],  world110@data[, keepVars])

## get world data
wd <- world50@data[, keepVars]


wd <- as.data.frame(lapply(wd, function(x){x[which(x==-99)]<- NA; x}))
factors <- sapply(wd, is.factor)

wd[, 1:7] <- lapply(wd[, 1:7], function(x){
	as.character(x)
})

## remove non-ascii
nonASCII <- lapply(wd[, 1:7], function(x){
	grep("I_WAS_NOT_ASCII", iconv(x, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
})

replASCII <- list(character(0),
				  c("St-Barthelemy", "Cote d'Ivoire", "Curacao", "Sao Tome and Principe"),
				  c("Saint-Barthelemy", "Cote d'Ivoire", "Curacao", "Sao Tome and Principe"),
				  c("Aland Islands", "Saint-Barthelemy", "Curacao", "Faeroe Is.", "Democratic Republic of Sao Tome and Principe"),
				  character(0),
				  character(0),
				  character(0))
names(replASCII) <- names(nonASCII)

for (na in names(nonASCII)) {
	cat("Replace: ", wd[[na]][nonASCII[[na]]], " by, ", replASCII[[na]], "\n")
	wd[[na]][nonASCII[[na]]] <- replASCII[[na]]
}

wd[, 1:7] <- lapply(wd[, 1:7], function(x){
	factor(x)
})

wd$gdp_cap_est <- wd$gdp_md_est / wd$pop_est * 1000000




## get land areas 

#download.file("http://api.worldbank.org/v2/en/indicator/ag.lnd.totl.k2?downloadformat=csv", "../shapes/ag.lnd.totl.k2_Indicator_en_csv_v2.zip", cacheOK = FALSE, mode="wb")
#unzip("../shapes/ag.lnd.totl.k2_Indicator_en_csv_v2.zip", exdir="../shapes")

WBareas <- read.csv("../shapes/ag.lnd.totl.k2_Indicator_en_csv_v2.csv", skip=2, stringsAsFactors=FALSE)

world50_eIV <- spTransform(world50, CRS("+proj=eck4"))

wd$area_approx <- approx_areas(world50_eIV, total.area.km2=WBareas$X2012[WBareas$Country.Code=="WLD"])

wd$area_WB <- WBareas$X2012[match(wd$iso_a3, WBareas$Country.Code)]

# nonmatched_iso <- na.omit(setdiff(wd$iso_a3, WBareas$Country.Code))
# nonmatched_iso2 <- na.omit(setdiff(WBareas$Country.Code, wd$iso_a3))
# WBareas$Country.Name[WBareas$Country.Code %in% nonmatched_iso2]
# wd$name[wd$iso_a3 %in% nonmatched_iso]

wd$area <- ifelse(is.na(wd$area_WB), wd$area_approx, wd$area_WB)

wd$pop_est_dens <- wd$pop_est / wd$area


wd <- wd[, c("iso_a3", "name", "sovereignt", "continent",
			 "subregion", "area", "pop_est", "pop_est_dens",
			 "gdp_md_est", "gdp_cap_est", "economy", 
			 "income_grp")]


## get data
ed <- wd[eur_sel, ]
ed[, 1:5] <- lapply(ed[, 1:5], function(x){
	factor(as.character(x))
})

ed$part <- ed$subregion

ed$part[which(ed$iso_a3=="TUR")] <- "Southern Europe"

ed$part <- factor(as.character(ed$part), levels=c("Northern Europe", "Western Europe", "Southern Europe", "Eastern Europe"))

names(ed)

ed <- ed[, c(1:4, 13, 6:12)]

ed[ed$continent!="Europe" & (ed$name !="Turkey"), 7:12] <- NA


## subset europe and neighboring countries
#proj4string(world50) <- "+proj=longlat +datum=WGS84"
eur1 <- world50[eur_sel,]

## global cropping
CP <- as(extent(-25, 87, 15, 82), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(eur1))
eur2 <- gIntersection(eur1, CP, byid=TRUE)


# Lambert azimuthal equal-area projection
eur4 <- spTransform(eur2, CRS("+proj=laea +lat_0=35 +lon_0=15 +x_0=0 +y_0=0"))

gIsValid(eur4, reason = TRUE)


## set bounding box
eur4@bbox[,] <- c(-3200000, -50000, 3000000, 4100000)


#eur5 <- append_data(eur4, ed[c(1:70, 58), ]) ## needed for Russia-asia
eur5 <- append_data(eur4, ed)
#eur5@data[71, 8:11] <- NA ## needed for Russia-asia


## save Europe
Europe <- eur5

save(Europe, file="./data/Europe.rda", compress="xz")



###########################################################################
## process world
###########################################################################


world2 <- world110

## crop to prevent inflated south pole
bbw <- world2@bbox
bbw[2, 1] <- -86
world3 <- crop_shape(world2, bb=bbw)
gIsValid(world3)


## simplify
#world4 <- gSimplify(world3, tol=.25, topologyPreserve=TRUE)
world4 <- world3
isV <- gIsValid(world4)
if (!isV) {
	world4 <- gBuffer(world4, byid=TRUE, width=0)
	gIsValid(world4)
}


## fix antarctica crop 
id.antarctica <- which(world4$name=="Antarctica")
pid.antarctica <- world4@polygons[[id.antarctica]]@plotOrder[1]

co.antarctica <- world4@polygons[[id.antarctica]]@Polygons[[pid.antarctica]]@coords
nant <- nrow(co.antarctica)
edge_id <- which.max(abs(co.antarctica[-1, 1] - co.antarctica[-nant, 1]))
steps <- 10
co.antarctica <- co.antarctica[c(1:edge_id, rep(edge_id, steps-2), (edge_id+1):nant), ]
co.antarctica[edge_id:(edge_id+steps-1), 1] <- seq(co.antarctica[edge_id,1],
												   co.antarctica[edge_id+steps-1,1],
												   length.out=steps)

world4@polygons[[id.antarctica]]@Polygons[[pid.antarctica]]@coords <- co.antarctica




# ## compromise
# world4_vdG <- spTransform(world4, CRS("+proj=vandg "))
# world4_r <- spTransform(world4, CRS("+proj=robin"))
# world4_wt <- spTransform(world4, CRS("+proj=wintri"))
# 
# ## shapes non-distorted (conformal)
# world4_mc <- spTransform(world4, CRS("+proj=mill"))
# world4_merc <- spTransform(world4, CRS("+proj=merc"))
# 
# 
# ## equidistant
# world4_eqc <- spTransform(world4, CRS("+proj=eqc "))
# world4_giso <- spTransform(world4, CRS("+proj=eqc +lat_ts=30"))
# 
# ## equal area
# world4_peter <- spTransform(world4, CRS("+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45"))
# world4_behr <- spTransform(world4, CRS("+proj=cea +lat_ts=30"))
# world4_hd <- spTransform(world4, CRS("+proj=cea +lat_ts=37.5")) # HoBo-Dyer
world4_eIV <- spTransform(world4, CRS("+proj=eck4")) # Eckert IV
 
#http://en.wikipedia.org/wiki/List_of_map_projections
#https://sites.google.com/site/spatialr/crsprojections
#ftp://ftp.remotesensing.org/proj/OF90-284.pdf
#http://gis.stackexchange.com/questions/29101/create-mercator-map-with-arbitrary-center-orientation
#http://www.progonos.com/furuti/MapProj/Dither/CartHow/HowER_W12/howER_W12.html
#http://resources.arcgis.com/en/help/main/10.1/index.html#/Eckert_IV/003r00000026000000/

#World <- world110_wt

World <- world4_eIV

gIsValid(World)


## set bouding box (leave out Antarctica)
#World@bbox[,] <- c(-14200000, -6750000, 15500000, 9700000)  # for Winkel Tripel

World@data <- wd[match(World$iso_a3, wd$iso_a3),]

save(World, file="./data/World.rda", compress="xz")

## projections: see ?proj4string => package rgdal

## test Antarctica
# World_merc <- spTransform(World, CRS("+proj=merc"))
# gIsValid(World_merc)
# geo(World_merc)
# require(plotGoogleMaps)
# require(RColorBrewer)
# 
# m <- plotGoogleMaps(World, filename="world4.htm", zcol="income_grp", colPalette=brewer.pal(9, "Greens"))
