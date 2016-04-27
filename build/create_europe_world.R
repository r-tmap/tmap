library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)
devtools::load_all("pkg")


###########################################################################
## download world shape files from http://www.naturalearthdata.com/features/
###########################################################################

#download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries_lakes.zip", "./shapes/ne_50m_admin_0_countries_lakes.zip")
unzip("./shapes/ne_50m_admin_0_countries_lakes.zip", exdir="./shapes")
world50 <- readOGR("./shapes", "ne_50m_admin_0_countries_lakes")

#download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries_lakes.zip", "./shapes/ne_110m_admin_0_countries_lakes.zip")
unzip("./shapes/ne_110m_admin_0_countries_lakes.zip", exdir="./shapes")
world110 <- readOGR("./shapes", "ne_110m_admin_0_countries_lakes")




## repair shapes
isV50 <- gIsValid(world50)
isV110 <- gIsValid(world110)

if (!isV50) world50 <- gBuffer(world50, byid=TRUE, width=0)
if (!isV110) world110 <- gBuffer(world110, byid=TRUE, width=0)

gIsValid(world50)
gIsValid(world110)



############ process data ########################################################################
keepVars <- c("iso_a3", "name", "name_long", "formal_en", "sovereignt", 
			  "continent", "subregion",
			  "pop_est", "gdp_md_est", "economy", "income_grp"
			  )

eur_sel <- world50$continent=="Europe" | world50$name %in% 
	c("Morocco", "Algeria", "Tunisia", "Israel", "Palestine", "Lebanon",
	  "Syria", "Iraq", "Turkey", "Jordan", "Saudi Arabia", "Iran", "Armenia", 
	  "Azerbaijan", "Georgia", "Kazakhstan", "Cyprus", "Greenland")




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

wd$name[wd$name=="Lao PDR"] <- "Laos"

wd[, 1:7] <- lapply(wd[, 1:7], function(x){
	factor(x)
})

wd$gdp_cap_est <- wd$gdp_md_est / wd$pop_est * 1000000

wd$income_grp <- ordered(wd$income_grp)

############ process areas ########################################################################



## get land areas 

#download.file("http://api.worldbank.org/v2/en/indicator/ag.lnd.totl.k2?downloadformat=csv", "./shapes/ag.lnd.totl.k2_Indicator_en_csv_v2.zip", cacheOK = FALSE, mode="wb")
#unzip("./shapes/ag.lnd.totl.k2_Indicator_en_csv_v2.zip", exdir="./shapes")

WBareas <- read.csv("./shapes/ag.lnd.totl.k2_Indicator_en_csv_v2.csv", skip=2, stringsAsFactors=FALSE)

world50_eIV <- spTransform(world50, CRS("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

wd$area_approx <- approx_areas(world50_eIV, total.area = WBareas$X2014[WBareas$Country.Code=="WLD"])

wd$area_WB <- WBareas$X2014[match(wd$iso_a3, WBareas$Country.Code)]

# nonmatched_iso <- na.omit(setdiff(wd$iso_a3, WBareas$Country.Code))
# nonmatched_iso2 <- na.omit(setdiff(WBareas$Country.Code, wd$iso_a3))
# WBareas$Country.Name[WBareas$Country.Code %in% nonmatched_iso2]
# wd$name[wd$iso_a3 %in% nonmatched_iso]

wd$area <- ifelse(is.na(wd$area_WB), wd$area_approx, wd$area_WB)


#download.file("http://www.happyplanetindex.org/assets/hpi-data.xlsx", "./shapes/hpi-data.xlsx", mode = "wb")
library(readxl)

hpi <- read_excel("./shapes/hpi-data.xlsx", sheet="Complete HPI Dataset", skip=5, 
				  col_types=c("blank", "numeric", "text", "text", rep("numeric", 5), rep("text", 3), rep("blank", 27)))

hpi <- hpi[!is.na(hpi$Country), ]

setdiff(hpi$Country, as.character(wd$name))
setdiff(as.character(wd$name), hpi$Country)

replace_from <- c("Bosnia and Herzegovina", "Central African Republic", "Congo, Dem. Rep. of the",  "Czech Republic",
				  "Dominican Republic", "United States of America")
replace_to <- c("Bosnia and Herz.", "Central African Rep.", "Dem. Rep. Congo",  "Czech Rep.",
				"Dominican Rep.", "United States")


hpi$Country[match(replace_from, hpi$Country)] <- replace_to

setdiff(hpi$Country, as.character(wd$name))

# dpulicated for South Sudan
hpi <-rbind(hpi, hpi[hpi$Country=="Sudan", ])
hpi$Country[nrow(hpi)] <- "S. Sudan"

wd <- cbind(wd, data.frame(life_exp=NA, well_being=NA, HPI=NA))
wd[match(hpi$Country, as.character(wd$name)), c("life_exp", "well_being", "HPI")] <- hpi[, c(4, 5, 8)]


############ pop density ########################################################################

wd$pop_est_dens <- wd$pop_est / wd$area


wd <- wd[, c("iso_a3", "name", "sovereignt", "continent",
			 "subregion", "area", "pop_est", "pop_est_dens",
			 "gdp_md_est", "gdp_cap_est", "economy", 
			 "income_grp", "life_exp", "well_being", "HPI")]

wd$gdp_md_est[wd$name=="Antarctica"] <- NA
wd$gdp_cap_est[wd$name=="Antarctica"] <- NA

############ EU selection ########################################################################

## get data
ed <- wd[eur_sel, ]
ed[, 1:5] <- lapply(ed[, 1:5], function(x){
	factor(as.character(x))
})

ed$part <- ed$subregion

ed$part[which(ed$iso_a3=="TUR")] <- "Southern Europe"

ed$part <- factor(as.character(ed$part), levels=c("Northern Europe", "Western Europe", "Southern Europe", "Eastern Europe"))

C_eu <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
C_sng <- c("AUT", "BEL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "ITA", "LVA", "LTU", "LIE", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE")
C_sng_cand <- c("ROU", "BGR", "CYP", "HRV")

ed$EU_Schengen <- factor(   ifelse(ed$iso_a3 %in% C_eu, 
				  			ifelse(ed$iso_a3 %in% C_sng, "EU Schengen",
				  		    ifelse(ed$iso_a3 %in% C_sng_cand, "EU Schengen cand.", "EU Non-Schengen")), 
				  			ifelse(ed$iso_a3 %in% C_sng, "Schengen Non-EU", NA)),
							levels=c("EU Schengen", "EU Schengen cand.", "EU Non-Schengen", "Schengen Non-EU"))
				  

names(ed)[c(1:4, 16:17, 6:12, 13:15)]

ed <- ed[, c(1:4, 16:17, 6:12, 13:15)]

ed[ed$continent!="Europe" & (ed$name !="Turkey"), 7:16] <- NA


## subset europe and neighboring countries
#proj4string(world50) <- "+proj=longlat +datum=WGS84"
eur1 <- world50[eur_sel,]




## global cropping
CP <- as(extent(-55, 87, 15, 85), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(eur1))
eur2 <- gIntersection(eur1, CP, byid=TRUE)


# Lambert azimuthal equal-area projection
#eur4 <- spTransform(eur2, CRS("+proj=laea +lat_0=35 +lon_0=15 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
eur3 <- spTransform(eur2, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))

gIsValid(eur3, reason = TRUE)


## crop
EU_marg <- .1e6
CP2 <- as(extent(2500000-10*EU_marg, 7350000+3*EU_marg, 1400000-EU_marg, 5750000+7*EU_marg), "SpatialPolygons")
proj4string(CP2) <- CRS(proj4string(eur3))
eur4 <- gIntersection(eur3, CP2, byid=TRUE)



qtm(CP2) + qtm(eur4, fill = "gold")






eur4@bbox[,] <- c(2500000, 1400000, 7350000, 5750000)

gIsValid(eur4, reason = TRUE)


#eur5 <- append_data(eur4, ed[c(1:70, 58), ]) ## needed for Russia-asia
eur5 <- append_data(eur4, ed)
#eur5@data[71, 8:11] <- NA ## needed for Russia-asia


## save Europe
Europe <- eur5

save(Europe, file="./pkg/data/Europe.rda", compress="xz")


pal <- RColorBrewer::brewer.pal(10, "Set3")[c(10, 8, 4, 5)]
tm_shape(Europe) +
	tm_polygons("EU_Schengen", palette=pal, title = "European Countries", showNA=FALSE) +
	tm_format_Europe_wide()


###########################################################################
## process world
###########################################################################


world2 <- world110

## crop to prevent inflated south pole
bbw <- world2@bbox
bbw[2, 1] <- -88 #-83.75
world3 <- crop(world2, bbw)
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
world4_eIV <- spTransform(world4, CRS("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) # Eckert IV
 
#http://en.wikipedia.org/wiki/List_of_map_projections
#https://sites.google.com/site/spatialr/crsprojections
#ftp://ftp.remotesensing.org/proj/OF90-284.pdf
#http://gis.stackexchange.com/questions/29101/create-mercator-map-with-arbitrary-center-orientation
#http://www.progonos.com/furuti/MapProj/Dither/CartHow/HowER_W12/howER_W12.html
#http://resources.arcgis.com/en/help/main/10.1/index.html#/Eckert_IV/003r00000026000000/

#World <- world110_wt

World <- world4_eIV

gIsValid(World)
World <- gBuffer(World, byid=TRUE, width=0)

## set bouding box (leave out Antarctica)
#World@bbox[,] <- c(-14200000, -6750000, 15500000, 9700000)  # for Winkel Tripel

setdiff(World$iso_a3, wd$iso_a3)
setdiff(World$name, wd$name)

World$name <- as.character(World$name)
World$name[World$iso_a3=="CIV"] <- "Cote d'Ivoire"
World$name[World$iso_a3=="LAO"] <- "Laos"
World@data <- wd[match(World$name, wd$name),]

save(World, file="./pkg/data/World.rda", compress="xz")

## projections: see ?proj4string => package rgdal

## test Antarctica
# World_merc <- spTransform(World, CRS("+proj=merc"))
# gIsValid(World_merc)
# qtm(World_merc)
