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
world110 <- readOGR("../shapes", "ne_50m_admin_0_countries_lakes")



keepVars <- c("iso_a3", "name", "name_long", "formal_en", "sovereignt", 
			  "continent", "subregion",
			  "pop_est", "gdp_md_est", "economy", "income_grp"
			  )

###########################################################################
## download continents shape file
###########################################################################

#download.file("http://baruch.cuny.edu/geoportal/data/esri/world/continent.zip", "../shapes/cont.zip") 
#unzip("../shapes/cont.zip", exdir="../shapes")
cont <- readOGR("../shapes", "continent")


###########################################################################
## process europe (with neighboring countries)
###########################################################################

## make splitting line for Russia
conteur <- cont[cont$CONTINENT=="Europe",]
plot(conteur)
#proj4string(conteur) <- "+proj=longlat +datum=WGS84"

CP <- as(extent(-32, 48, 30, 72), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(conteur))
conteur2 <- gUnion(conteur, CP, byid=TRUE)

CP <- as(extent(40, 64, 67, 70.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(conteur2))
conteur3 <- gUnion(conteur2, CP, byid=TRUE)

CP <- as(extent(10, 75, 72, 85), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(conteur3))
conteur4 <- gDifference(conteur3, CP, byid=TRUE)

CP <- as(extent(48, 75, 70.5, 85), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(conteur4))
conteur5 <- gDifference(conteur4, CP, byid=TRUE)



## subset europe and neighboring countries
#proj4string(world50) <- "+proj=longlat +datum=WGS84"
eur1 <- world50[world50$continent=="Europe" | world50$name %in% c("Morocco", "Algeria",
						"Tunesia", "Libya", "Egypt", "Israel", "Palestine", "Lebanon",
						"Syria", "Iraq", "Kuwait", "Turkey", "Jordan", "Saudi Arabia", "Iran", "Armenia", "Azerbaijan", "Georgia",
						"Kazakhstan", "Turkmenistan", "Uzbekistan"),]

## global cropping
CP <- as(extent(-25, 87, 27.5, 82), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(eur1))
eur2 <- gIntersection(eur1, CP, byid=TRUE)


## get data
data_eur <- eur1@data[,keepVars]
data_eur <- as.data.frame(lapply(data_eur, function(x){x[which(x==-99)]<- NA; x}))
factors <- sapply(data_eur, is.factor)
data_eur[, 1:7] <- lapply(data_eur[, 1:7], function(x){
	as.factor(as.character(x))
})
data_eur[data_eur$continent!="Europe" & (data_eur$name !="Turkey"), 8:11] <- NA

## split Russia
russiaEur <- gIntersection(eur2, conteur5, byid=TRUE)

russiaEur <- russiaEur[58, ]
russiaAll <- eur2[58, ]

russiaAsia <- gDifference(russiaAll, russiaEur)

## keep top 3 areas of Asia's Russia
maxids <- tail(order(sapply(russiaAsia@polygons[[1]]@Polygons, function(x) x@area)), 3)

russiaAsia@polygons[[1]]@Polygons <- russiaAsia@polygons[[1]]@Polygons[maxids]
russiaAsia@polygons[[1]]@area <- sum(sapply(russiaAsia@polygons[[1]]@Polygons, function(x)x@area))
russiaAsia@polygons[[1]]@plotOrder <- rev(as.integer(maxids))
slot(russiaAsia, "polygons") <- lapply(slot(russiaAsia, "polygons"),
								   checkPolygonsHoles) 

## append Asia's Russia to Europe
russiaPolygons <- c(russiaEur@polygons, russiaAsia@polygons)
russiaID <- which.max(sapply(eur2@polygons, function(x)x@area))

eur3 <- eur2
eur3@polygons <- c(eur3@polygons[1:(russiaID-1)], russiaPolygons[1], eur3@polygons[(russiaID+1):length(eur3@polygons)], russiaPolygons[2])

eur3@plotOrder <- c(as.integer(length(eur3@polygons)), eur3@plotOrder)
slot(eur3, "polygons") <- lapply(slot(eur3, "polygons"),
									   checkPolygonsHoles) 

## use better projection for Europe
eur4 <- spTransform(eur3 ,CRS("+proj=utm +zone=33 +north"))
gIsValid(eur4, reason = TRUE)


## set bounding box
eur4@bbox[,] <- c(-2200000, 3800000, 3400000, 8000000)

## append Data


eur5 <- appendData(eur4, data_eur[c(1:70, 58), ])
#eur5$iso_a3	<- as.character(eur5$iso_a3)
#eur5$iso_a3[eur5$iso_a3=="-99"] <- "Kosovo"
#eur5$iso_a3[71] <- "RUS (Asia)"
eur5@data[71, 8:11] <- NA
eur5$gdp_cap_est <- eur5$gdp_md_est / eur5$pop_est * 1000000


## save Europe
Europe <- eur5

save(Europe, file="./data/Europe.rda")

###########################################################################
## process world
###########################################################################
plot(world110)

## Set world porjection to Winkel Tripel

## compromise
world110_vdG <- spTransform(world110, CRS("+proj=vandg "))
world110_r <- spTransform(world110, CRS("+proj=robin"))
world110_wt <- spTransform(world110, CRS("+proj=wintri"))

## shapes non-distorted (conformal)
world110_mc <- spTransform(world110, CRS("+proj=mill"))
world110_merc <- spTransform(world110, CRS("+proj=merc"))


## equidistant
world110_eqc <- spTransform(world110, CRS("+proj=eqc "))
world110_giso <- spTransform(world110, CRS("+proj=eqc +lat_ts=30"))

## equal area
world110_peter <- spTransform(world110, CRS("+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45"))
world110_behr <- spTransform(world110, CRS("+proj=cea +lat_ts=30"))
world110_hd <- spTransform(world110, CRS("+proj=cea +lat_ts=37.5")) # HoBo-Dyer
world110_eIV <- spTransform(world110, CRS("+proj=eck4")) # Eckert IV
 
#http://en.wikipedia.org/wiki/List_of_map_projections
#https://sites.google.com/site/spatialr/crsprojections
#ftp://ftp.remotesensing.org/proj/OF90-284.pdf
#http://gis.stackexchange.com/questions/29101/create-mercator-map-with-arbitrary-center-orientation
#http://www.progonos.com/furuti/MapProj/Dither/CartHow/HowER_W12/howER_W12.html
#http://resources.arcgis.com/en/help/main/10.1/index.html#/Eckert_IV/003r00000026000000/

World <- world110_wt
World <- world110_eIV

## set bouding box (leave out Antarctica)
#World@bbox[,] <- c(-14200000, -6750000, 15500000, 9700000)  # for Winkel Tripel

World@data <- World@data[, keepVars]
summary(World@data)

World@data <- as.data.frame(lapply(World@data, function(x){x[which(x==-99)]<- NA; x}))
factors <- sapply(World@data, is.factor)
World@data[, 1:7] <- lapply(World@data[, 1:7], function(x){
	as.factor(as.character(x))
})
World$gdp_cap_est <- World$gdp_md_est / World$pop_est * 1000000

#plot(World)


save(World, file="./data/World.rda")


## projections: see ?proj4string => package rgdal



