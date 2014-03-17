library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)



#world <- getShape("../shapes/ne_110m_admin_0_countries_lakes.shp")


###########################################################################
## download world shape files from http://www.naturalearthdata.com/features/
###########################################################################

download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries_lakes.zip", "../shapes/ne_50m_admin_0_countries_lakes.zip")
unzip("../shapes/ne_50m_admin_0_countries_lakes.zip", exdir="../shapes")
world50 <- readShapePoly("../shapes/ne_50m_admin_0_countries_lakes.shp")

download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries_lakes.zip", "../shapes/ne_110m_admin_0_countries_lakes.zip")
unzip("../shapes/ne_110m_admin_0_countries_lakes.zip", exdir="../shapes")
world110 <- readShapePoly("../shapes/ne_50m_admin_0_countries_lakes.shp")


keepVars <- c("iso_a3", "name", "name_long", "formal_en", "sovereignt", 
			  "continent", "subregion",
			  "pop_est", "gdp_md_est", "economy", "income_grp"
			  )

###########################################################################
## download continents shape file
###########################################################################

download.file("http://baruch.cuny.edu/geoportal/data/esri/world/continent.zip", "../shapes/cont.zip") 
unzip("../shapes/cont.zip", exdir="../shapes")
cont <- readShapePoly("../shapes/continent.shp")


###########################################################################
## process europe (with neighboring countries)
###########################################################################

## make splitting line for Russia
conteur <- cont[cont$CONTINENT=="Europe",]
plot(conteur)
proj4string(conteur) <- "+proj=longlat +datum=WGS84"

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

## append data
# data_eur <- eur1@data[,keepVars]
# factors <- sapply(data_eur, is.factor)
# data_eur[, 1:7] <- lapply(data_eur[, 1:7], function(x){
# 	as.factor(as.character(x))
# })

# eur4 <- appendData(eur3, data_eur)
# eur4$gdp_cap_est <- eur4$gdp_md_est / eur4$pop_est * 1000000
# 
# ## use better projection
# eur5 <- spTransform(eur4 ,CRS("+proj=utm +zone=33 +north"))
# plot(eur5)


## subset europe and neighboring countries

proj4string(world50) <- "+proj=longlat +datum=WGS84"
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
factors <- sapply(data_eur, is.factor)
data_eur[, 1:7] <- lapply(data_eur[, 1:7], function(x){
	as.factor(as.character(x))
})
data_eur[data_eur$continent!="Europe" & (data_eur$name !="Turkey"), 8:11] <- NA


## remove asian russia from europe
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

# worth checking: simpily shape
# eur10 <- gSimplify(eur7, tol=.01)

## use better projection
eur4 <- spTransform(eur3 ,CRS("+proj=utm +zone=33 +north"))


gIsValid(eur4, reason = TRUE)
#eur8b <- gBuffer(eur8, width=0, byid=TRUE) 

eur4@bbox[,] <- c(-2200000, 3800000, 3400000, 8000000)

eur5 <- appendData(eur4, data_eur[c(1:70, 58), ])
eur5$iso_a3	<- as.character(eur5$iso_a3)
eur5$iso_a3[eur5$iso_a3=="-99"] <- "Kosovo"
eur5$iso_a3[71] <- "RUS (Asia)"
eur5@data[71, 8:11] <- NA


	
eur5$gdp_cap_est <- eur5$gdp_md_est / eur5$pop_est * 1000000



#geo.choropleth(eur5, "gdp_cap_est", style="kmeans") + geo.borders(eur5) + geo.text(eur5, "iso_a3")

europe <- eur5

save(europe, file="./data/europe.rda")

###########################################################################
## process world
###########################################################################
plot(world110)

proj4string(world110) <- "+proj=longlat +datum=WGS84"

world110_wt <- spTransform(world110, CRS("+proj=wintri"))

world110_ll <- spTransform(world110, CRS("+proj=longlat +datum=WGS84"))

world110_r <- spTransform(world110, CRS("+proj=robin"))

plot(world110_wt)
plot(world110_r)

world

world110_vdG <- spTransform(world110, CRS("+proj=vandg "))
#+lon_0=0 +x_0=0 +y_0=0 +R_A +a=6371000 +b=6371000 +units=m +no_defs

plot(world110_vdG)

world <- world110_wt
world@bbox[,] <- c(-12600000, -6500000, 15300000, 9500000) 

#geo.borders(world)
#geo.borders(world) + geo.theme(draw.frame=TRUE, frame.lwd=NA)

save(world, file="./data/world.rda")


## projections: see ?proj4string => package rgdal


####### europe regions map

eur$region <- ""
eur$region[eur$iso_a3 %in% c("CZE", "HUN", "LIE", "POL", "SVK", "BGR", "ROU", "RUS", "UKR", "BLR", "MDA")] <-
	"Eastern Europe"

eur$region[eur$iso_a3 %in% c("DNK", "EST", "FIN", "ISL", "LVA", "LTU", "NOR", "SWE", "IRL", "GBR", "ALA", "FRO", "IMN")] <-
	"Northern Europe"

eur$region[eur$iso_a3 %in% c("NLD", "AUT", "CHE", "DEU", "BEL", "LUX", "FRA", "GGY", "JEY", "MCO")] <-
	"Western Europe"

eur$region[eur$iso_a3 %in% c("PRT", "ESP", "ITA", "SVN", "ALB", "AND", "BIH", "GRC", "HRV", "Kosovo", "SMR", "SRB", "TUR", "VAT", "MKD", "MLT", "MNE")] <-
	"Southern Europe"

eur$region <- factor(eur$region)

geo.borders(eur) +
	geo.choropleth(eur, "region") +
	geo.text(eur, "iso_a3")

devtools::load_all("../../treemap/pkg")
TCdf <- treepalette(eur@data, index= c("region", "iso_a3"))
eur$col <- TCdf$HCL.color[match(eur$iso_a3, TCdf$iso_a3)]

geo.borders(eur) +
	geo.fill(eur, eur$col) +
	geo.text(eur, "iso_a3")

####### world cartograms

names(world110_wt@data)

geo.borders(world110_wt) +
	geo.choropleth(world110_wt, col="region_un") + geo.text(world110_wt, "iso_a3")


