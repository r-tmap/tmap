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
world50 <- getShape("../shapes/ne_50m_admin_0_countries_lakes.shp")

download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries_lakes.zip", "../shapes/ne_110m_admin_0_countries_lakes.zip")
unzip("../shapes/ne_110m_admin_0_countries_lakes.zip", exdir="../shapes")
world110 <- getShape("../shapes/ne_50m_admin_0_countries_lakes.shp")


keepVars <- c("iso_a3", "name", "name_long", "formal_en", "sovereignt", 
			  "continent", "subregion",
			  "pop_est", "gdp_md_est", "economy", "income_grp"
			  )

###########################################################################
## download continents shape file
###########################################################################

download.file("http://baruch.cuny.edu/geoportal/data/esri/world/continent.zip", "../shapes/cont.zip") 
unzip("../shapes/cont.zip", exdir="../shapes")
cont <- getShape("../shapes/continent.shp")


###########################################################################
## process europe
###########################################################################

proj4string(world50) <- "+proj=longlat +datum=WGS84"
eur1 <- world50[world50$continent=="Europe" | world50$name=="Turkey", ]
plot(eur1)

## global cropping
CP <- as(extent(-25, 70, 34, 82), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(eur1))
eur2 <- gIntersection(eur1, CP, byid=TRUE)

plot(eur2)

## modify europe continent such that focus is on dividing russia
conteur <- cont[cont$CONTINENT=="Europe",]
plot(conteur)
proj4string(conteur) <- "+proj=longlat +datum=WGS84"

CP <- as(extent(-32, 48, 34, 72), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(conteur))
conteur2 <- gUnion(conteur, CP, byid=TRUE)
plot(conteur2)

CP <- as(extent(40, 64, 67, 70.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(conteur2))
conteur3 <- gUnion(conteur2, CP, byid=TRUE)
geo.borders(conteur3)

CP <- as(extent(10, 75, 72, 85), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(conteur3))
conteur4 <- gDifference(conteur3, CP, byid=TRUE)
plot(conteur4)

CP <- as(extent(48, 75, 70.5, 85), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(conteur4))
conteur5 <- gDifference(conteur4, CP, byid=TRUE)
plot(conteur5)

## remove asian russia from europe
eur3 <- gIntersection(eur2, conteur5, byid=TRUE)
plot(eur3)

## append data
data_eur <- eur1@data[,keepVars]
factors <- sapply(data_eur, is.factor)
data_eur[, 1:7] <- lapply(data_eur[, 1:7], function(x){
	as.factor(as.character(x))
})

eur4 <- appendData(eur3, data_eur)
eur4$gdp_cap_est <- eur4$gdp_md_est / eur4$pop_est * 1000000

## use better projection
eur5 <- spTransform(eur4 ,CRS("+proj=utm +zone=33 +north"))
plot(eur5)


###########################################################################
## process world
###########################################################################
plot(world110)

proj4string(world110) <- "+proj=longlat +datum=WGS84"

world110_wt <- spTransform(world110, CRS("+proj=wintri"))
world110_r <- spTransform(world110, CRS("+proj=robin"))

plot(world110_wt)
plot(world110_r)


####### test ########

geo.borders(eur5) + 
	geo.theme(draw.frame=TRUE) +
	geo.choropleth(eur5, col="gdp_cap_est", style="kmeans", n=10) +
	geo.text(eur5, "iso_a3", cex=.5)

geo.borders(eur5) + 
	geo.theme(draw.frame=TRUE) +
	geo.choropleth(eur5, col="income_grp", palette="RdYlBu", n=3) +
	geo.text(eur5, "iso_a3", cex=.5)


geo.choropleth(eur5, col="iso_a3")


####### europe regions map
str(eur5@data)

eur5$region <- ""
eur5$region[eur5$iso_a3 %in% c("CZE", "HUN", "LIE", "POL", "SVK", "BGR", "ROU", "RUS", "UKR", "BLR", "MDA")] <-
	"Eastern Europe"

eur5$region[eur5$iso_a3 %in% c("DNK", "EST", "FIN", "ISL", "LVA", "LTU", "NOR", "SWE", "IRL", "GBR", "ALA", "FRO", "IMN")] <-
	"Northern Europe"

eur5$region[eur5$iso_a3 %in% c("NLD", "AUT", "CHE", "DEU", "BEL", "LUX", "FRA", "GGY", "JEY", "MCO")] <-
	"Western Europe"

eur5$region[eur5$iso_a3 %in% c("PRT", "ESP", "ITA", "SVN", "ALB", "AND", "BIH", "GRC", "HRV", "-99", "SMR", "SRB", "TUR", "VAT", "MKD", "MLT")] <-
	"Southern Europe"

eur5$region <- factor(eur5$region)

geo.borders(eur5) +
	geo.choropleth(eur5, "region") +
	geo.text(eur5, "iso_a3")


####### world cartograms

names(world110_r@data)

geo.borders(world110_r) +
	geo.choropleth(world110_r, col="region_un")

