shp <- getShape("gm", 2010)

shp <- rd2wgs84(shp)

library(plotKML)
kml(shp, folder.name=".", file.name="test3.kml", colour="blue")


#shp <- readOGR(dsn="./maps/2010", layer="gm_2010")
#shp <- rd2wgs84(shp)
#writeOGR(shp, "test.kml", shp@data, driver="KML")
