#https://github.com/rstudio/leaflet/pull/298

library(mapview)

data(World, metro)



x <- mapView(World)
mapView(metro)

mapviewColors(metro, zcol="pop2020")

l1 <- tmap_leaflet(tm_shape(World) +
	tm_polygons("HPI"))

l2 <- tmap_leaflet(tm_shape(World) +
				   	tm_polygons("economy"))

x <- latticeView(l1, l2, ncol = 2)



l1 <- leaflet() %>% addProviderTiles("CartoDB.Positron")
l2 <- leaflet() %>% addProviderTiles("OpenStreetMap")
sync(l1, l2)

m1 <- mapview(World)
m2 <- mapview(World)
sync(m1, m2)

devtools::session_info()




tm <- tm_shape(World) +
	tm_polygons("HPI")


l <- tmap_leaflet(tm_shape(World) +
	tm_polygons("HPI"))


World$c1 <- factor(World$continent %in% c("Africa"), labels=c("AE", "other"))


qtm(World[World$continent=="Africa",])

tm_shape(World) +
	tm_polygons("HPI") +
	tm_facets(by = "c1", drop.units = T, free.scales = T, free.coords = T)

ttm()
tm_shape(World) +
	tm_polygons("HPI") +
	tm_facets(by = "continent", drop.units = T)

tm_shape(World) +
	tm_polygons("HPI") +
	tm_facets(by = "c1", drop.units = F) + tm_view(basemaps = F) +
	tm_layout(legend.outside = F)


tm_shape(World) +
	tm_polygons(c("HPI", "economy"))

World1 <- World[World$c1=="AE", ]
World2 <- World[World$c1!="AE", ]

l1 <- tmap_leaflet(tm_shape(World1) +
	tm_polygons(c("HPI")))

l2 <- tmap_leaflet(tm_shape(World2) +
				   	tm_polygons(c("HPI")))




latticeView(lfs[[1]], lfs[[2]], ncol=2)
latticeView(lfs[[2]], lfs[[1]])

latticeView(l1, l2)

identical(gt1,gt2)
r <- mapply(function(g1, g2) {
	identical(g1,g2)	
}, gt1,gt2)

which(!r)


l <- leaflet()
library(htmlwidgets)
library(htmltools)
l2 <- appendContent(l, {
	tags$head(
		tags$style(HTML('.leaflet-container {background: #fff;}'))
	)	
})


tm_shape(World) +
	tm_fill("HPI")













library(sp)
library(raster)

data(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")

## view different aspects of same data set
m1 <- mapview(meuse, zcol = "soil", burst = TRUE)
m2 <- mapview(meuse, zcol = "lead")
m3 <- mapview(meuse, zcol = "landuse", map.types = "Esri.WorldImagery")
m4 <- mapview(meuse, zcol = "dist.m")

sync(m1,m2,m3,m4)

