data(World)

ttm()

install_github("bhaskarvk/leaflet.extras")

library(leaflet.extras)

tmap_mode("view")
tm <- tm_shape(World) + tm_polygons("HPI", id = "name")

tmap_leaflet(tm) %>% 
	addSearchFeatures(
		targetGroups  = 'World',
		options = searchFeaturesOptions(openPopup=FALSE, zoom = 6))

leaflet(st_transform(st_as_sf(World), 4326)) %>% 
	addPolygons(label=~name, popup=~name, group='World') %>% 
	addSearchFeatures(
		targetGroups  = 'World',
		options = searchFeaturesOptions(zoom = 7, openPopup=FALSE))
