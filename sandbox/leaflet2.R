library(leaflet)
data(World, metro)
World <- tmaptools::set_projection(World, "longlat")



leaflet(World) %>% 
	addProviderTiles("CartoDB.PositronNoLabels", group="l1") %>% 
	addPolygons(group="l2", color = "blue", fillOpacity = .8) %>% 
	addProviderTiles("CartoDB.PositronOnlyLabels", group="l3") %>% 
	addLayersControl(baseGroups = "l1", overlayGroups = c("l2", "l3"))



tm_basemap("CartoDB.PositronNoLabels") +
tm_shape(World) + tm_polygons("HPI") +
tm_tiles("CartoDB.PositronOnlyLabels")


leaflet() %>% addProviderTiles(providers$MtbMap) %>%
	addProviderTiles(providers$Stamen.TonerLines,
					 options = providerTileOptions(opacity = 0.35)) %>%
	addProviderTiles(providers$Stamen.TonerLabels)


leaflet() %>%
	addProviderTiles("Stamen.Watercolor") %>%
	addProviderTiles("Stamen.TonerHybrid")


states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")

library(spData)
leaflet(us_states) %>% 
	addProviderTiles("CartoDB.PositronNoLabels", options=providerTileOptions(zIndex = 1)) %>% 
	addPolygons(group="polygons", color = "blue", fillOpacity = .8, options=list(zIndex = 2)) %>% 
	addProviderTiles("CartoDB.PositronOnlyLabels", group="labels", options=providerTileOptions(zIndex = 3, pane = 'markerPane')) %>% 
	addLayersControl(overlayGroups = c("polygons", "labels"))


tm_basemap() +
tm_shape(World) +
	tm_polygons("HPI", group = "HPI") +
	tm_tiles("CartoDB.PositronOnlyLabels", group = "Labels") +
	tm_dots(col = "blue", group = "Dots") +
tm_shape(metro) +
	tm_dots(col = "red", group = "Dots")


library(spData)
qtm(us_states)
