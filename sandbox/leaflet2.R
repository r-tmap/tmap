library(leaflet)
data(World, metro)
World <- tmaptools::set_projection(World, "longlat")



leaflet(World) %>% 
	#addProviderTiles("CartoDB.PositronNoLabels", group="l1") %>% 
	addPolygons(group="l1") %>% 
	addProviderTiles("CartoDB.PositronOnlyLabels", group="l2") %>% 
	addProviderTiles("MtbMap", group="l3") %>% 
	addLayersControl(baseGroups = c("l1", "l3"), overlayGroups = "l2")



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


tm_basemap() +
tm_shape(World) +
	tm_polygons("HPI") +
	tm_tiles("CartoDB.PositronOnlyLabels") +
	tm_dots(col = "blue") +
tm_shape(metro) +
	tm_dots(col = "red")