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


data(World, metro)
tmap_mode("view")

tm_basemap() +
tm_shape(World) +
	tm_polygons("HPI", group = "HPI") +
	tm_tiles("CartoDB.PositronOnlyLabels", group = "Labels") +
	tm_dots(col = "blue", group = "Dots") +
tm_shape(metro) +
	tm_dots(col = "red", group = "Dots")


tm_basemap() +
tm_shape(World) +
	tm_polygons("HPI", group = "HPI") +
	tm_tiles("CartoDB.PositronOnlyLabels", group = "Labels") +
	tm_dots(col = "blue", group = "Dots") +
	tm_shape(metro) +
	tm_dots(col = "red", group = "Dots") +
	tm_view(basemaps = "Stamen.Watercolor")

tm_basemap() +
	tm_shape(World) +
	tm_polygons("HPI", group = "HPI") +
	tm_tiles("CartoDB.PositronOnlyLabels", group = "Labels") +
	tm_dots(col = "blue", group = "Dots") +
	tm_shape(metro) +
	tm_dots(col = "red", group = "Dots") +
	tm_view(basemaps = "Stamen.Watercolor", bbox = "Italy")

tm_basemap()

tm_shape(metro) +
	tm_dots() +
	tm_tiles("CartoDB.PositronOnlyLabels") +
	tm_text("name") +
	tm_tiles(leaflet::providers$Stamen.TonerLabels) +
tm_shape(World) +
	tm_tiles("CartoDB.PositronOnlyLabels") +
	tm_polygons() +
	tm_tiles(leaflet::providers$Stamen.TonerLabels) +
	tm_tiles(leaflet::providers$Stamen.TonerLines) +
	tm_dots(col = "purple") +
	tm_tiles(leaflet::providers$Stamen.Terrain) +
	tm_grid() +
	tm_style_albatross() +
	tm_basemap()
	

c("tm_shape", "tm_symbols", "tm_tiles", "tm_text", "tm_tiles", "tm_shape", "tm_tiles", "tm_lines", "tm_tiles", "tm_tiles", "tm_symbols", "tm_tiles", "tm_grid", "tm_layout", "tm_basemap")



tm_basemap() + tm_tiles(providers$CartoDB.DarkMatterOnlyLabels)

# todo: meerdere tm_basemaps tm_tiles per group -> vectoriseren


library(spData)
qtm(us_states)


## test prearrange_element_order

examples <- list(c("tm_shape", "tm_fill", "tm_borders", "tm_symbols", "tm_shape", "tm_lines", "tm_grid", "tm_layout"),
c("tm_shape", "tm_fill", "tm_grid", "tm_borders", "tm_symbols", "tm_shape", "tm_lines", "tm_layout"),
c("tm_shape", "tm_fill", "tm_borders", "tm_tiles", "tm_symbols", "tm_shape", "tm_lines", "tm_grid", "tm_layout"),
c("tm_shape", "tm_fill", "tm_borders", "tm_tiles", "tm_symbols", "tm_shape", "tm_lines", "tm_tiles", "tm_grid", "tm_layout"),
c("tm_shape", "tm_tiles", "tm_fill", "tm_borders", "tm_tiles", "tm_symbols", "tm_shape", "tm_tiles", "tm_lines", "tm_grid", "tm_layout"),
c("tm_shape", "tm_symbols", "tm_tiles", "tm_text", "tm_tiles", "tm_shape", "tm_tiles", "tm_lines", "tm_grid", "tm_layout"),
c("tm_shape", "tm_symbols", "tm_tiles", "tm_text", "tm_tiles", "tm_shape", "tm_tiles", "tm_lines", "tm_grid", "tm_layout", "tm_basemap"),

c("tm_shape", "tm_symbols", "tm_tiles", "tm_tiles", "tm_tiles", "tm_shape", "tm_tiles", "tm_lines", "tm_grid", "tm_layout", "tm_basemap"),

c("tm_shape", "tm_symbols", "tm_tiles", "tm_text", "tm_tiles", "tm_shape", "tm_tiles", "tm_lines", "tm_tiles", "tm_tiles", "tm_symbols", "tm_tiles", "tm_grid", "tm_layout", "tm_basemap"),

c("tm_shape", "tm_fill", "tm_borders", "tm_symbols", "tm_shape", "tm_grid", "tm_layout"),
c("tm_fill", "tm_borders", "tm_symbols", "tm_shape","tm_grid", "tm_layout"),
c("tm_fill", "tm_borders", "tm_symbols", "tm_shape","tm_grid", "tm_layout"),
c("tm_shape", "tm_tiles", "tm_shape", "tm_grid", "tm_layout"))

for (e in examples) {
	cat("####################################\n")
	print(e)
	cat("------------------------------------\n")
	tryCatch({
		res <- prearrange_element_order(e)
		print(res[[1]])
		print(res[[2]])
	}, error = function(e) print(e))
}




