# world choropleth/bubble map of the world
data(World, metro)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

map1 <- tm_shape(metro) +
	tm_bubbles("pop2010", col = "growth", 
			   border.col = "black", border.alpha = .5, 
			   style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
			   palette="-RdYlBu", contrast=1, 
			   title.size="Metro population", 
			   title.col="Growth rate (%)", id="name") + 
	tm_layout(legend.bg.color = "grey90", legend.bg.alpha=.5, legend.frame=TRUE)


lf <- tmap_leaflet(map1)

# show leaflet widget
lf

# add marker
require(leaflet)
lf %>% leaflet::addMarkers(2.2945, 48.8582, popup = "Eiffel tower")

\dontrun{
# alternative
eiffelTower <- geocode_OSM("Eiffel Tower", as.SPDF = TRUE)

map1 + 
tm_shape(eiffelTower) +
	tm_markers()
}
