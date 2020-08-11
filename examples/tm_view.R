# world choropleth/bubble map of the world
data(World, metro)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

map1 <- tm_shape(metro) +
	tm_bubbles("pop2010", col = "growth", 
		border.col = "black", border.alpha = .5, 
		style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
		palette="-RdYlBu", contrast=1, 
		title.size="Metro population", 
		title.col="Growth rate (%)", id="name", 
		    popup.vars=c("pop2010", "pop2020", "growth")) + 
	tm_legend(outside=TRUE)

current.mode <- tmap_mode("plot")

# plot map
map1

# view map with default view options
tmap_mode("view")
map1

# view map with changed view options
map1 + tm_view(set.view = c(7, 51, 4)) # longitude 7, latitude 51, zoom 4

# interactive world map in original CRS
tm_shape(World) +tm_polygons("HPI") + tm_view(projection = 0) + tm_basemap(NULL)

# restore current mode
tmap_mode(current.mode)
