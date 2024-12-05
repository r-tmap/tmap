data(World, rivers, metro)

# just the map
qtm(World)

# choropleth
qtm(World, fill = "economy", style = "cobalt", crs = "+proj=eck4")

qtm(World, col = NULL) +
qtm(metro, size = "pop2010",
	size.legend = tm_legend("Metropolitan Areas"))

# dot map
\dontrun{
current.mode <- tmap_mode("view")
qtm(metro, bbox = "China")
tmap_mode(current.mode) # restore mode
}

\dontrun{
# without arguments, a plain interactive map is shown (the mode is set to view)
qtm()

# search query for OpenStreetMap nominatim
qtm("Amsterdam")
}
