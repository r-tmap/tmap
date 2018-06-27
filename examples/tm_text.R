current.mode <- tmap_mode("plot")

data(World, metro)

tm_shape(World) +
    tm_text("name", size="AREA")

tm_shape(World) +
	tm_text("name", size="pop_est", col="continent", palette="Dark2",
			title.size = "Population", title.col="Continent") +
	tm_legend(outside = TRUE)

tmap_mode("view")

require(tmaptools)
metro_aus <- crop_shape(metro, bb("Australia"))

tm_shape(metro_aus) +
	tm_dots() +
	tm_text("name")

# alternative
\dontrun{
tm_shape(metro_aus) +
	tm_markers(text = "name")
}

# restore current mode
tmap_mode(current.mode)
