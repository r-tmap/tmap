data(Europe, land, metro)

land_europe <- crop_shape(land, Europe)

qtm(land_europe, raster="trees", style="natural")

metro_europe <- crop_shape(metro, Europe, polygon = TRUE)

qtm(Europe) +
tm_shape(metro_europe) +
	tm_bubbles("pop2010", col="red", title.size="European cities") +
	tm_legend(frame=TRUE)
