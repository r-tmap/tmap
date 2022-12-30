

### examples V3
current.mode <- tmap_mode("plot")

data(NLD_muni)

tm_shape(NLD_muni) +
	tm_polygons() +
	tm_format("NLD") +
	tm_compass()

qtm(NLD_muni, theme = "NLD") + tm_compass()
qtm(NLD_muni, theme = "NLD") + tm_compass(type="radar", position=c("left", "top"), show.labels = 3)

# restore current mode
tmap_mode(current.mode)
