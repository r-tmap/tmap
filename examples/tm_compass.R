tm_shape(NLD_muni) +
	tm_polygons("origin_native") +
	tm_format("NLD") +
	tm_compass(type = "8star", frame= TRUE, position = tm_pos_in("left", "top"))

tm_shape(NLD_muni) +
	tm_polygons("origin_native", fill.legend = tm_legend(stack = "horizontal")) +
	tm_format("NLD") +
	tm_compass(type = "8star", frame= TRUE, position = tm_pos_in("left", "top"))

tm_shape(NLD_muni) +
	tm_polygons("origin_native") +
	tm_format("NLD") +
	tm_compass(type = "8star", frame= TRUE, position = tm_pos_in("left", "top"), stack = "horizontal")

### examples V3
current.mode <- tmap_mode("plot")

data(NLD_muni)

tm_shape(NLD_muni) +
	tm_polygons() +
	tm_format("NLD") +
	tm_compass(type = "8star", frame= TRUE)

qtm(NLD_muni, theme = "NLD") + tm_compass()
qtm(NLD_muni, theme = "NLD") + tm_compass(type="radar", position=c("left", "top"), show.labels = 3)

# restore current mode
tmap_mode(current.mode)
