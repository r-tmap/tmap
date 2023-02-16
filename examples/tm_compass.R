tm_shape(NLD_muni) +
	tm_polygons("origin_native") +
	tm_compass(type = "8star", position = tm_pos_in("right", "bottom"))

tm_shape(NLD_muni) +
	tm_polygons("origin_native") +
	tm_compass(type = "8star", position = tm_pos_out("right", "center", pos.h = "left", pos.v = "top"))

tm_shape(NLD_muni) +
	tm_polygons("origin_native", fill.legend = tm_legend(position = tm_pos_in("left", "top"))) +
	tm_compass(type = "8star", position = tm_pos_in("left", "top"))

tm_shape(NLD_muni) +
	tm_polygons("origin_native", fill.legend = tm_legend(position = tm_pos_in("left", "bottom"))) +
	tm_compass(type = "4star", position = tm_pos_in("left", "bottom", align.v = "bottom"), stack = "horizontal") + 
	tm_layout(inner.margins = c(0.4, 0.1, 0.1, 0.1))


### examples V3
current.mode <- tmap_mode("plot")

data(NLD_muni)

tm_shape(NLD_muni) +
	tm_polygons() +
	tm_format("NLD") +
	tm_compass(type = "8star")

#qtm(NLD_muni, theme = "NLD") + tm_compass()
#qtm(NLD_muni, theme = "NLD") + tm_compass(type="radar", position=c("left", "top"), show.labels = 3)

# restore current mode
tmap_mode(current.mode)
