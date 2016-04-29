current.mode <- tmap_mode("plot")

data(NLD_muni)
qtm(NLD_muni, theme = "NLD") + tm_scale_bar(position=c("left", "bottom"))

data(Europe)
tm_shape(Europe, unit = "miles", unit.size=1609) + 
	tm_polygons() + 
tm_scale_bar()  + tm_layout(attr.outside = TRUE)

# restore current mode
tmap_mode(current.mode)
