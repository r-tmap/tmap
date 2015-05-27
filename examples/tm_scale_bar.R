data(NLD_muni)
qtm(NLD_muni) + tm_scale_bar(position=c("left", "bottom"))

data(Europe)
tm_shape(Europe, unit = "miles", unit.size=1609) + tm_polygons() + tm_scale_bar()
