x = tm_shape(NLD_muni) +
	tm_polygons(fill = NA, col = NA) +
	tm_layout(frame = FALSE)

x_arrow = x + tm_compass(position = tm_pos_in("center", "center"), size = 4, text.size = 5)
x_radar = x + tm_compass(position = tm_pos_in("center", "center"), size = 4, text.size = 5, type = "radar")
x_rose = x + tm_compass(position = tm_pos_in("center", "center"), size = 4, text.size = 5, type = "rose")
x_4star = x + tm_compass(position = tm_pos_in("center", "center"), size = 4, text.size = 5, type = "4star")
x_8star = x + tm_compass(position = tm_pos_in("center", "center"), size = 4, text.size = 5, type = "8star")

png(filename = "inst/img/compass_arrow.png", bg = "transparent", width = 400, height = 400); x_arrow; dev.off()
png(filename = "inst/img/compass_radar.png", bg = "transparent", width = 400, height = 400); x_radar; dev.off()
png(filename = "inst/img/compass_rose.png", bg = "transparent", width = 400, height = 400); x_rose; dev.off()
png(filename = "inst/img/compass_4star.png", bg = "transparent", width = 400, height = 400); x_4star; dev.off()
png(filename = "inst/img/compass_8star.png", bg = "transparent", width = 400, height = 400); x_8star; dev.off()

