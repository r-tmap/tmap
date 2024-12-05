f = system.file("shapes/world.gpkg", package = "spData")
world = read_sf(f)
tanzania = read_sf(f, query = 'SELECT * FROM world WHERE name_long = "Tanzania"')

tmap_design_mode()

tm_shape(tanzania) +
	tm_polygons(lwd = 2) +
	tm_scalebar(c(0, 200, 400), position = tm_pos_out("center", "bottom"))


# problem 1: alignment
# problem 2: scalebar (comp in general?) width: wsu[3] = fW, margins make it smaller. Why are margins not included in fW? 


tmap_design_mode()

tm_shape(tanzania) +
	tm_polygons(lwd = 2) +
	tm_title("gfjgklds hgfjdhkljghkl ;gfdgjklgf", position = tm_pos_out("center", "bottom"))

tm_shape(tanzania) +
	tm_polygons(lwd = 2) +
	tm_compass(position = tm_pos_out("center", "bottom"), frame = TRUE)


tm_shape(tanzania) +
	tm_polygons(lwd = 2) +
	tm_scalebar(c(0, 200, 400), position = tm_pos_out("center", "bottom"), margins = rep(0,4)) +
	tm_compass(position = tm_pos_out("center", "bottom"), frame = TRUE, margins = rep(0,4))


# problem 3:
tm_shape(World) +
	tm_polygons(lwd = 2) +
	tm_compass(position = tm_pos_out("center", "bottom"), frame = TRUE)

tm_shape(World) +
	tm_polygons(lwd = 2, fill = "life_exp") +
	tm_compass(position = tm_pos_out("center", "bottom"), frame = TRUE)



tm_shape(World) +
	tm_polygons(lwd = 2, fill = "life_exp") +
	tm_compass(position = tm_pos_out("center", "bottom"), frame = TRUE)

# process_meta 381
# problem: don't know yet if autoout is placed t/b or l/r. 
# autoout  right bottom
