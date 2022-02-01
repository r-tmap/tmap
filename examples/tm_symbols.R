# load Africa country data
data(World, metro)
Africa = World[World$continent == "Africa", ]
metro_A = sf::st_intersection(metro, Africa)

tm_shape(metro_A) +
	tm_symbols()

tm_shape(Africa) + 
	tm_polygons() +
tm_shape(metro_A) +
	tm_symbols(fill = "pop1950", size = "pop2030", size.scale = tm_scale(values.scale = 3))
