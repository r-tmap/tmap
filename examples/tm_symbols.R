########################
## plot symbol shapes
########################

# create grid of 25 points in the Atlantic
atlantic_grid = cbind(expand.grid(x = -51:-47, y = 20:24), id = seq_len(25))
x = sf::st_as_sf(atlantic_grid, coords = c("x", "y"), crs = 4326)

tm_shape(x, bbox = tmaptools::bb(x, ext = 1.2)) +
	tm_symbols(shape = "id",
			   size = 2,
			   lwd = 2,
			   fill = "orange",
			   col = "black",
			   shape.scale = tm_scale_asis()) +
	tm_text("id", ymod = -2)
