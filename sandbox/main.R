library(devtools)
load_all()
source("sandbox/load_test_data.R")

show_data()

tm_shape(World) +
	tm_polygons(fill = c("blue", "red"))


# size variables reduced to first one
tm_shape(land) +
	tm_raster("trees") +
tm_shape(World, name = "The World", is.main = TRUE, crs = "+proj=robin") +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln")) +
	tm_symbols(fill = c("blue", "red"), size = "life_exp") +
tm_facets_wrap(by = "continent")

# size variables mapped to columns
tm_shape(land) +
	tm_raster("trees") +
tm_shape(World, name = "The World", is.main = TRUE, crs = "+proj=robin") +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln")) +
	tm_symbols(color = c("blue", "red"), size = "life_exp") +
tm_facets_grid(rows = "continent")

tm_shape(World, name = "The World", is.main = TRUE, crs = "+proj=robin") +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln")) +
	tm_facets_grid(rows = "continent")



# size variables mapped to columns (free scales)
tm_shape(land) +
	tm_raster("trees") +
	tm_shape(World, name = "The World", is.main = TRUE, crs = "+proj=robin") +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln"), size.free = TRUE) +
	tm_symbols(color = c("blue", "red"), size = "life_exp", size.free = TRUE) +
	tm_facets_grid(rows = "continent")


# size variables mapped to columns (free scales)
tm_shape(land) +
	tm_raster("trees") +
	tm_shape(World, name = "The World", is.main = TRUE, crs = "+proj=robin") +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln"), size.free = TRUE) +
	tm_symbols(color = c("blue", "red"), size = "life_exp", size.free = TRUE)
#	tm_facets_grid(rows = "continent")



# color and size aes have different free dimensions
tm_shape(World, name = "The World", is.main = TRUE) +
	tm_symbols(col = "HPI", size = "life_exp", size.free = c(TRUE, FALSE, FALSE), col.free = c(FALSE, TRUE, FALSE)) +
	tm_facets_grid(rows = "continent", columns = "alpha_class")



# wrap mvars
tm_shape(World) +
	tm_polygons(fill = c("well_being", "well_being2"))

tm_shape(World) +
	tm_polygons(fill = c("well_being", "well_being2")) +
	tm_symbols(fill = c("red", "purple")) +
	tm_facets_grid(rows = "continent")


tm_shape(World) +
	tm_polygons(fill = c("r1", "r2"),
				fill.scale = tm_scale_intervals()) +
	tm_facets_grid(rows = "continent")



tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.scale = tm_scale_rgb()) +
	tm_facets_grid(rows = "continent")

tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.scale = tm_scale_rgb(),
				fill.free = c(TRUE, TRUE, TRUE)) +
	tm_facets_grid(rows = "continent")

tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.scale = tm_scale_rgb(),
				fill.free = c(TRUE, FALSE, TRUE)) +
	tm_facets_grid(rows = "continent")

tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.scale = tm_scale_rgb(),
				fill.free = c(FALSE, FALSE, TRUE)) +
	tm_facets_grid(rows = "continent")


############# pipeline

# restructure to tmapObject

tm_shape(land) +
	tm_raster("trees") +
	tm_shape(World, name = "The World", is.main = TRUE) +
	#tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln"), size.free = TRUE) +
	tm_polygons(fill = "economy", fill_alpha = .5) +
	tm_symbols(fill = c("blue", "red"), size = "life_exp", size.free = TRUE)
#	tm_facets_grid(rows = "continent")



###### test cases v4
tm1 = tm_shape(World) + tm_polygons("HPI")
tm1

tm1 + tm_facets_wrap(by = "continent", ncols = 3)
tm1 + tm_facets(by = "continent", drop.units = FALSE)
tm1 + tm_facets(by = "continent", drop.units = FALSE, free.coords = FALSE)

tm1 + tm_facets(by = "continent", drop.units = TRUE, free.coords = FALSE)







