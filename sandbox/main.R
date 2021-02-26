library(devtools)
check_man()
load_all()
library(stars)
library(sf)
library(data.table)
library(pryr)
library(profvis)


source("sandbox/test_data.R")
World$gdp_est_mln = World$gdp_cap_est * World$pop_est / 1e6
World$well_being2 = round(World$well_being * rnorm(nrow(World), mean = 1, sd = .2), 1)
set.seed = 1234
World$r1 = round(runif(nrow(World), min = 0, max = 255))
World$g1 = round(runif(nrow(World), min = 0, max = 255))
World$b1 = round(runif(nrow(World), min = 0, max = 255))
World$r2 = round(pmin(pmax(World$r1 + rnorm(nrow(World), mean = 0, sd = 20), 0), 255))
World$g2 = round(pmin(pmax(World$g1 + rnorm(nrow(World), mean = 0, sd = 20), 0), 255))
World$b2 = round(pmin(pmax(World$b1 + rnorm(nrow(World), mean = 0, sd = 20), 0), 255))

World$alpha_class = factor(floor(seq(1, 5, length.out = nrow(World) + 1)[1:nrow(World)]), labels = LETTERS[1:4])
						   

show_data()








############ examples

## 1

# tmel = tm_shape(land) +
# 	tm_raster("trees") +
# tm_shape(World, name = "The World", is.main = TRUE) +
# 	tm_borders() +
# tm_facets(by = "continent") +
# tm_shape(metro) +
# 	tm_symbols(size = "pop2020")


# size variables reduced to first one
tmx =  tm_shape(land) +
	tm_raster("trees") +
tm_shape(World, name = "The World", is.main = TRUE) +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln")) +
	tm_symbols(color = c("blue", "red"), size = "life_exp") +
tm_facets_wrap(by = "continent")

# size variables mapped to columns
tmx =  tm_shape(land) +
	tm_raster("trees") +
tm_shape(World, name = "The World", is.main = TRUE) +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln")) +
	tm_symbols(color = c("blue", "red"), size = "life_exp") +
tm_facets_grid(rows = "continent")

# size variables mapped to columns (free scales)
tmx =  tm_shape(land) +
	tm_raster("trees") +
	tm_shape(World, name = "The World", is.main = TRUE) +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln"), size.free = TRUE) +
	tm_symbols(color = c("blue", "red"), size = "life_exp", size.free = TRUE) +
	tm_facets_grid(rows = "continent")


# size variables mapped to columns (free scales)
tmx =  tm_shape(land) +
	tm_raster("trees") +
	tm_shape(World, name = "The World", is.main = TRUE) +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln"), size.free = TRUE) +
	tm_symbols(color = c("blue", "red"), size = "life_exp", size.free = TRUE)
#	tm_facets_grid(rows = "continent")



# color and size aes have different free dimensions
tmx =  tm_shape(World, name = "The World", is.main = TRUE) +
	tm_symbols(color = "HPI", size = "life_exp", size.free = c(TRUE, FALSE, FALSE), color.free = c(FALSE, TRUE, FALSE)) +
	tm_facets_grid(rows = "continent", columns = "alpha_class")



# wrap mvars
tmx =  tm_shape(World) +
	tm_polygons(fill = c("well_being", "well_being2"))

tmx =  tm_shape(World) +
	tm_polygons(fill = c("well_being", "well_being2")) +
	tm_symbols(color = c("red", "purple")) +
	tm_facets_grid(rows = "continent")


tmx =  tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.setup = tm_aes_color_rgb()) +
	tm_facets_grid(rows = "continent")

tmx =  tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.setup = tm_aes_color_rgb(),
				fill.free = c(TRUE, TRUE, TRUE)) +
	tm_facets_grid(rows = "continent")

tmx =  tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.setup = tm_aes_color_rgb(),
				fill.free = c(TRUE, FALSE, TRUE)) +
	tm_facets_grid(rows = "continent")

tmx =  tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.setup = tm_aes_color_rgb(),
				fill.free = c(FALSE, FALSE, TRUE)) +
	tm_facets_grid(rows = "continent")


############# pipeline

# restructure to tmapObject

tmx1 = step1_rearrange(tmx)
tmx2 = step2_data(tmx1)
tmx3 = step3_trans(tmx2)
step4_plot(tmx3, "Grid")


