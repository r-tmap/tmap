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
World$r2 = round(pmin(pmax(World$r1 + rnorm(nrow(World), mean = 0, sd = 50), 0), 255))
World$g2 = round(pmin(pmax(World$g1 + rnorm(nrow(World), mean = 0, sd = 50), 0), 255))
World$b2 = round(pmin(pmax(World$b1 + rnorm(nrow(World), mean = 0, sd = 50), 0), 255))

World$alpha_class = factor(floor(seq(1, 5, length.out = nrow(World) + 1)[1:nrow(World)]), labels = LETTERS[1:4])
						   

show_data()



(tm = tm_shape(World) +
		tm_polygons("economy"))


(tm  = tm_shape(World) +
		tm_polygons("economy") +
		tm_facets("continent"))

(tm  = tm_shape(World) +
		tm_polygons("economy") +
		tm_facets_grid("income_grp", "alpha_class"))

(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = c(TRUE, FALSE, TRUE)) +
		tm_symbols("economy", color.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class"))


(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = FALSE) +
		#tm_symbols("economy", color.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class"))


(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = TRUE))
# todo: calc meta margin height
# step 4#127


tm + tm_options(bg.color = "pink", outer.bg.color = "gold")


(tm  = tm_shape(World) +
		tm_polygons("economy") +
		tm_facets_wrap("continent") + tm_options(asp = 1))


(tm  = tm_shape(World) +
		tm_polygons("economy") +
		tm_facets_wrap("continent") + tm_options(asp = 0))



(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = c(TRUE, FALSE, TRUE)) +
		tm_symbols("economy", color.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class")) + tm_options(asp=0)
