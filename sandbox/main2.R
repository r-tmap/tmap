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
World$pop_class = cut(World$pop_est, breaks = c(0, 10, 100, 1000, Inf) * 1e6, labels = c("Small", "Medium", "Large", "Extra Large"))					   

metro$alpha_class = factor(floor(seq(1, 5, length.out = nrow(metro) + 1)[1:nrow(metro)]), labels = LETTERS[1:4])



show_data()



(tm = tm_shape(World) +
		tm_polygons("economy"))


(tm = tm_shape(World) +
		tm_polygons("economy", fill.scale = tm_scale_categorical(values = tmap_pals$rcartocolor.carto$ArmyRose)))




(tm  = tm_shape(World) +
		tm_polygons("economy") +
		tm_symbols("economy"))
		


(tm  = tm_shape(World) +
		tm_polygons("economy") +
		tm_facets("continent"))

(tm  = tm_shape(World) +
		tm_polygons("economy") +
		tm_facets_grid("income_grp", "alpha_class") + tm_options(asp = NA)) 

(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = c(TRUE, FALSE, TRUE)) +
		tm_symbols("pop_class", color.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class"))


(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = c(TRUE, FALSE, TRUE)) +
		tm_symbols("pop_class", color.free = c(TRUE, FALSE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class"))

(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = c(T, FALSE, TRUE)) +
		#tm_symbols("pop_class", col.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class"))


(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = FALSE) +
		#tm_symbols("economy", color.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class"))


(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = c(F, T, T), col = "pop_class", col.free = c(T, F, F)) +
		#tm_symbols("economy", color.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class"))



(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = TRUE))
# todo: calc meta margin height
# step 4#127



tm + tm_options(asp = 0)


tm + tm_options(bg.color = "pink", outer.bg.color = "gold")


(tm  = tm_shape(World) +
		tm_polygons("economy") +
		tm_facets_wrap("continent") + tm_options(asp = 1))


(tm  = tm_shape(World) +
		tm_polygons("economy") +
		tm_facets_wrap("continent") + tm_options(asp = 0))



(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = c(TRUE, FALSE, TRUE)) +
		#tm_symbols("pop_class", color.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class")) + tm_options(asp=0)


(tm  = tm_shape(World) +
		tm_polygons(c("economy", "income_grp")))

(tm  = tm_shape(World) +
		tm_polygons(c("economy", "gdp_cap_est"), fill.scale = list(tm_scale_categorical(values = "rdylbu"), tm_scale_intervals(n = 9, style = "kmeans"))))

# (tm  = tm_shape(World) +
# 		tm_polygons(c("economy", "gdp_cap_est"), fill.setup = list(tm_aes_color(palette = "set3"), tm_aes_color(palette = "Hawaii", n = 9, style = "cont"))))

# (tm  = tm_shape(World) +
# 		tm_polygons("gdp_cap_est", fill.setup = list(tm_aes_color(palette = "set3"), tm_aes_color(palette = "Hawaii", n = 9, style = "cont"))))

# (tm  = tm_shape(World) +
# 		tm_polygons("gdp_cap_est", fill.setup = list(tm_aes_color(palette = "brewer.blues", n = 9, style = "cont"))))

tmap_design_mode()
(tm = tm_shape(World) +
		tm_polygons("economy") + tm_options(asp=NA))

(tm = tm_shape(World) +
		tm_polygons("economy") + tm_options(asp=0))

(tm = tm_shape(World) +
		tm_polygons("economy", fill.scale = tm_scale_categorical(n.max = 4)))

(tm = tm_shape(World) +
		tm_polygons("economy", fill.scale = tm_scale_categorical(n.max = 4)))



## step2 164
(tm = tm_shape(World) +
		tm_polygons("economy") + tm_options(asp=0))

(tm = tm_shape(World) +
		tm_polygons("economy", col = "continent", fill_alpha = "HPI") + tm_options(asp=0))


(tm = tm_shape(metro) +
		tm_symbols(col = "pop2020", size = "pop2020", size.free = TRUE, col.free = FALSE) +
		tm_facets("alpha_class")
		)

(tm = tm_shape(metro) +
		tm_symbols(fill = "pop2020", size = "pop2020", shape = "alpha_class", size.free = TRUE, col.free = FALSE)
)


(tm = tm_shape(World) +
		tm_polygons("economy", fill_alpha = "HPI"))

(tm = tm_shape(World) +
		tm_polygons("economy", fill_alpha = "alpha_class", fill_alpha.legend = tm_legend(position = tm_lp_out("left", "center"))))


(tm = tm_shape(World) +
		tm_polygons("economy", fill_alpha = "alpha_class", fill_alpha.legend = tm_legend(position = tm_lp_out("right", "center"))))


(tm = tm_shape(World) +
		tm_polygons("economy", 
					fill_alpha = "continent", 
					fill_alpha.legend = tm_legend(position = tm_lp_out("left", "center")),
					fill.free = c(TRUE, FALSE, FALSE),
					fill_alpha.free = c(FALSE, TRUE, FALSE),
					col = "income_grp",
					col.legend = tm_legend(position = tm_lp_inset("left", "center"))) +
		tm_facets_grid(rows = "alpha_class", columns = "pop_class"))


(tm = tm_shape(World) +
		tm_polygons("economy", col = "income_grp", col.legend = tm_legend(position = tm_lp_inset("left", "top"))) + tm_options(legend.frame = "red"))

(tm = tm_shape(World) +
		tm_polygons("economy", col = "income_grp", col.legend = tm_legend(position = tm_lp_inset("left", "top"))) + tm_options(legend.frame = "red"))



(tm  = tm_shape(World) +
		tm_polygons(c("economy", "gdp_cap_est")))


(tm = tm_shape(World) +
		tm_polygons("economy", fill.free = TRUE) +
		tm_facets(by = "pop_class", ncol = 2))


(tm = tm_shape(World) +
	tm_polygons("economy", fill.legend = tm_legend(position = tm_lp_inset("left", "top"))) +
	tm_facets(by = "pop_class", ncol = 2))


(tm = tm_shape(World) +
		tm_polygons("economy", 
					fill.free = TRUE, 
					fill.scale = tm_scale_categorical(levels.drop = TRUE), 
					fill.legend = tm_legend(position = tm_lp_inset("left", "top"))) +
		tm_facets(by = "pop_class", ncol = 2))


tm_shape(World) +
	tm_borders()


# to do's

# improve error message
(tm  = tm_shape(World) +
	tm_polygons(c("economy", "ffggfds")))


# tm_scale_discrete
# tm_scale_continuous


# symbols, lines, raster


tm_shape(land) +
	tm_raster("cover", col.legend = tm_legend(position = tm_lp_out("right", "center"))) +
	tm_options(meta.margins = c(0,0,0,0.1))

tm_shape(land) +
	tm_raster("cover")


# timings

system.time({
	tm  = tm_shape(World) +
	 	tm_polygons(c("economy", "income_grp"))
	print(tm)
})


system.time({
	(tm_shape(land) +
	 	tm_raster("cover")) |> print()
})



library(profvis)

profvis({
	(tm_shape(land) +
	 	tm_raster("cover")) |> print()
})


# tmapGrid 417
# n levels 6 or 8?
tm_shape(World) + tm_polygons("HPI")

tm_shape(metro) +
	tm_symbols(fill = "pop2020", col = "red")


tm_shape(metro) +
	tm_symbols(fill = "pop2020", lty = "pop2020", lty.scale = tm_scale_intervals(), fill.scale = tm_scale_intervals(value.neutral = "blue"))
