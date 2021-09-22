library(devtools)
load_all()
source("sandbox/load_test_data.R")

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
		tm_symbols("pop_class", fill.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class"))


(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = c(TRUE, FALSE, TRUE)) +
		tm_symbols("pop_class", fill.free = c(TRUE, FALSE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class"))

(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = c(T, FALSE, TRUE)) +
		tm_symbols("pop_class", col.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class"))


(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = FALSE) +
		tm_symbols("economy", col.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class"))


(tm  = tm_shape(World) +
		tm_polygons("economy", fill.free = c(F, T, T), col = "pop_class", col.free = c(T, F, F)) +
		tm_symbols("economy", col.free = c(FALSE, TRUE, TRUE)) +
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
		tm_symbols("pop_class", col.free = c(FALSE, TRUE, TRUE)) +
		tm_facets_grid("income_grp", "alpha_class")) + tm_options(asp=0)


(tm  = tm_shape(World) +
		tm_polygons(c("economy", "income_grp")))

(tm  = tm_shape(World) +
		tm_polygons(c("economy", "gdp_cap_est"), fill.scale = list(tm_scale_categorical(values = "rdylbu"), tm_scale_intervals(n = 9, style = "kmeans"))))

(tm  = tm_shape(World) +
		tm_polygons(c("economy", "gdp_cap_est"), fill.scale = list(tm_scale_categorical(values = "set3"), tm_scale_continuous(values = "Hawaii", n = 9))))


tmap_design_mode()
(tm = tm_shape(World) +
		tm_polygons("economy") + tm_options(asp=NA))

(tm = tm_shape(World) +
		tm_polygons("economy") + tm_options(asp=0))

(tm = tm_shape(World) +
		tm_polygons("economy", fill.scale = tm_scale_categorical(n.max = 4)))

(tm = tm_shape(World) +
		tm_polygons("economy", fill.scale = tm_scale_categorical(n.max = 4)))



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
		tm_polygons("economy", fill_alpha = "alpha_class") +
		tm_place_legends_left(0.2))

(tm = tm_shape(World) +
		tm_polygons("economy", 
					fill_alpha = "continent", 
					fill_alpha.legend = tm_legend(position = tm_lp_out("left", "center")),
					fill.free = c(TRUE, FALSE, FALSE),
					fill_alpha.free = c(FALSE, TRUE, FALSE),
					col = "income_grp",
					col.legend = tm_legend(position = tm_lp_in("left", "center"))) +
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


tm_shape(land) +
	tm_raster("cover", col.legend = tm_legend(position = tm_lp_out("right", "center"))) +
	tm_options(meta.margins = c(0,0,0,0.1))

tm_shape(land) +
	tm_raster("cover")

tm_shape(World) + tm_polygons("HPI")

tm_shape(metro) +
	tm_symbols(fill = "pop2020", col = "red")


tm_shape(metro) +
	tm_symbols(fill = "pop2020", lty = "alpha_class", lty.scale = tm_scale_categorical(), fill.scale = tm_scale_intervals(value.neutral = "#FFFFFFFF"))


tm_shape(metro) +
	tm_symbols(fill = "pop2020", size = "pop2010", size.scale = tm_scale_intervals())



tm_shape(metro) +
	tm_symbols(fill = "pop2020", size = "pop2010", size.scale = tm_scale_intervals(values.contrast = c(0.3,0.9)))


tm_shape(metro) +
	tm_symbols(fill = "pop2020", size = "pop2010", size.scale = tm_scale_intervals(values = c(0.3,0.4,0.8,0.9)))

tm_shape(metro) +
	tm_symbols(fill = "pop2020", size = "pop2020", size.scale = tm_scale_intervals(), size.legend = tm_legend_combine("fill"))


tm_shape(metro) +
	tm_symbols(fill = "pop2020", size = "pop2020", size.scale = tm_scale_intervals(), size.legend = tm_legend_combine("fill"), fill.free = TRUE, size.free = TRUE) +
	tm_facets_wrap(by = "alpha_class")

tm_shape(metro) +
	tm_symbols(fill = "pop2020", size = "pop2010", size.scale = tm_scale_intervals(), fill.free = F, size.free = T) +
	tm_facets_wrap(by = "alpha_class")


tm_shape(metro) +
	tm_symbols(size = "pop2010", size.free = T, size.scale = tm_scale_intervals()) +
	tm_facets_wrap(by = "alpha_class")


tm_shape(metro) +
	tm_symbols(fill = "pop2010", fill.free = T, fill.scale = tm_scale_intervals()) +
	tm_facets_wrap(by = "alpha_class")


tm_shape(metro) +
	tm_symbols(fill = "pop2010", fill.free = T, fill.scale = tm_scale_intervals(as.count = TRUE)) +
	tm_facets_wrap(by = "alpha_class")



tm_shape(World) +
	tm_polygons("HPI2", fill.scale = tm_scale_intervals(as.count = T, n = 15))

tm_shape(World) +
	tm_polygons("HPI3", fill.scale = tm_scale_discrete(ticks = 12:50, values = "RdYlBu"))

tm_shape(World) +
	tm_polygons("HPI2", fill.scale = tm_scale_intervals(n=14, midpoint = 30, values = "RdYlBu", as.count = T))

tm_shape(World) +
	tm_polygons("HPI2", fill.scale = tm_scale_intervals(n=14, midpoint = 30, values = "RdYlBu", as.count = T), fill.legend = tm_legend(position = tm_lp_out("right", "center"))) +
	tm_options(meta.auto.margins = 0.1)



tm_shape(World) +
	tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "RdYlBu", breaks = c(-20,-10,10,50)))

tm_shape(World) +
	tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "RdYlBu", breaks = c(-20,-10,10,50)), fill.legend = tm_legend(space = 0.3, space.na = 0.3))

tm_shape(World) +
	tm_polygons(c("HPI", "economy"), fill.legend = list(tm_legend("test"), tm_legend("test2")))

tm_shape(World) +
	tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "RdYlBu", breaks = c(-20,-10,10,1000)))

tm_shape(World) +
	tm_polygons("HPI", fill.scale = tm_scale_continuous(), fill.legend = tm_legend(space = 1, space.na = .5))

tm_shape(World) +
	tm_polygons("HPI", fill.scale = tm_scale_continuous(n = 3), fill.legend = tm_legend(height =10))

tm_shape(World) +
	tm_symbols(size = "pop_est")

tm_shape(World) +
	tm_symbols(size = "pop_est", shape = "pop_class", col = "red")

tm_shape(World) +
	tm_symbols(size = "pop_est") +
tm_shape(metro) +
	tm_symbols(size = "pop2020", size.scale = tm_scale_intervals())

tm_shape(World) +
	tm_polygons(fill = "pop_est_dens", fill.scale = tm_scale_intervals(values = "Reds")) +
	tm_polygons(fill = "economy", fill_alpha = 0.5, fill.scale = tm_scale_categorical(values = "Blues"), lwd = "well_being", lwd.scale = tm_scale_intervals())

tm_shape(World) +
	tm_polygons("HPI")

tm_shape(World) +
	tm_polygons("HPI", fill.scale = tm_scale_log10())

tm_shape(World, crs = "+proj=eck4") +
	tm_cartogram(size = "HPI", fill = "HPI", fill.scale = tm_scale_intervals())

tm_shape(World, crs = "+proj=eck4") +
	tm_cartogram(size = "HPI", fill = "HPI")

tm_shape(World, crs = "+proj=eck4") +
	tm_balloons(size = "pop_est", col = "economy")

tm_shape(prec_nc) +
	tm_polygons("Total_precipitation_surface_1_Hour_Accumulation", fill.legend = tm_legend("Hourly precipitation")) +
	tm_facets(by = "time")

tm_shape(World) +
	tm_polygons(fill = "life_exp") +
	tm_facets_grid("well_being_class", "footprint_class") +
	tm_shape(World, is.main = TRUE) +
	tm_borders()

(tm = tm_shape(metro) +
		tm_symbols(col = "pop2020", size = "pop2020", size.free = TRUE, col.free = FALSE) +
		tm_facets("alpha_class"))

###################################################################################
# TO DO list
###################################################################################


tm_shape(landsat_terra) +
	tm_raster(c("lan_1", "lan_2", "lan_3", "lan_4"), col.free = FALSE) + tm_options(max.raster = 10000)

tm_shape(landsat_stars) +
	tm_raster("landsat.tif") +
	tm_facets("band") + tm_options(max.raster = 10000)


tm_shape(landsat_terra) +
	tm_rgb(tm_mv("lan_4", "lan_3", "lan_2"), col.scale = tm_scale_rgb(maxValue = 31961))


land_terra = rast(as(land, "Raster"))

tm_shape(land) +
	tm_raster("trees")

tm_shape(land_terra) +
	tm_raster("trees")

tm_shape(landsat_stars) +
	tm_raster("landsat.tif") +
	tm_facets_wrap("band")

tm_shape(World) +
	tm_polygons(c("HPI", "income_grp"))


tm_shape(lux) +
	tm_polygons(c("POP", "AREA"))


tm_shape(lux) +
	tm_polygons("POP")


tm_shape(weather) + 
	tm_raster("pr") +
	tm_facets("time")

tm_shape(weather) + 
	tm_raster("tas") +
	tm_facets("time")


tm_shape(weather1) + 
	tm_raster("X", col.free = c(FALSE, TRUE)) +
	tm_facets_grid("time", "attributes")



## units
tm_shape(weather) + 
	tm_raster("pr", col.scale = tm_scale_continuous()) +
	tm_facets("time")


## raster alpha
tm_shape(weather) + 
	tm_raster("pr", col.scale = tm_scale_continuous(), col_alpha = "tas", col_alpha.scale = tm_scale_intervals()) +
	tm_facets("time")

tm_shape(weather) + 
	tm_raster(col = "tas", col_alpha = "pr") +
	tm_facets("time")



## raster alpha
tm_shape(weather) + 
	tm_raster("pr", col.scale = tm_scale_continuous(), col_alpha = "tas", col_alpha.scale = tm_scale_continuous()) +
	tm_facets("time")


tm_shape(landsat_terra) +
	tm_raster(col = "lan_1", col_alpha = "lan_2")

tm_shape(landsat_terra) +
	tm_rgb(tm_mv("lan_4", "lan_3", "lan_2"), col.scale = tm_scale_rgb(maxValue = 31961))




tm_shape(landsat_stars2) +
	tm_raster(col = "X1", col_alpha = "X2")

set.seed(5)
r<- raster(matrix(data=runif(1000, min = -2, max=5), nrow=100, ncol=100), crs = "EPSG:4326")

library(sf)
library(raster)
library(terra)
library(stars)
r<- raster(matrix(data=runif(1000, min = -2, max=5), nrow=100, ncol=100))
st_crs(r)
st_crs(rast(r))



tm_shape(r)+
	tm_raster(style = "cont", legend.reverse = TRUE, midpoint = 0, breaks = c(-2,-1,0,1,2,3,4,5))+
	tm_layout(legend.outside = TRUE, aes.palette = "div", main.title= "Default Pal, Diverge 0")

tm_shape(r)+
	tm_raster("layer", col.scale = tm_scale_continuous(midpoint = 0, ticks = c(-2,-1,0,1,2,3,4,5)), col.legend = tm_legend(reverse = TRUE))
	
tm_shape(r)+
	tm_raster("layer", col.scale = tm_scale_continuous(ticks = c(-2,-1,0,1,2,3,4,5)), col.legend = tm_legend(reverse = TRUE))

tm_shape(r)+
	tm_raster("layer", col.scale = tm_scale_continuous())


data("NLD_prov")

LB = NLD_prov[NLD_prov$name == "Limburg", ]

library(maptiles)
x = maptiles::get_tiles(LB |> st_transform(crs = "EPSG:4326"), "OpenStreetMap", zoom = 9) #CartoDB.PositronOnlyLabels
x

tm_shape(x) +
	tm_rgb(tm_mv("red", "green", "blue"))

plot(x)





tm_shape(ls_stars) +
	tm_raster("layer")
tm_shape(ls_terra) +
	tm_raster("layer")


tm_shape(la_stars) +
	tm_raster("trees")
tm_shape(la_terra) +
	tm_raster("trees")


tm_shape(land) +
	tm_raster("trees")


tm_shape(land_terra) +
	tm_raster("trees")


tm_shape(land_terra) +
	tm_raster("trees")

tm_shape(land) +
	tm_raster("cover_cls")

tm_shape(land_terra) +
	tm_raster("cover_cls")




shp = land
shp = landsat_stars
shp = landsat_stars2



dims = dim(shp)
dims = dims[setdiff(names(dims), names(get_xy_dim(shp)))]
atts = length(shp)
levels(shp[[1]])
sapply(shp, nlevels)




data(land)

tm_shape(land) +
	tm_raster("trees") +
	tm_facets("cover_cls")


## attributes as variables (stars with no bands)
tm_shape(land) +
	tm_raster(c("cover", "cover_cls", "trees", "elevation"))

tm_shape(land) +
	tm_raster("ATTRIBUTES")


## band values as variables (stars with 1 attr)
tm_shape(landsat_stars) +
	tm_raster("band")

tm_shape(landsat_stars) +
	tm_raster("band_1", "band_2", "band_3")

## 

weather






