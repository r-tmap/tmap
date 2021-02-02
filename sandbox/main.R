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



############ examples

## 1

# tmel = tm_shape(land) +
# 	tm_raster("trees") +
# tm_shape(World, name = "The World", is.main = TRUE) +
# 	tm_borders() +
# tm_facets(by = "continent") +
# tm_shape(metro) +
# 	tm_symbols(size = "pop2020")


tmel = tm_shape(land) +
	tm_raster("trees") +
tm_shape(World, name = "The World", is.main = TRUE) +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln")) +
	tm_symbols(color = c("blue", "red"), size = "pop_est") +
	tm_facets_wrap(by = "continent")
	

# wrap mvars
tmel = tm_shape(World) +
	tm_polygons(fill = c("well_being", "well_being2"))

tmel = tm_shape(World) +
	tm_polygons(fill = c("well_being", "well_being2")) +
	tm_facets_grid(rows = "continent")


tmel = tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.setup = tm_aes_color_rgb()) +
	tm_facets_grid(rows = "continent")

tmel = tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.setup = tm_aes_color_rgb(),
				fill.free = c(TRUE, TRUE, TRUE)) +
	tm_facets_grid(rows = "continent")

tmel = tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.setup = tm_aes_color_rgb(),
				fill.free = c(TRUE, FALSE, TRUE)) +
	tm_facets_grid(rows = "continent")

tmel = tm_shape(World) +
	tm_polygons(fill = c(MV("r1", "g1", "b1"), MV("r2", "g2", "b2")),
				fill.setup = tm_aes_color_rgb(),
				fill.free = c(FALSE, FALSE, TRUE)) +
	tm_facets_grid(rows = "continent")


############# pipeline

# restructure to tmapObject
tmo = tmapObject(tmel)

str(tmo,3)


# prepare data for transformation and mapping

updateData(tmo)

tmg = tmo[[1]]

tml = tmg$tmls[[1]]

dt = tmg$tms$dt

byvar = ""


a = "continent"
x = "d"
dt[, (x) := get(a)]

dt[, x := sum(l1__data__size__1__1), by = .(by1__, by2__, along__)]



tmapAesColorDiscrete


dt[, .(x = sum(dv__l1__color__1__1)), by = list(tmapID__, by1__, by2__, along__)]



tmo[[1]]$tms$dt
tmo[[2]]$tms$dt





#################################################################
#### other end of the bridge:
#################################################################

World = st_transform(World, crs = 4326)

x = st_geometry(World)



fill = pals::brewer.blues(7)[as.integer(World$economy)]
color = "black"

bbx = st_bbox(World[23,])

# grid
tmapGridInit(bbx)
tmapGridPolygons(x, fill = fill, color = color)
tmapGridRun()

# leaflet
tmapLeafletInit(bbx)
tmapLeafletPolygons(x, fill = fill, color = color)
tmapLeafletRun()
