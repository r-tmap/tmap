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
tmel = tm_shape(land) +
	tm_raster("trees") +
tm_shape(World, name = "The World", is.main = TRUE) +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln")) +
	tm_symbols(color = c("blue", "red"), size = "life_exp") +
tm_facets_wrap(by = "continent")

# size variables mapped to columns
tmel = tm_shape(land) +
	tm_raster("trees") +
tm_shape(World, name = "The World", is.main = TRUE) +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln")) +
	tm_symbols(color = c("blue", "red"), size = "life_exp") +
tm_facets_grid(rows = "continent")

# size variables mapped to columns (free scales)
tmel = tm_shape(land) +
	tm_raster("trees") +
	tm_shape(World, name = "The World", is.main = TRUE) +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln"), size.free = TRUE) +
	tm_symbols(color = c("blue", "red"), size = "life_exp", size.free = TRUE) +
	tm_facets_grid(rows = "continent")



# color and size aes have different free dimensions
tmel = tm_shape(World, name = "The World", is.main = TRUE) +
	tm_symbols(color = "HPI", size = "life_exp", size.free = c(TRUE, FALSE, FALSE), color.free = c(FALSE, TRUE, FALSE)) +
	tm_facets_grid(rows = "continent", columns = "alpha_class")



# wrap mvars
tmel = tm_shape(World) +
	tm_polygons(fill = c("well_being", "well_being2"))

tmel = tm_shape(World) +
	tm_polygons(fill = c("well_being", "well_being2")) +
	tm_symbols(color = c("red", "purple")) +
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
ad = updateData(tmo)





################ 

# loop over ad's
adi = ad[[2]]


shpDT = adi$shapeDT

al = adi$layers[[1]]

for (al in adi$layers) {
	if (al$trans_isglobal) {
		transDT = al$trans_dt
	
		bycols = names(transDT)[substr(names(transDT), 1, 2) == "by"]
		sdcols = names(transDT)#[c(1L, ncol(transDT))]
		
		transDT[, .(shp = do.call(do_trans, list(tdt = .SD, FUN = al$trans_fun))), by = bycols, .SDcols = sdcols]	
		#transDT[, .(shp = do.call(do_trans, c(as.list(.SD), list(FUN = al$trans_fun)))), by = bycols, .SDcols = sdcols]	
		#transDT[, .(shp = do.call(do_trans, as.list(.SD))), by = bycols, .SDcols = sdcols]	
	}
}

#do_trans = function(tmapID__, ..., FUN) {
do_trans = function(tdt, FUN) {
	browser()
	
	shpDT
	
	res = do.call(FUN, c(list(shp = shp[tmapID__]), list(...)))
	res$tmapID = tmapID__
	list(res)
}




#####################
str(tmo[[1]],2)

str(x[[2]],1)


shp = tmo[[2]]$tms$shp
dt = x$group2$layer1$trans$size


tmaptransCartogram = function(shp, size) {
	x = st_sf(geometry = shp, weight = size)
	require(cartogram)
	list(shp = cartogram::cartogram_cont(x, weight = "weight", itermax = 5))
}

shp2 = tmaptransCartogram(shp, size = World$HPI)

do_trans = function(tmapID__, ...) {
	res = do.call(tmaptransCartogram, c(list(shp = shp[tmapID__]), list(...)))
	res$tmapID = tmapID__
	list(res)
}

dt[, shape:= NULL]
da = dt[, .(shape = do.call(do_trans, as.list(.SD))), by = c("by1__", "by2__"), .SDcols = c("tmapID__", "size")]






#################################################################
#### other end of the bridge:
#################################################################
library(grid)

World = st_transform(World, crs = "+proj=eck4")
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
