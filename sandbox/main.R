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
# 
# library(starsExtra)
# 
# 
# s1 = tmapShape(land, is.main = TRUE, crs = st_crs(land), bbox = NULL, unit = NULL, shp_name = "land")
# 
# ids = which(as.integer(land$cover) == 1)
# 
# s1$shp$values[ids] = ids
# 
# 
# shp = s1$shp
# 
# shp[[1]]
# 
# 
# starsExtra::trim2(s1$shp)
# 
# x = s1$shp
# 
# 
# 
# 
# 
# shp = land[1]
# 
# 
# 
# shp = land[3]
# shp$trees[] = NA
# shp$trees[ids] = ids
# 
# 
# 
# trim2(shp)
# 
# shpTM = do.call(shapeTM, bd$group1$layers$layer1$shpDT$shpTM[[1]])
# shpTM$tmapID = which(as.integer(land$cover) == 1L)
# 
# stm_bbox(shpTM)
# stm_bbox_all(shpTM)
# 
# shpTM = do.call(shapeTM, bd$group2$layers$layer1$shpDT$shpTM[[1]])
# 
# shpTM = shapeTM(World, tmapID = 1:177)
# 
# stm_bbox(shpTM)
# 
# 











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
bd = transData(ad)


##### plotting


for (k in 1:nby[3]) {
	
}


str(bd,3)

################ 







## shp tmapID b1__ by2__
## or 
## shpDT by1__ by2__


#adi$layers[c("trans_dt", "trans_fun")] = list()




#################################################################
#### other end of the bridge:
#################################################################
library(grid)

World = st_transform(World, crs = "+proj=eck4")
World = st_transform(World, crs = 4326)

x = st_geometry(World)


##
by1 = 3
by2 = 1
by3 = 1

x = bd$group2$layers$layer1$shpDT[by1__ == by1 & by2__ == by2, ]$shpTM[[1]]$shp
fill = bd$group2$layers$layer1$mapping_dt[by1__ == by1, ]$fill
color = bd$group2$layers$layer1$mapping_dt[by1__ == by1, ]$color

x = st_transform(x, crs = 4326)


x = bd$group1$layers$layer1$shpDT$shpTM[[1]]$shp
color = bd$group1$layers$layer1$mapping_dt$color

##


fill = pals::brewer.blues(7)[as.integer(World$economy)]
color = "black"

bbx = st_bbox(World[23,])

bbx = st_bbox(World)


fl = attr(bd, "fl")
nby = vapply(fl, length, integer(1))


# grid
tmapGridInit(nrow = nby[1], ncol = nby[2])
#tmapGridShape(bbx = bbx, facet_row = 2, facet_col = 2)

ng = length(bd)

get_shpTM = function(shpDT, by1, by2, by3) {
	b = c(by1, by2, by3)
	bynames = intersect(names(shpDT), paste0("by", 1:3, "__"))
	byids = as.integer(substr(bynames, 3, 3))
	
	sel = rep(TRUE, nrow(shpDT))
	if (length(bynames)) {
		for (i in 1:length(bynames)) {
			sel = sel & shpDT[[bynames[i]]] == b[byids[i]]			
		}
	}
	if (sum(sel) != 1L) stop("multiple shpTMs")
	shpDT$shpTM[[which(sel)]]
}

for (ip in 1L:nby[3]) {
	for (ic in 1L:nby[2]) {
		for (ir in 1L:nby[1]) {
			for (ig in 1L:ng) {
				bdi = bd[[ig]]
				nl = length(bdi$layers)
				for (il in 1L:nl) {
					
					tmapGridShape(bbx = bbx, facet_row = ir, facet_col = ic)
					
					bl = bdi$layers[[il]]
					shpTM = get_shpTM(bl$shpDT, ir, ic, ip)
					mdt = bl$mapping_dt
					
					FUN = paste0("tmapGrid", bl$mapping_fun)

					do.call(FUN, list(shpTM = shpTM, dt = mdt, facet_col = ic, facet_row = ir))
				}
				
			}
		}
	}
}



tmapGridRaster(x, color = color, )
tmapGridPolygons(x, fill = fill, color = color)

tmapGridPolygons(shpTM, )


tmapGridRun()

# leaflet
tmapLeafletInit(bbx)
tmapLeafletPolygons(x, fill = fill, color = color)
tmapLeafletRun()






