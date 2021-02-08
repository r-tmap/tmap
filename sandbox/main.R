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
x = updateData(tmo)


# determine levels for facets
get_fact_lev = function() {
	fl = list(1L, 1L, 1L)
	for (g in x) {
		for (l in g) {
			for (dt in c(l$trans, l$mapping)) {
				for (bi in 1L:3L) {
					by_var = paste0("by", bi, "__")
					by_col = dt[[by_var]]
					by_isn = is.integer(by_col)
					by_nlv = if (by_isn) max(by_col) else nlevels(by_col)
					
					fi = fl[[bi]]
					fi_isn = is.integer(fi) 
					fi_nlv = if (fi_isn) fi else length(fi)
					
					if (by_nlv > 1L && fi_nlv > 1L && by_nlv != fi_nlv) {
						stop("number of facets in plotting dimension", bi, "is not consistent", call. = FALSE)
					} else if (by_nlv > 1L && fi_isn) {
						fl[[bi]] = if (by_isn) by_nlv else levels(by_col)
					}
				}
			}
		}
	}
	fl
}
fl = get_fact_lev()

names(fl) = paste0("by", 1:3, "__")

dt = x$group1$layer1$mapping$color


dt

for (i in 1:3) {
	byname = paste0("by", i, "__")
	dt[, (byname) := as.integer(get(..byname))]
	if (max(dt[[byname]]) == 1L && length(fl[[i]]) > 1L) {
		dt = rbindlist(lapply(1L:length(fl[[i]]), function(j) {
			copy(dt)[, (byname) := j]
		}))
		
	}
}


completeDT <- function(DT, cols, defs = NULL){
	mDT = do.call(CJ, c(DT[, ..cols], list(unique=TRUE)))
	res = DT[mDT, on=names(mDT)]
	if (length(defs)) 
		res[, names(defs) := Map(replace, .SD, lapply(.SD, is.na), defs), .SDcols=names(defs)]
	res[]
} 






res = dt[J(as.data.table(fl)), on = names(fl)]



# harmonize data
x2 = lapply(x, function(g) {
	lapply(g, function(l) {
		lapply(l, function(dt) {
			dt
			
			
			
			if (nlevels(dt$by1__) < length(flev[[1]])
			
			if (nlevels(dt$by1__) < length(flev[[1]])) {
				
			}
			
		})
	})
})


dt







nf_per_aes = do.call(rbind, lapply(x, function(g) {
	do.call(rbind, lapply(g, function(l) {
		do.call(rbind, lapply(l, function(dt) {
			c(nlevels(dt$by1__), nlevels(dt$by2__), nlevels(dt$by2__))
		}))
	}))
}))

nf = apply(nf_per_aes, MARGIN = 2, max)

apply(nf_per_aes, MARGIN = 1, FUN = function(nfi) {
	if (any(nfi > 1) )
})






lapply(x, function(g) {
	lapply(g, function(l) {
		lapply(l, function(dt) {
			nfi = c(nlevels(dt$by1__), nlevels(dt$by2__), nlevels(dt$by2__))
			
		})
	})
})


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
