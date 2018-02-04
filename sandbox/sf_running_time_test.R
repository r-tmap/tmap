data(World, land)
WorldOne <- rgeos::gUnaryUnion(World)


load("~/Dropbox/projects/tmap_extra/data/US.rdata")
load("~/Dropbox/projects/tmap_extra/data/crimes.rdata")

sapply(ls(), function(lsi) {
	obj <- get(lsi)
	if (inherits(obj, "Spatial") && !inherits(obj, c("SpatialGrid", "SpatialRaster"))) {
		obj <- as(obj, "sf")
		assign(lsi, obj, envir = .GlobalEnv)
	}
	
	if (is.list(obj) && !inherits(obj, "data.frame")) {
		obj <- lapply(obj, FUN = function(o) {
			if (inherits(o, "Spatial") && !inherits(o, c("SpatialGrid", "SpatialRaster"))) {
				o <- as(o, "sf")
			} else {
				o
			}
		})
		assign(lsi, obj, envir = .GlobalEnv)
	}
	NULL	
})



tm1 <- tm_shape(World) + 
	tm_fill("pop_est_dens", style="kmeans", title="Population density") +
	tm_format_World(title="The World") + tm_style_albatross(frame.lwd=10)

tm2 <- tm_shape(land) +
	tm_raster("elevation", breaks=c(-Inf, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, Inf),  
			  palette = terrain.colors(9), title="Elevation", auto.palette.mapping = FALSE) +
	tm_shape(World, is.master=TRUE) +
	tm_borders("grey20") +
	tm_grid(projection="longlat", labels.size = .5) +
	tm_text("name", size="AREA") +
	tm_compass(position = c(.65, .15), color.light = "grey90") +
	tm_credits("Eckert IV projection", position = c("right", "BOTTOM")) +
	tm_layout(inner.margins=c(.04,.03, .02, .01), 
			  earth.boundary = TRUE, 
			  space.color="grey90") +
	tm_style_classic(bg.color="lightblue") +
	tm_legend(position = c("left", "bottom"), 
			  frame = TRUE,
			  bg.color="lightblue")

tm3 <- tm_shape(World, projection="robin") +
	tm_fill("HPI", palette="div", auto.palette.mapping = FALSE, n=7, 
			title = "Happy Planet Index") +
	tm_shape(WorldOne) + 
	tm_borders() +
	tm_grid(projection = "longlat") +
	tm_credits("Winkel Tripel projection", position = c("right", "BOTTOM")) +
	tm_style_natural(earth.boundary = c(-180, 180, -87, 87), inner.margins = .05) +
	tm_legend(position=c("left", "bottom"), bg.color="grey95", frame=TRUE)


tm4 <- tm_shape(crimes) +
	tm_dots("Crime.type")

tm5 <- tm_shape(US) +
	tm_polygons()

tm6 <- tm_shape(rivers) +
	tm_lines()



ttm()
system.time(print(tm1))
system.time(print(tm2))
system.time(print(tm3))
system.time(print(tm4))
system.time(print(tm5))
system.time(print(tm6))

Rprof(tmp <- tempfile())
print(tm5)
Rprof()
summaryRprof(tmp)
unlink(tmp)





