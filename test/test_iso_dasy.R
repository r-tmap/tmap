library(maptools)
library(rgeos)

data(land, World)

land_sm <- smooth_raster(land, "trees")
qtm(land_sm$RasterLayer, raster="trees")

## raster
land_cover <- raster_cover(land, "trees", bandwidth = 1, output = c("SpatialPolygons", "RasterLayer"))
qtm(land_cover$RasterLayer, raster="cover") + tm_shape(land_cover$SpatialPolygons) + tm_borders(lwd = 3)

land_iso <- iso_dasymetric(land, var="trees", nrow=NA, ncol=NA, N=250000, nlevels=5, bandwidth=1)


ply <- land_cover$SpatialPolygons
lns <- land_iso$iso

ply <- unionSpatialPolygons(land_iso$dasy, ID=rep(1, length(land_iso$dasy)))

qtm(ply) + qtm(lns)





qtm(dpi2) + qtm(dpi2[24,], fill="red") + tm_bubbles()

qtm(dpi2)



qtm(ply) + qtm(ps[[251]], fill="red") + tm_bubbles()



nps <- length(ps)









tm_shape(land_iso$dasy) +
	tm_fill(col = "level", palette="YlGn") +
tm_shape(land_iso$iso4) +
	tm_lines("purple", lwd=2)


## test subset
ch <- crop(as(land, "RasterBrick"), extent(bb("China")))
ch <- as(ch, "SpatialGridDataFrame")

qtm(ch, raster="trees")

ch_cover <- raster_cover(ch, "trees")
qtm(ch_cover)

ch_iso <- iso_dasymetric(ch, var="trees", nrow=NA, ncol=NA, N=250000, nlevels=5, bandwidth=1)

qtm(ch_iso$dasy)




## points
World_ll <- set_projection(World, "longlat")
World_pop <- sample_dots(World_ll, vars = "pop_est_dens", n = 1000)
qtm(World_pop)

land_iso2 <- iso_dasymetric(World_pop, var="trees", nrow=NA, ncol=NA, N=250000, nlevels=5, bandwidth=1)

##################################################
data(land, World)

iso <- iso_dasymetric(land, "trees")

tm_shape(iso$dasy) +
	tm_fill("level", palette="Blues") +
	tm_shape(iso$iso) +
	tm_lines(col="level", lwd=2, palette="Reds") +
	tm_layout(bg.color="grey80")


################################################
data(metro)
metro_eck <- set_projection(metro, projection = "eck4")

metro_per_country <- tapply(metro_eck$pop2010, INDEX = list(metro_eck$iso_a3), FUN=sum)

World$pop_metro <- 0

metro_per_country_in_World <- metro_per_country[names(metro_per_country) %in% World$iso_a3]
World$pop_metro[match(names(metro_per_country_in_World), World$iso_a3)] <- metro_per_country_in_World
	
World$pop_est_dens_non_metro <- (World$pop_est - World$pop_metro) / World$area



World_pop <- sample_dots(World, vars="pop_est_dens_non_metro", w = 1e6, npop = 7.3e9)
tm_shape(World_pop) + tm_dots()

metro_dots <- do.call("sbind", lapply(1:length(metro_eck), function(i) {
	m <- metro_eck[i,]
	m[rep(1, max(1, m$pop2010 %/% 1e6)),]
}))

dots <- sbind(as(World_pop, "SpatialPoints"), as(metro_dots, "SpatialPoints"))

# test if dots are inside polygons (they do)
tm_shape(dots, bbox=bb(xlim=c(.8e7, 1.6e7), ylim=c(2e6, 7e6))) + tm_dots() + qtm(World, fill=NULL)

library(KernSmooth)
World_pop_iso <- smooth_raster(dots, weight = 1e6, bandwidth = c(100000,100000))
qtm(World_pop_iso$RasterLayer, raster="count") + qtm(World, fill=NULL)



World_pop_iso <- iso_dasymetric(dots, weight = 1e6, cover=World, nlevels = 20)
World_pop_iso <- iso_dasymetric(dots, weight = 1e6, bandwidth = c(100000,100000), cover=World, nlevels = 20)


qtm(World_pop)

World_pop_iso <- iso_dasymetric(World_pop, weight = 677450, bandwidth = 10, cover=World)
World_pop_iso <- iso_dasymetric(World_pop, weight = 677450, bandwidth = 1, cover=World, N=1e5)

tm_shape(World_pop_iso$dasy) +
	tm_fill("level", palette="Blues", max.categories = 40) +
	tm_shape(World_pop_iso$iso) +
	tm_lines(col = "black") +
	tm_layout(bg.color="grey80")


