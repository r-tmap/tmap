## Netherlands - Maastricht area
data(NLD_muni)
NLD_muni$Area <- approx_areas(NLD_muni)
NLD_muni <- set_projection(NLD_muni, "longlat")

MST <- NLD_muni[NLD_muni$name=="Maastricht",]
bb_mst <- MST@bbox
bb_mst[1:4] <- c(5.70, 50.84, 5.72, 50.85)


shp_rst <- read_osm(bb_mst, raster=TRUE)

shps <- read_osm(bb_mst, poly = "building")
shps <- read_osm(bb_mst, poly = "building", line = "highway", point="amenity")


tm_shape(shp_rst) +
	tm_raster() +
tm_shape(shps$poly) +
	tm_fill("red") +
tm_shape(shps$line) +
	tm_lines("blue") +
tm_shape(shps$point) +
	tm_bubbles(size=.3, col="purple") +
tm_layout(inner.margins=0)
	
# choropleth with osm background
data(NLD_muni)
NLD_muni$area <- approx_areas(NLD_muni)
NLD_muni <- set_projection(NLD_muni, "longlat")
bb_NLD <- NLD_muni@bbox

osm_NLD <- read_osm(bb(bb_NLD, ext=1.1), raster = TRUE)
tm_shape(osm_NLD) +
	tm_raster() +
tm_shape(NLD_muni) +
	tm_polygons("population", convert2density=TRUE, area="area", style="kmeans", alpha=.7, palette="Reds")

# meuse dataset with osm background
require(sp)
data(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")

meuse_osm <- read_osm(bb(meuse, ext=1.2, projection = "longlat"), raster=TRUE)

tm_shape(meuse_osm) +
	tm_raster() +
	tm_shape(meuse) +
	tm_bubbles(size=.3, col="zinc", border.col = "black", style="kmeans", palette="Reds", contrast=c(.2, 1))

# user2015
aalborg_bb <- bb(xlim = c(9.9075, 9.9175), ylim=c(57.042, 57.045))
aalborg_osm_rast <- read_osm(aalborg_bb, raster=TRUE)


aalborg_osm_vec <- read_osm(aalborg_bb, 
							buildings=osm_poly("building"),
							roads=osm_line("highway"),
							trees=osm_point("natural=tree"),
							park=osm_poly("leisure=park"),
							cemetery=osm_poly("landuse=cemetery"),
							water=osm_poly("natural=water"),
							railway=osm_line("railway"))

qtm(aalborg_osm_rast) +
tm_shape(aalborg_osm_vec$park, bbox=aalborg_bb) +
	tm_polygons(col = "darkolivegreen3") +
tm_shape(aalborg_osm_vec$cemetery) +
	tm_polygons(col="grey80") +
tm_shape(aalborg_osm_vec$building) +
	tm_polygons(col = "gold") +
tm_shape(aalborg_osm_vec$roads) +
	tm_lines("grey40", lwd = 2) +
tm_shape(aalborg_osm_vec$water) + 
	tm_polygons(col = "lightblue") +
tm_shape(aalborg_osm_vec$trees) + 
	tm_bubbles(size=.25, col="forestgreen") +
tm_shape(aalborg_osm_vec$railway) +
	tm_lines(col = "grey20", lwd = 2, lty = "dashed")
	

57.043182,9.913029,16
