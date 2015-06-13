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
	

