\dontrun{
#### Choropleth with OSM background

# load Netherlands shape
data(NLD_muni)

# read OSM raster data
osm_NLD <- read_osm(NLD_muni, ext=1.1)

# plot with regular tmap functions
tm_shape(osm_NLD) +
	tm_raster() +
tm_shape(NLD_muni) +
	tm_polygons("population", convert2density=TRUE, style="kmeans", alpha=.7, palette="Purples")

#### A close look at the building of Statistics Netherlands in Heerlen

# create a bounding box around the CBS (Statistics Netherlands) building
CBS_bb <- bb("CBS Weg 11, Heerlen", width=.003, height=.002)

# read Microsoft Bing satellite and OpenCycleMap OSM layers
CBS_osm1 <- read_osm(CBS_bb, type="bing")
CBS_osm2 <- read_osm(CBS_bb, type="opencyclemap")

# plot OSM raster data
qtm(CBS_osm1)
qtm(CBS_osm2)

# read vectorized OSM data
CBS_osm3 <- read_osm(CBS_bb, 
					 roads=osm_line("highway"),
					 parking=osm_poly("amenity=parking"),
					 building=osm_poly("building"),
					 park=osm_poly("leisure=park"),
					 railway_area=osm_poly("landuse=railway"),
					 railway=osm_line("railway"),
					 forest=osm_poly("landuse=forest"),
					 grass=osm_poly("landuse=grass"),
					 bicycle=osm_line("highway=cycleway"))

# plot vectorized OSM data
tm_shape(CBS_osm3$grass, bbox=CBS_bb) + tm_polygons("darkolivegreen3") +
	tm_shape(CBS_osm3$forest) + tm_fill("forestgreen") +
	tm_shape(CBS_osm3$railway_area) + tm_fill(col="grey70") +
	tm_shape(CBS_osm3$parking) + tm_polygons("gold") +
	tm_shape(CBS_osm3$building) + tm_polygons("grey50") +
	tm_shape(CBS_osm3$roads, bbox=CBS_bb) + tm_lines(col="gold", lwd=3) + 
	tm_shape(CBS_osm3$bicycle) + tm_lines(col="blue", lwd=3) + 
	tm_shape(CBS_osm3$railway) + tm_lines(col="grey20", lwd=3, lty="dashed") + 
	tm_layout(bg.color="grey90")
}
