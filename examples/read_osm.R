#### Choropleth with OSM background

# load Netherlands shape
data(NLD_muni)

# get bounding box of the Netherlands in longitude latitude coordinates 
bb_NLD <- bb(NLD_muni, projection = "longlat")

# read OSM raster data
osm_NLD <- read_osm(bb(bb_NLD, ext=1.1))

# plot with regular tmap functions
tm_shape(osm_NLD) +
	tm_raster() +
tm_shape(NLD_muni) +
	tm_polygons("population", convert2density=TRUE, style="kmeans", alpha=.7, palette="Reds")

#### A close look at Aalborg Congress Centre (host for the useR2014)

# define bounding box of Aalborg Congress Centre
bb_Aal <- bb(xlim = c(9.9075, 9.9175), ylim=c(57.043, 57.046))

# read OSM raster data
rast_Aal <- read_osm(bb_Aal, type="mapquest")

# raster OSM of Aalburg
qtm(rast_Aal)

# read OSM vector data
vec_Aal <- read_osm(bb_Aal,
					buildings=osm_poly("building"),
					roads=osm_line("highway"),
					trees=osm_point("natural=tree"),
					park=osm_poly("leisure=park"),
					cemetery=osm_poly("landuse=cemetery"),
					railway=osm_line("railway"),
					parking=osm_poly("amenity=parking"))

# vector OSM of Aalburg
tm_shape(vec_Aal$park, bbox=bb_Aal) +
	tm_polygons(col = "darkolivegreen3") +
tm_shape(vec_Aal$cemetery) +
	tm_polygons(col="darkolivegreen3") +
tm_shape(vec_Aal$parking) +
	tm_polygons(col="grey85") +
tm_shape(vec_Aal$building) +
	tm_polygons(col = "gold") +
tm_shape(vec_Aal$roads) +
	tm_lines("grey40", lwd = 3) +
tm_shape(vec_Aal$trees) + 
	tm_bubbles(size=.25, col="forestgreen") +
tm_shape(vec_Aal$railway) +
	tm_lines(col = "grey40", lwd = 3, lty = "longdash") +
tm_layout(inner.margins=0, bg.color="grey95")
