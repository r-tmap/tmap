library(tmap)
library(rgdal)

data(NLD_muni)

NLD_muni$density <- calc_densities(NLD_muni, var = "population") * 1e6
NLD_muni <- set_projection(NLD_muni, projection = "longlat")


get_polygon_ranges(NLD_muni)

writeOGR(NLD_muni, '../test/leaflet2/dataMap.geojson','dataMap', driver='GeoJSON')
