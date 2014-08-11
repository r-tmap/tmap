library(tmap)
library(rgdal)

data(NLD_muni)

NLD_muni$density <- calc_densities(NLD_muni, var = "population") * 1e6
NLD_muni <- set_projection(NLD_muni, projection = "longlat")


get_polygon_ranges(NLD_muni)

writeOGR(NLD_muni, '../test/leaflet2/NLDpop.geojson','dataMap', driver='GeoJSON')

load("../test/NDW_example/throughput/doorlopende_rijkswegen_nuts.rda")
qtm(drw_nuts)

drw_nuts <- set_projection(drw_nuts, projection = "longlat")
writeOGR(drw_nuts, '../test/leaflet2/NLDdrw.geojson','dataMap', driver='GeoJSON')

