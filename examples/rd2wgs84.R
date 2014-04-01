# load data
data(NLD_muni)

# convert to Rijksdriehoekstelsel
NLD_muni_RD <- rd2wgs84(NLD_muni)

# plot maps
geo_shape(NLD_muni) + geo_borders() + geo_theme(title="WGS84")
geo_shape(NLD_muni) + geo_borders() + geo_theme(title="RD")

