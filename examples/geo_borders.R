## Europe example
data(Europe)
geo_shape(Europe) + geo_borders()

## Netherlands example
data(NLD_prov)
data(NLD_muni)

geo_shape(NLD_prov) + 
	geo_fill("name") + 
geo_shape(NLD_muni) + 
	geo_borders() + 
geo_shape(NLD_prov) + 
	geo_borders(lwd=2) +
	geo_text("name") +
geo_theme_NLD("Provinces and municipalities", legend.show=FALSE)
