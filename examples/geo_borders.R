data(Europe)

geo_shape(Europe) + geo_borders()

data(NLD_prov)
data(NLD_muni)

geo_shape(NLD_prov) + 
	geo_choropleth("name") + 
geo_shape(NLD_muni) + 
	geo_borders("gray25") + 
geo_shape(NLD_prov) + 
	geo_borders(lwd=2) +
	geo_text("name") +
geo_theme_NLD("Provinces and municipalities", legend.profile="hide")
	