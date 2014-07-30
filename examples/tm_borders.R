## Europe example
data(Europe)
tm_shape(Europe) + tm_borders()

## Netherlands example
data(NLD_prov)
data(NLD_muni)

tm_shape(NLD_prov) + 
    tm_fill("name") + 
tm_shape(NLD_muni) + 
    tm_borders() + 
tm_shape(NLD_prov) + 
    tm_borders(lwd=2) +
    tm_text("name") +
tm_layout_NLD("Provinces and municipalities", legend.show=FALSE)
