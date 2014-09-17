data(NLD_muni)
data(NLD_prov)


png("NLD_bevolking.png", width=800, height=700)

tm_shape(NLD_muni) +
	tm_fill("population", style = "kmeans", convert2density = TRUE) +
	tm_borders(lwd=.5) +
tm_shape(NLD_prov) +
	tm_borders(lwd=1) +
tm_layout_NLD("Nederlandse bevolking per km2", inner.margins = c(.05,.5,.2,.05), scale=2.5, bg.color="white")
dev.off()
