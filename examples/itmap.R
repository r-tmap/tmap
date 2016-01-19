data(World, metro)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

tmap_mode("view")
tm_shape(World) +
		   	tm_polygons("income_grp", palette="-Blues", contrast=.7, id="name", title="Income group") +
		   	tm_shape(metro) +
		   	tm_bubbles("pop2010", col = "growth", 
		   			   border.col = "black", border.alpha = .5, 
		   			   style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
		   			   palette="-RdYlBu", contrast=1, 
		   			   title.size="Metro population", 
		   			   title.col="Growth rate (%)", id="name") + 
		   	tm_layout(legend.bg.color = "grey90", legend.bg.alpha=.5, legend.frame=TRUE)


data(NLD_muni, NLD_prov)
tm_shape(NLD_muni) +
	tm_fill(col="population", convert2density=TRUE, 
			style="kmeans", title="Population (per km2)", legend.hist=TRUE, id = "name") +
	tm_borders("grey25", alpha=.5) + 
	tm_shape(NLD_prov) +
	tm_borders("grey40", lwd=2) +
	tm_format_NLD_wide(bg.color="white", frame = FALSE, legend.hist.bg.color="grey90")
