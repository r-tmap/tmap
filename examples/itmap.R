\dontrun{
data(World, metro)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

require(dplyr)

(tm_shape(World) +
	tm_polygons("income_grp", palette="-Blues", contrast=.7, id="name", title="Income group") +
	tm_shape(metro) +
	tm_bubbles("pop2010", col = "growth", 
			   border.col = "black", border.alpha = .5, 
			   style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
			   palette="-RdYlBu", contrast=1, 
			   title.size="Metro population", 
			   title.col="Growth rate (%)", id="name") + 
	tm_layout(legend.bg.color = "grey90", legend.bg.alpha=.5, legend.frame=TRUE, asp=0)) %>% 
itmap()
}