data(World, Europe, metro)

tm_shape(World) +
    tm_text("name", size="AREA")

tm_shape(Europe) +
    tm_polygons() +
    tm_text("iso_a3", size="AREA", col = "grey20", root=4, shadow = TRUE, scale=2,
        size.lowerbound = .1) +
tm_shape(Europe) +
    tm_text("name", size="AREA", root=4, scale=1,
        ymod=-1 * approx_areas(Europe, unit = "norm")^(1/4))

tm_shape(Europe) +
	tm_polygons() +
tm_shape(metro) +
	tm_bubbles("pop2010", size.lim = c(0, 15e6), legend.size.is.portrait = TRUE, 
			   title.size = "European metropolitan areas") +
tm_shape(metro[metro$pop2010>=2e6, ]) +
	tm_text("name", auto.placement = TRUE)

tm_shape(World) +
	tm_text("name", size="pop_est", col="continent", palette="Dark2", title.size = "Population", title.col="Continent")