data(Europe)

animation_tmap({
	tm_shape(Europe) + 
		tm_fill("yellow") + 
		tm_borders() + 
		tm_facets(by = "name", nrow=1,ncol=1) + 
		tm_layout(scale=2)
}, width=1200, height=800, delay=100, filename="European countries.gif")

data(World, metro)

animation_tmap({
	tm_shape(World) +
		tm_polygons() +
	tm_shape(metro) + 
		tm_bubbles(paste0("pop", seq(1970, 2030, by=10)), border.col = "black", border.alpha = .5) +
		tm_facets(free.scales.bubble.size = FALSE, nrow=1,ncol=1) + 
		tm_format_World(scale=2, outer.margins=0,asp=0)
}, width=1200, height=550, delay=100, filename="World population.gif")
