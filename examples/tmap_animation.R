\dontrun{
data(World, metro)

m1 <- tm_shape(NLD_prov) + 
	      tm_polygons("yellow") +
	  tm_facets(along = "name")

tmap_animation(m1, filename="Dutch_provinces.gif", width=800, delay=40)

m2 <- tm_shape(World, simplify = 0.5) +
          tm_fill() +
      tm_shape(metro) + 
          tm_bubbles(size = paste0("pop", seq(1970, 2030, by=10)),
          		   col = "purple",
          		   border.col = "black", border.alpha = .5,
          		   scale = 2) +
      tm_facets(free.scales.symbol.size = FALSE, nrow=1,ncol=1) + 
      tm_format("World", scale=.5)

tmap_animation(m2, filename="World population.gif", width=1200, delay=100)
}
