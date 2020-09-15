\dontrun{
data(NLD_prov)

m1 <- tm_shape(NLD_prov) + 
	      tm_polygons("yellow") +
	  tm_facets(along = "name")

tmap_animation(m1, delay=40)

data(World, metro)

m2 <- tm_shape(World, simplify = 0.5) +
          tm_fill() +
      tm_shape(metro) + 
          tm_bubbles(size = paste0("pop", seq(1970, 2030, by=10)),
          		   col = "purple",
          		   border.col = "black", border.alpha = .5,
          		   scale = 2) +
      tm_facets(free.scales.symbol.size = FALSE, nrow=1,ncol=1) + 
      tm_format("World")

tmap_animation(m2, delay=100, outer.margins = 0)

m3 <- lapply(seq(50, 85, by = 5), function(age) {
	World$at_most <- World$life_exp <= age
	World_sel <- World[which((World$life_exp <= age) & (World$life_exp > (age - 5))), ]
	tm_shape(World) +
		tm_polygons("at_most", palette = c("gray95", "gold"), legend.show = FALSE) +
		tm_shape(World_sel) +
		tm_text("name", size = "AREA", root = 5, remove.overlap = TRUE) +
		tm_layout(main.title = paste0("Life expectency at most ", age), frame = FALSE)
})

tmap_animation(m3, width = 1200, height = 600, delay = 100)

m4 <- tm_shape(World) +
	tm_polygons() +
tm_shape(metro) +
	tm_bubbles(col = "red") +
	tm_text("name", ymod = -1) +
tm_facets(by = "name", free.coords = F, nrow = 1, ncol = 1) +
	tm_layout(panel.show = FALSE, frame = FALSE)

tmap_animation(m4, filename = "World_cities.mp4", 
    width=1200, height = 600, fps = 2, outer.margins = 0)
}
