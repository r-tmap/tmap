\dontrun{
	data(NLD_muni, NLD_prov)
	m <- tm_shape(NLD_muni) +
		     tm_fill(col="population", convert2density=TRUE, 
                 style="kmeans", 
                 title=expression("Population (per " * km^2 * ")"), 
                 legend.hist=FALSE) +
		     tm_borders("black", alpha=.5) + 
		 tm_shape(NLD_prov) +
		     tm_borders("grey25", lwd=2) +
	     tm_format_NLD(inner.margins = c(.02, .15, .06, .15)) + 
	     tm_scale_bar(position = c("left", "bottom")) +
	     tm_compass(position=c("right", "bottom")) + 
	     tm_style_classic()
	
	tmap_save(m, "choropleth.png", height=7)
	
	data(World)
	m2 <- tm_shape(World) +
		tm_fill("well_being", id="name", title="Well-being") +
		tm_format_World()

	# save image
	tmap_save(m2, "World_map.png", width=1920, height=1080, asp=0)

	# cut left inner margin to make sure Antarctica is snapped to frame
	tmap_save(m2 + tm_layout(inner.margins = c(0, -.1, 0.05, 0.01)), 
        "World_map2.png", width=1920, height=1080, asp=0)
	
	# save interactive plot
	tmap_save(m2, "World_map.html")
}
