context("classic world map")

test_that("qtm plots polygons", {
	data(World, land, rivers)
	tmap_mode("plot")
	expect_silent({
		tm_shape(land) +
			tm_raster("elevation", breaks=c(-Inf, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, Inf),  
					  palette = terrain.colors(9), title="Elevation") +
			tm_shape(World, is.master=TRUE) +
			tm_borders("grey20") +
			tm_shape(rivers) +
			tm_lines(col = "navyblue", lwd = "strokelwd", legend.lwd.show = FALSE) +
			tm_grid(projection="longlat", labels.size = .5) +
			tm_shape(World) +
			tm_text("name", size="AREA") +
			tm_compass(position = c(.65, .15), color.light = "grey90") +
			tm_credits("Eckert IV projection", position = c("right", "BOTTOM")) +
			tm_scale_bar(position = c("center", "bottom")) +
			tm_style("classic") +
			tm_layout(bg.color="lightblue",
					  inner.margins=c(.04,.03, .02, .01), 
					  earth.boundary = TRUE, 
					  space.color="grey90") +
			tm_legend(position = c("left", "bottom"), 
					  frame = TRUE,
					  bg.color="lightblue")
		
	})
	
	ttm()
	expect_warning({
		tmap_last()
	})
	
	
})

