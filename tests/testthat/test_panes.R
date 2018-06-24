context("panes")

test_that("map panes", {
	data(World)
	tmap_mode("view")
	
	expect_silent({
		print({
		tm_basemap(leaflet::providers$Stamen.Watercolor) +
			tm_shape(World) +
			tm_fill("HPI") +
			tm_tiles(leaflet::providers$Stamen.TonerLabels) +
			tm_shape(World) +
			tm_bubbles(size = "gdp_cap_est", legend.size.show = FALSE) +
			tm_tiles(leaflet::providers$Stamen.TonerLines)
		})
	})
})
