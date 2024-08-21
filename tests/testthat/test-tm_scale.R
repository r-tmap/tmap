test_that("tm_scale() works", {
	skip_on_cran()
	expect_no_error(
		tm_world <- World |> 
		tm_shape() +
		tm_fill(
			fill = "area",
			fill.scale = tm_scale_continuous_pseudo_log()
		)
	)
	expect_no_error(tm_world)
	expect_no_error(tmap_leaflet(tm_world) |> 
		addLayersControl(
			baseGroups = "area",
			options = layersControlOptions(
				collapsed = FALSE
			)
		)
	)
	
})

test_that("tm_scale_continuous_pseudo_log() works with special words", {
	skip_on_cran()
	expect_no_error(
		tm_world <- World |> 
			tm_shape() +
			tm_fill(
				fill = "MAP_COLORS",
				fill.scale = tm_scale_continuous_pseudo_log()
			)
	)
	expect_no_error(tm_world)
	mod <- tmap_mode("view")
	expect_no_error(tm_world)
	tmap_mode(mod)
})
