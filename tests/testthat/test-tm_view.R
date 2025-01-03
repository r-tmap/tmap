test_that("tm_view() errors correctly with wrong position", {
	current_mode <- tmap_mode("view")
	expect_snapshot(error = TRUE, {
		World |>
			tm_shape() +
			tm_crs("auto") +
			tm_polygons(fill = "pop_est") +
			tm_view(control.bases = c("shp1", "shp2"), control.collapse = TRUE, control.position = c("top", "right"))
	})

  tmap_mode(current_mode)
})
