test_that("tm_borders work", {
	# Borders only
	map <- tm_shape(World) + tm_borders()
	map
	expect_equal(map[[1]]$shp_name, "World")
})
