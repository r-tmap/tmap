context("qtm")

test_that("qtm without args", {
	tmap_mode("plot")
	expect_error({
		print(qtm())
	})
	
	tmap_mode("view")
	expect_silent({
		qtm()
	})
})

test_that("qtm plots polygons", {
	data(World)
	expect_silent({
		print(qtm(World))
	})
	ttm(); expect_silent(tmap_last())
})

test_that("qtm plots lines", {
	data(rivers)
	expect_silent({
		print(qtm(rivers))
	})
	ttm(); expect_silent(tmap_last())
})

test_that("qtm plots dots", {
	data(metro)
	expect_silent({
		print(qtm(metro))
	})
	ttm(); expect_silent(tmap_last())
})

test_that("qtm plots raster", {
	data(land)
	tmap_options(show.messages = FALSE)
	expect_silent({
		print(qtm(land))
	})
	ttm(); expect_silent(tmap_last())
})
