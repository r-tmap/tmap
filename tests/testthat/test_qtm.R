context("qtm")

test_that("qtm plots polygons", {
	data(World)
	expect_silent({
		qtm(World)
	})
})
