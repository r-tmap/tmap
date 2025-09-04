test_that("tmap_mode is plot by default.", {
	skip_on_cran()
	m = tmap_mode()
	expect_equal(m, "plot")
})
