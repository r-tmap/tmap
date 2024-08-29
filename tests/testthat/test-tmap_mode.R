test_that("tmap_mode is plot by default.", {
	skip_on_cran()
	expect_message(tmap_mode(), "Current tmap mode is \"plot\"")
})
