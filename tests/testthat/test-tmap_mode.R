test_that("tmap_mode is plot by default.", {
	skip_on_cran()
	tmap_mode("plot")
	expect_message(tmap_mode(), "current tmap mode is \"plot\"")
})

