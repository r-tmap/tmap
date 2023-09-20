test_that("tmap_mode is plot by default.", {
	expect_message(tmap_mode(), "current tmap mode is \"plot\"")
})
