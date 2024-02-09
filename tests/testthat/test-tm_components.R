test_that("tm_title works (#796)", {
	skip_on_cran()
	a_line <- matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE) %>%
		sf::st_linestring() %>%
		sf::st_sfc() %>%
		sf::st_sf(crs = 4326)
	
	expect_no_warning({
		tm_shape(a_line) +
			tm_lines() +
			tm_title(text = "A line")
	})
})
