test_that("fancy_breaks() handles NA with large values (big.num.abbr path)", {
	skip_on_cran()
	expect_no_error(fancy_breaks(c(1e6, NA, 3e6)))
	expect_equal(
		fancy_breaks(c(1e6, NA, 3e6)),
		c("1 mln", "NA mln", "3 mln"),
		ignore_attr = TRUE
	)
})
