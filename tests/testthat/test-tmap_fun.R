test_that("functions are found in loaded namespaces of extension packages", {
	# functions found from the tmap namespace are returned by name
	expect_identical(tmap_fun("tmapScaleCategorical"), "tmapScaleCategorical")
	expect_identical(tmap_fun("a_function_that_does_not_exist"), "a_function_that_does_not_exist")
	expect_identical(tmap_fun(sum), sum)
})

test_that("layers of extension packages work without attaching them", {
	skip_if_not_installed("tmap.glyphs")
	ZH = NLD_muni[NLD_muni$province == "Zuid-Holland", ]
	ZH$income_middle = 100 - ZH$income_high - ZH$income_low
	expect_no_error(suppressMessages(print(tm_shape(ZH) + tm_polygons() +
		tmap.glyphs::tm_donuts(parts = tm_vars(c("income_low", "income_middle", "income_high"), multivariate = TRUE)))))
})
