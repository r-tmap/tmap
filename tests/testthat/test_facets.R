context("facets")

# tests <- list(
# 	list(layer = "polygons", args = list(col = "green"), use_filter = FALSE),
# 	list(layer = "polygons", args = list(col = "v1"), use_filter = FALSE),
# 	list(layer = "polygons", args = list(col = "v2"), use_filter = FALSE),
# 	list(layer = "lines", args = list(col = "green", lwd = 10), use_filter = FALSE),
# 	list(layer = "lines", args = list(col = "v1", lwd = 10), use_filter = FALSE),
# 	list(layer = "lines", args = list(col = "v2", lwd = 10), use_filter = FALSE),
# 	list(layer = "lines", args = list(col = "blue", lwd = "v1", scale = 5), use_filter = FALSE),
# 	list(layer = "lines", args = list(palette = "Set1", col = "v2", lwd = "v1", scale = 5), use_filter = FALSE),
# 	list(layer = "symbols", args = list(col = "green"), use_filter = FALSE),
# 	list(layer = "symbols", args = list(col = "v1"), use_filter = FALSE),
# 	list(layer = "symbols", args = list(col = "v2"), use_filter = FALSE),
# 	list(layer = "symbols", args = list(col = "blue", size = "v1"), use_filter = FALSE),
# 	list(layer = "symbols", args = list(col = "red", shape = "v2"), use_filter = FALSE),
# 	list(layer = "text", args = list(col = "green", text = "name2"), use_filter = FALSE),
# 	list(layer = "text", args = list(col = "red", text = "name2",  size = "v1"), use_filter = FALSE),
# 	list(layer = "text", args = list(col = "v2", text = "name2"), use_filter = FALSE)
# )

source("../helper_functions/facet_test_functions.R")

test_that("facets (polygons, plot)", {
	tmap_mode("plot")
	
	tests <- list(
		list(layer = "polygons", args = list(col = "green"), use_filter = FALSE),
		list(layer = "polygons", args = list(col = "v1"), use_filter = FALSE),
		list(layer = "polygons", args = list(col = "v2"), use_filter = FALSE))
	
	nrs <- sapply(tests, run_facet_test)

	expect_equal(sum(nrs), expected = 0)
})
