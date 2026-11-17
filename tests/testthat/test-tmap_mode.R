test_that("tmap_mode is plot by default.", {
	skip_on_cran()
	m = tmap_mode()
	expect_equal(m, "plot")
})

test_that("a single basemap is not a radio button when data layers use radio groups", {
	tmap_mode("view")
	on.exit(tmap_mode("plot"))
	l = tmap_leaflet(tm_basemap("Esri.WorldGrayCanvas") +
		tm_shape(World) + tm_polygons("HPI", group = "A", group.control = "radio") +
		tm_polygons("life_exp", group = "B", group.control = "radio"))
	lc = Filter(function(x) x$method == "addLayersControl", l$x$calls)[[1]]
	expect_identical(lc$args[[1]], c("A", "B"))
})
