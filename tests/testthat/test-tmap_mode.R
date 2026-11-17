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

test_that("hover.text.size sets the size of hover labels", {
	tmap_mode("view")
	on.exit(tmap_mode("plot"))
	l = tmap_leaflet(tm_shape(World) + tm_polygons(hover = "name") + tm_view(hover.text.size = 16))
	pc = Filter(function(x) x$method == "addPolygons", l$x$calls)[[1]]
	ts = unlist(lapply(pc$args, function(a) if (is.list(a)) a$textsize))
	expect_identical(ts, "16px")
})

test_that("mode-specific options can be set with tmap_options()", {
	tmap_mode("view")
	on.exit({tmap_options_reset(); tmap_mode("plot")})
	expect_no_warning(tmap_options(use_circle_markers = FALSE, hover.text.size = 18))
	expect_identical(tmap_options("use_circle_markers")$use_circle_markers, FALSE)
	expect_identical(tmap_options("hover.text.size")$hover.text.size, 18)
})

test_that("smooth_factor is passed on to leaflet", {
	tmap_mode("view")
	on.exit(tmap_mode("plot"))
	l = tmap_leaflet(tm_shape(World) + tm_polygons() + tm_view(smooth_factor = 0.2))
	pc = Filter(function(x) x$method == "addPolygons", l$x$calls)[[1]]
	expect_true(any(vapply(pc$args, function(a) is.list(a) && identical(a$smoothFactor, 0.2), logical(1))))
})
