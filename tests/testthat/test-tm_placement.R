test_that("tm_placement validates and completes the fallback chain", {
	expect_equal(tm_placement()$fallback, c("wrap", "rotate", "shrink", "move", "drop"))
	expect_equal(tm_placement(fallback = "wrap")$fallback, c("wrap", "overlap"))
	expect_error(tm_placement(fallback = c("drop", "wrap")))
	expect_error(tm_placement(fallback = c("wrap", "drop", "overlap")))
	expect_error(tm_placement(fallback = "fly"))
	expect_error(tm_placement(rotate.max = 120))
	expect_error(tm_placement(shrink.min = 0))
	expect_null(placement_resolve("none"))
	expect_equal(placement_resolve("inside")$fallback, c("wrap", "rotate", "shrink", "drop"))
	expect_equal(placement_resolve("callout")$move, "column")
	expect_error(placement_resolve("everywhere"))
})

test_that("wrap variants balance the lines", {
	expect_equal(placement_wrap_variants("Central African Rep."),
				 c("Central\nAfrican Rep.", "Central\nAfrican\nRep."))
	expect_length(placement_wrap_variants("Chad"), 0)
})

test_that("boxes are fitted inside polygons", {
	# an L-shaped polygon: the horizontal arm is 10 x 2, the vertical arm 2 x 10
	L = sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 2), c(2, 2), c(2, 10), c(0, 10), c(0, 0))))
	e = placement_edges(L)
	a = placement_poly_anchor(sf::st_sfc(L))
	anchor = sf::st_coordinates(a$shp)[1, 1:2]
	# a 6 x 1.5 box fits horizontally in the lower arm
	pos = placement_fit_box(e, anchor, a$r, w = 6, h = 1.5, angle = 0)
	expect_false(is.null(pos))
	expect_true(pos[2] > 0.75 && pos[2] < 1.25)
	# a 6 x 1.5 box rotated 90 degrees fits in the vertical arm
	pos90 = placement_fit_box(e, anchor, a$r, w = 6, h = 1.5, angle = 90)
	expect_false(is.null(pos90))
	expect_true(pos90[1] > 0.75 && pos90[1] < 1.25)
	# a 12 x 1.5 box does not fit
	expect_null(placement_fit_box(e, anchor, a$r, w = 12, h = 1.5, angle = 0))
	# a box wider and higher than the arms does not fit
	expect_null(placement_fit_box(e, anchor, a$r, w = 3, h = 3, angle = 0))
})

test_that("tm_labels with placement plots in plot and view mode", {
	Africa = World[World$continent == "Africa", ]
	tmap_mode("plot")
	m = tm_shape(Africa) + tm_polygons() + tm_labels("name")
	expect_no_error(print(m))
	expect_no_error(print(tm_shape(Africa) + tm_polygons() +
		tm_labels("name", options = opt_tm_labels(placement = tm_placement(fallback = c("wrap", "overlap"))))))
	tmap_mode("view")
	expect_no_error(tmap_leaflet(m))
	tmap_mode("plot")
})

test_that("tm_leader validates", {
	expect_equal(tm_leader()$type, "straight")
	expect_error(tm_leader(type = "curly"))
	expect_error(tm_leader(min.length = -1))
})

test_that("1D layout removes overlap and stays within bounds", {
	pos = placement_layout_1d(ideal = c(0, 0.1, 0.2), h = c(1, 1, 1), gap = 0.1, lo = -10, hi = 10)
	expect_true(all(diff(pos) >= 1.1 - 1e-9))
	expect_equal(mean(pos), 0.1)
	pos = placement_layout_1d(ideal = c(9, 9, 9), h = c(1, 1, 1), gap = 0, lo = -10, hi = 10)
	expect_equal(max(pos) + 0.5, 10)
})

test_that("labels that do not fit are moved outside with leader lines", {
	big = sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0))))
	tiny = sf::st_polygon(list(rbind(c(10, 4), c(10.5, 4), c(10.5, 4.5), c(10, 4.5), c(10, 4))))
	polys = list(big, tiny)
	anchors = rbind(c(5, 5), c(10.25, 4.25))
	measure = function(txt, i) c(2, 0.5)
	bbx = c(xmin = -1, ymin = -1, xmax = 16, ymax = 11)
	for (mv in c("free", "column", "right")) {
		pl = placement_polygons(tm_placement(move = mv, move.max = 1), polys, anchors, radii = c(5, 0.25), text = c("A", "B"),
								measure = measure, bbx = bbx, line = 0.5)
		expect_equal(pl$moved, c(FALSE, TRUE))
		expect_true(all(pl$keep))
		# the moved label is outside the land, within the panel
		expect_true(pl$x[2] - 1 >= 10.5 - 1e-9 && pl$x[2] + 1 <= 16)
		expect_false(anyNA(pl$leader[2, ]))
	}
	# without "move" the label is dropped
	pl = placement_polygons(tm_placement(fallback = c("wrap", "drop")), polys, anchors, radii = c(5, 0.25),
							text = c("A", "B"), measure = measure, bbx = bbx, line = 0.5)
	expect_equal(pl$keep, c(TRUE, FALSE))
})

test_that("configurations follow the fallback steps", {
	pl = tm_placement(fallback = c("abbreviate", "wrap", "drop"))
	cfgs = placement_configs("New Hampshire", "NH", c("abbreviate", "wrap"), pl)
	txt = vapply(cfgs, `[[`, character(1), "text")
	expect_equal(txt[1:2], c("New Hampshire", "NH"))
	expect_true("New\nHampshire" %in% txt)
	# without a short text, abbreviate is skipped
	cfgs = placement_configs("New Hampshire", NA, c("abbreviate"), pl)
	expect_length(cfgs, 1)
})

test_that("point labels are placed without overlap, in order of priority", {
	anchors = rbind(c(0, 0), c(0.5, 0), c(10, 10))
	measure = function(txt, i) c(2, 0.5)
	bbx = c(xmin = -5, ymin = -5, xmax = 15, ymax = 15)
	pl = placement_points(tm_placement(fallback = c("drop")), anchors, text = c("A", "B", "C"), measure = measure,
						  bbx = bbx, line = 0.5, priority = c(1, 2, 3))
	expect_true(all(pl$keep))
	r = placement_rects(pl$x, pl$y, 2, 0.5)
	expect_false(any(placement_rects_overlap(r[1, , drop = FALSE], r[2:3, , drop = FALSE])))
	# the label with the highest priority gets the preferred position (top right)
	expect_true(pl$x[3] > 10 && pl$y[3] > 10)
})

test_that("tm_labels accepts short and priority, and deprecated options", {
	us = World[World$continent == "Africa", ]
	expect_no_error(print(tm_shape(us) + tm_polygons() + tm_labels("name", short = "iso_a3", priority = "pop_est")))
	expect_no_error(print(tm_shape(metro) + tm_dots() + tm_labels("name", priority = "pop2020")))
	expect_message(opt_tm_labels(point.label.gap = 1), "deprecated")
	expect_equal(suppressMessages(opt_tm_labels(point.label.gap = 1))$mapping.args$placement$gap, 1)
	expect_null(suppressMessages(opt_tm_labels(point.label = FALSE))$mapping.args$placement)
})
