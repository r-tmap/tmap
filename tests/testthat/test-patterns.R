# Opus5.5
test_that("tm_patterns creates normalized pattern specifications", {
	p = tm_patterns("hatch", angle = c(0, 45))
	expect_s3_class(p, "tmap_patterns")
	expect_equal(unclass(p), c("hatch(angle=0)", "hatch(angle=45)"))

	# shortcuts and defaults
	expect_equal(unclass(tm_patterns("hatch_down")), "hatch(angle=135)")
	expect_equal(unclass(tm_patterns("stipple")), "dots(random=0.7)")
	expect_equal(unclass(tm_patterns("none")), "none")

	# sets are expanded, and can be subsetted
	expect_length(tm_patterns("mixed"), 14)
	expect_equal(unclass(tm_patterns("classic")[1]), "hatch(angle=45)")

	expect_error(tm_patterns("foo"), "Unknown pattern type")
	expect_warning(tm_patterns("zigzag", random = 1), "does not use")
})

# Opus5.5
test_that("pattern values are normalized from all three forms", {
	a = pattern_norm(c("hatch", "bricks"))
	b = pattern_norm(list(tm_patterns("hatch"), tm_patterns("bricks")))
	c = pattern_norm(tm_patterns(c("hatch", "bricks")))
	expect_equal(a, b)
	expect_equal(a, c)
	expect_true(pattern_valid("dots"))
	expect_false(pattern_valid("zigzog"))
	expect_false(pattern_valid(1))

	expect_equal(pattern_set("mixed", 3), c("hatch", "dots", "hatch_down"))
	expect_equal(pattern_set("-classic", 1), "dashes")
	expect_warning(pattern_set("shapes", 7), "recycled")
})

# Opus5.5
test_that("tile geometry is clipped and does not draw parts twice", {
	g = pattern_geom("hexagons", 0.1)
	segs = pattern_tile_segs(g)
	expect_true(all(segs >= -1e-9))
	expect_true(all(segs[, c("x0", "x1")] <= g$w + 1e-9))
	expect_true(all(segs[, c("y0", "y1")] <= g$h + 1e-9))
	expect_false(any(duplicated(round(segs, 8))))

	expect_null(pattern_geom("none", 0.1))
	expect_true(is.na(pattern_spacing(0.01)))
	expect_equal(pattern_spacing(1, scale = 2), 0.25 * 0.2 * 2)
})

# Opus5.5
test_that("patterns are drawn in plot mode", {
	skip_on_cran()
	pdf(NULL)
	on.exit(dev.off())

	w = tm_shape(World)
	expect_no_error(print(w + tm_polygons("HPI", pattern = "hatch")))
	expect_no_error(print(w + tm_polygons("HPI", pattern = "economy", pattern_col = "auto")))
	expect_no_error(print(w + tm_polygons("HPI", pattern = "dots", pattern_density = "footprint")))
	expect_no_error(print(w + tm_polygons("HPI", pattern = "economy",
		pattern.legend = tm_legend(orientation = "landscape"))))
	expect_no_error(print(w + tm_polygons(c("HPI", "footprint"), pattern = "stipple") +
		tm_facets_wrap()))

	# grob: one pattern grob per unique pattern, between fill and borders
	g = tmap_grob(tm_shape(World) + tm_polygons(pattern = "economy"))
	expect_s3_class(g, "gTree")
})

# Opus5.5
test_that("pattern messages", {
	skip_on_cran()
	pdf(NULL)
	on.exit(dev.off())
	w = tm_shape(World)
	expect_message(print(w + tm_polygons("HPI", pattern_density = "footprint")), "has no effect")
	# missing values (value.na "none") get the same pattern as the first class
	s = tm_scale_intervals(n = 3, values = c("none", "hatch", "dots", "bricks"))
	expect_message(print(w + tm_polygons(pattern = "HPI", pattern.scale = s)), "missing values")
	expect_error(print(w + tm_polygons(pattern = "HPI", pattern.scale = tm_scale_continuous())))
})

# Opus5.5
test_that("patterns are drawn in view mode", {
	skip_on_cran()
	tmap_mode("view")
	on.exit(tmap_mode("plot"))
	lf = tmap_leaflet(tm_shape(World) + tm_polygons("HPI", pattern = "economy"))
	expect_s3_class(lf, "leaflet")
	js = vapply(lf$jsHooks$render, function(h) h$code, character(1))
	expect_true(any(grepl("tmap-pattern-defs", js)))

	lf = tmap_leaflet(tm_shape(World) + tm_polygons("HPI", pattern = "dots",
		options = opt_tm_polygons(pattern.zoom = TRUE)))
	data = lapply(lf$jsHooks$render, function(h) h$data)
	expect_true(any(vapply(data, function(d) length(d$zoom) > 0, logical(1))))
	# the legend swatches zoom as well
	lf = tmap_leaflet(tm_shape(World) + tm_polygons(pattern = "economy",
		options = opt_tm_polygons(pattern.zoom = TRUE)))
	data = lapply(lf$jsHooks$render, function(h) h$data)
	expect_true(any(vapply(data, function(d) isTRUE(grepl("tmap-pattern-legend", d$cls)), logical(1))))
})

# Opus5.5
test_that("tmap_show_values draws the available values", {
	pdf(NULL)
	on.exit(dev.off())
	expect_equal(tmap_show_values("pattern", values = "classic"), pattern_set("classic"))
	expect_length(tmap_show_values("pattern"), length(.pattern_types) + length(.pattern_shortcuts))
	expect_equal(tmap_show_values("pattern", values = tm_patterns("hatch", angle = c(0, 30))),
				 c("hatch(angle=0)", "hatch(angle=30)"))
	expect_equal(tmap_show_values("shape"), 0:25)
	expect_no_error(tmap_show_values("lty", values = c("solid", "44", "1343")))
	expect_no_error(tmap_show_values("lty", values = 1:6))
	expect_no_error(tmap_show_values("fontface"))
	sets = tmap_show_values("pattern", values = c("classic", "shapes"))
	expect_equal(names(sets), c("classic", "shapes"))
	expect_equal(sets$shapes, pattern_set("shapes"))
	expect_equal(tmap_show_values("lty", values = 1:3), 1:3)
	expect_error(tmap_show_values("pattern", values = "zigzog"))
	expect_error(tmap_show_values("shape", values = "a"))
})

# Opus5.5
test_that("dot value of dot density legends", {
	skip_on_cran()
	pdf(NULL)
	on.exit(dev.off())

	utrecht = NLD_dist[NLD_dist$province == "Utrecht", ]
	utrecht$pop_dens = utrecht$population / utrecht$area # units: 1/km^2
	dotmap = function(legend, scale = tm_scale_continuous_area(limits = c(0, 10000)), pattern = "stipple",
					  data = utrecht) {
		tm_shape(data) +
			tm_polygons(pattern = pattern, pattern_density = "pop_dens", pattern_density.scale = scale,
						pattern_density.legend = legend)
	}
	grob_texts = function(m) {
		g = tmap_grob(m)
		labs = character()
		walk = function(x) {
			if (inherits(x, "text")) labs <<- c(labs, x$label)
			if (inherits(x, "gTree")) for (ch in x$children) walk(ch)
		}
		walk(g)
		labs
	}

	# K = v_max / d_max^2 (per km2)
	o = tmap_options_mode("plot", mode.specific = FALSE)
	leg = modifyList(tm_legend(dot.value = TRUE), list(reverse = FALSE, na.show = NA))
	r = tmapScaleContinuous(utrecht$pop_dens, tm_scale_continuous_area(limits = c(0, 10000)), leg, tm_chart_none(),
							o, "pattern_density", "polygons", list(), sortRev = NULL, bypass_ord = FALSE,
							submit_legend = FALSE)
	expect_equal(r$legend$dot_info$K, 10000)
	expect_equal(r$legend$dot_info$text, "1 dot ≈ {n}")
	# the data per hectare: 100 times as many units per km2
	leg$dot.unit = "ha"
	r = tmapScaleContinuous(as.numeric(utrecht$pop_dens), tm_scale_continuous_area(limits = c(0, 100)), leg,
							tm_chart_none(), o, "pattern_density", "polygons", list(), sortRev = NULL,
							bypass_ord = FALSE, submit_legend = FALSE)
	expect_equal(r$legend$dot_info$K, 10000)

	# the note is drawn below the legend items
	txt = grob_texts(dotmap(tm_legend(dot.value = "1 dot ≈ {n} inhabitants")))
	expect_true(any(grepl("^1 dot ≈ [0-9,]+ inhabitants$", txt)))
	txt = grob_texts(dotmap(tm_legend(dot.value = TRUE, orientation = "landscape")))
	expect_true(any(grepl("^1 dot ≈ [0-9,]+$", txt)))

	# not proportional: no note, but a message
	expect_message(txt <- grob_texts(dotmap(tm_legend(dot.value = TRUE), scale = tm_scale_continuous())),
				   "not proportional")
	expect_false(any(grepl("1 dot", txt)))
	# not a dot pattern
	expect_message(txt <- grob_texts(dotmap(tm_legend(dot.value = TRUE), pattern = "hatch")), "dot pattern")
	expect_false(any(grepl("1 dot", txt)))

	# view mode: the note is added with JavaScript
	tmap_mode("view")
	on.exit(tmap_mode("plot"), add = TRUE)
	lf = tmap_leaflet(dotmap(tm_legend(dot.value = TRUE)))
	js = vapply(lf$jsHooks$render, function(h) h$code, character(1))
	expect_true(any(grepl("tmap-dot-note", js)))
})

# Opus5.5
test_that("cross legends", {
	skip_on_cran()
	pdf(NULL)
	on.exit(dev.off())

	W = World[!(World$continent %in% c("Antarctica", "Seven seas (open ocean)")), ]
	W$continent = droplevels(W$continent)
	m = tm_shape(W) +
		tm_polygons(fill = "continent", pattern = "income_grp", pattern.legend = tm_legend_cross("fill"))

	labs = character()
	walk = function(x) {
		if (inherits(x, "text")) labs <<- c(labs, x$label)
		if (inherits(x, "gTree")) for (ch in x$children) walk(ch)
	}
	walk(tmap_grob(m))
	# row labels (income groups), column labels (continents), and the axis labels
	expect_true(all(levels(W$income_grp) %in% labs))
	expect_true(all(levels(W$continent) %in% labs))
	expect_true(all(c("income_grp", "continent") %in% labs))

	expect_no_error(print(tm_shape(W) + tm_polygons(fill = "HPI", pattern = "economy",
		pattern.legend = tm_legend_cross("fill", title = "HPI by economy", xlab = "HPI", ylab = "Economy"))))

	# not possible: messages, and separate legends
	expect_message(print(tm_shape(W) + tm_polygons(fill = "HPI", fill.scale = tm_scale_continuous(),
		pattern = "economy", pattern.legend = tm_legend_cross("fill"))), "continuous")
	expect_message(print(tm_shape(W) + tm_polygons(pattern = "economy",
		pattern.legend = tm_legend_cross("fill"))), "not crossed")

	# view mode: an html table
	tmap_mode("view")
	on.exit(tmap_mode("plot"), add = TRUE)
	expect_no_error(lf <- tmap_leaflet(m))
	html = unlist(lapply(lf$x$calls, function(cl) if (cl$method == "addControl") cl$args[[1]]))
	expect_true(any(grepl("<table", html, fixed = TRUE)))
})

# Opus5.5
test_that("seed of random dot patterns", {
	expect_equal(unclass(tm_patterns("stipple", seed = 1)), "dots(random=0.7)")
	expect_equal(unclass(tm_patterns("stipple", seed = 2)), "dots(random=0.7,seed=2)")
	g1 = pattern_geom("dots(random=0.7)", 0.1)
	g2 = pattern_geom("dots(random=0.7,seed=2)", 0.1)
	expect_equal(nrow(g1$circles), nrow(g2$circles))
	expect_false(isTRUE(all.equal(g1$circles$x, g2$circles$x)))
	expect_warning(tm_patterns("hatch", seed = 2), "does not use")
})

# Opus5.5
test_that("patterns are clipped geometrically on devices without clipping paths (#1135)", {
	skip_on_cran()
	pdf(NULL)
	on.exit(dev.off())
	op = options(tmap.pattern.clip = "geometry")
	on.exit(options(op), add = TRUE)
	expect_false(pattern_clip_paths())
	expect_no_error(print(tm_shape(World) + tm_polygons(pattern = "economy")))
	expect_no_error(print(tm_shape(World) + tm_polygons(pattern = "dots", pattern_density = "HPI")))

	# the segments of a hatch pattern lie within the polygon
	sq = sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))))
	grid::grid.newpage()
	grid::pushViewport(grid::viewport(xscale = c(-1, 2), yscale = c(-1, 2), width = grid::unit(3, "in"),
									  height = grid::unit(3, "in")))
	g = pattern_geom("hatch", 0.1)
	pr = pattern_area_prims(g, matrix(c(0, 0, 3, 3), ncol = 4))
	pr2 = pattern_clip_geometry(pr, sq)
	x = grid::convertX(grid::unit(c(0, 1), "native"), "inch", valueOnly = TRUE)
	expect_true(all(pr2$segs[, c("x0", "x1")] >= x[1] - 1e-6 & pr2$segs[, c("x0", "x1")] <= x[2] + 1e-6))
	expect_lt(nrow(pr2$segs), nrow(pr$segs))
})
