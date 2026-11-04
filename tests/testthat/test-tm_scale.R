test_that("tm_scale() works", {
	skip_on_cran()
	expect_no_error(
		tm_world <- World |> 
		tm_shape() +
		tm_fill(
			fill = "area",
			fill.scale = tm_scale_continuous_pseudo_log()
		)
	)
	expect_no_error(tm_world)
	expect_no_error(tmap_leaflet(tm_world) |> 
		addLayersControl(
			baseGroups = "area",
			options = layersControlOptions(
				collapsed = FALSE
			)
		)
	)
	
})

test_that("tm_scale_continuous_pseudo_log() works with special words", {
	skip_on_cran()
	expect_no_error(
		tm_world <- World |> 
			tm_shape() +
			tm_fill(
				fill = "MAP_COLORS",
				fill.scale = tm_scale_continuous_pseudo_log()
			)
	)
	expect_no_error(tm_world)
	mod <- tmap_mode("view")
	expect_no_error(tm_world)
	tmap_mode(mod)
})

# Opus5.5
test_that("tm_scale_discrete() works with a single value (#1255)", {
	skip_on_cran()
	poly = sf::st_polygon(list(rbind(c(0.3, 2), c(1, 1), c(0, 0.5), c(0.3, 2))))
	x = sf::st_sf(value = 12345, geometry = sf::st_sfc(list(poly)))

	pdf(NULL)
	on.exit(dev.off())
	expect_no_error(print(tm_shape(x) +
		tm_polygons(fill = "value", fill.scale = tm_scale_discrete(values = "greens"))))
	expect_no_error(print(tm_shape(x) +
		tm_symbols(size = "value", size.scale = tm_scale_discrete())))
})

# Opus5.5
test_that("user-specified value.na overrules the na color of the palette, a style's value.na does not (#1201)", {
	skip_on_cran()
	o = tmap_options_mode(mode.specific = FALSE)
	sc = tm_scale_intervals(values = "hcl.blues3")
	pal_na = cols4all::c4a_na("hcl.blues3")

	expect_identical(get_scale_defaults(sc, o, "fill", "polygons", "num")$value.na, pal_na)

	o2 = value_list_set_user(o, list(fill = "yellow"))
	o2$value.na$fill = "yellow"
	expect_identical(get_scale_defaults(sc, o2, "fill", "polygons", "num")$value.na, "yellow")

	o3 = complete_options(list(value.na = list(fill = "grey60")), o2)
	expect_identical(get_scale_defaults(sc, o3, "fill", "polygons", "num")$value.na, pal_na)
})

test_that("tm_scale_continuous_area and _volume map proportionally", {
	x = c(0, 100, 400, 2500, 10000)
	o = tmap_options_mode("plot", mode.specific = FALSE)
	leg = modifyList(tm_legend(), list(reverse = FALSE, na.show = NA))
	vals = function(aes, s) {
		tmapScaleContinuous(x, s, leg, tm_chart_none(), o, aes, "polygons", list(), sortRev = NULL,
							bypass_ord = FALSE, submit_legend = FALSE)$vals
	}
	expect_equal(vals("size", tm_scale_continuous_area()), (x / 1e4) ^ 0.5)
	expect_equal(vals("pattern_density", tm_scale_continuous_area()), (x / 1e4) ^ 0.5)
	expect_equal(vals("size", tm_scale_continuous_area(perceptual = TRUE)), (x / 1e4) ^ 0.5716)
	expect_equal(vals("size", tm_scale_continuous_area(perceptual = 0.8)), (x / 1e4) ^ 0.625)
	expect_equal(vals("size", tm_scale_continuous_volume()), (x / 1e4) ^ (1/3))
	expect_equal(vals("size", tm_scale_continuous_volume(perceptual = TRUE)), (x / 1e4) ^ (4/9))
	expect_equal(vals("size", tm_scale_continuous_volume(values = c(0, 2))), 2 * (x / 1e4) ^ (1/3))
	expect_error(vals("fill", tm_scale_continuous_area()), "color map variable")
	expect_error(tm_scale_continuous_area(trans = "log"), "trans")
	expect_error(tm_scale_continuous_volume(perceptual = 2), "perception exponent")
})

test_that("the range of values is used by tm_scale_continuous", {
	x = c(0, 100, 400, 2500, 10000)
	o = tmap_options_mode("plot", mode.specific = FALSE)
	leg = modifyList(tm_legend(), list(reverse = FALSE, na.show = NA))
	vals = function(aes, s) {
		tmapScaleContinuous(x, s, leg, tm_chart_none(), o, aes, "polygons", list(), sortRev = NULL,
							bypass_ord = FALSE, submit_legend = FALSE)$vals
	}
	expect_equal(vals("lwd", tm_scale_continuous(values = c(0.5, 4))), 0.5 + 3.5 * x / 1e4)
	expect_equal(vals("lwd", tm_scale_continuous(values = tm_seq(0.5, 1))), 0.5 + 0.5 * x / 1e4)
	# values.range is relative: the default lwd values are 0 to 3
	expect_equal(vals("lwd", tm_scale_continuous(values.range = c(0.5, 1))), 1.5 + 1.5 * x / 1e4)
})
