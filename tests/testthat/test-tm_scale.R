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
