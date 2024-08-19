skip_on_cran()
World2 <- World
World2$pop_class = cut(World2$pop_est, breaks = c(0, 10, 100, 1000, Inf) * 1e6, labels = c("Small", "Medium", "Large", "Extra Large"))					   
World2$HPI_class = cut(World2$HPI, breaks = seq(10, 50, by = 10))
World2$well_being_class = cut(World2$well_being, breaks = seq(2, 8, by = 2))
World2$footprint_class = cut(World2$footprint, breaks = seq(0, 16, by = 4))

metro$pop2020_class = cut(metro$pop2020, breaks = c(.5, 1.5, 2.5, 5, 15, 40) * 1e6)

Africa = World2[World2$continent == "Africa", ]


test_that("Base layer works at different positions", {
	skip_if_not_installed("maptiles")
	t <- tm_basemap("OpenStreetMap") +
		tm_shape(Africa) +
		tm_polygons("HPI", fill.scale = tm_scale(values = "viridis")) +
		tm_symbols(size = "pop_est", fill = "purple", size.scale = tm_scale(values = tm_seq(0, 2, "sqrt"))) +
		tm_facets_wrap("well_being_class") +
		tm_shape(metro) +
		tm_symbols(fill = "pop2020") +
		tm_layout(bg.color = "grey95")
	expect_s3_class(t, "tmap")
	expect_equal(
		attr(t[[1]], "class"),
		c("tm_basemap", "tm_aux_layer", "tm_element", "list")
	)
	tm_shape(Africa) +
		tm_polygons("HPI", fill.scale = tm_scale(values = "viridis")) +
		tm_basemap("OpenStreetMap")+
		tm_symbols(size = "pop_est", fill = "purple", size.scale = tm_scale(values = tm_seq(0, 2, "sqrt"))) +
		tm_facets_wrap("well_being_class") +
		tm_shape(metro) +
		tm_symbols(fill = "pop2020") +
		tm_layout(bg.color = "grey95")
	
	tm_shape(Africa) +
		tm_polygons("HPI", fill.scale = tm_scale(values = "viridis")) +
		tm_symbols(size = "pop_est", fill = "purple", size.scale = tm_scale(values = tm_seq(0, 2, "sqrt"))) +
		tm_facets_wrap("well_being_class") +
		tm_shape(metro) +
		tm_basemap("OpenStreetMap")+
		tm_symbols(fill = "pop2020") +
		tm_layout(bg.color = "grey95")
	
	tm_shape(Africa) +
		tm_polygons("HPI", fill.scale = tm_scale(values = "viridis")) +
		tm_symbols(size = "pop_est", fill = "purple", size.scale = tm_scale(values = tm_seq(0, 2, "sqrt"))) +
		tm_facets_wrap("well_being_class") +
		tm_shape(metro) +
		tm_symbols(fill = "pop2020") +
		tm_basemap("OpenStreetMap")+
		tm_layout(bg.color = "grey95")
	
	tm_shape(Africa) + tm_basemap("OpenStreetMap") +
		tm_polygons("pop_est_dens") +
		tm_facets_grid("footprint_class", "HPI_class")
	
	tm_shape(Africa) +
		tm_basemap("Esri.NatGeoWorldMap") +
		tm_borders(lwd = 2) +
		tm_shape(metro) +
		tm_symbols(fill = "pop2020_class", col = "black")
})

test_that("Projected CRS warp work", {
	t <- tm_shape(NLD_prov) +
		tm_basemap("OpenStreetMap") +
		tm_borders()
	expect_s3_class(t, "tmap")
})


test_that("Reproject shape to long-lat works.", {
	expect_no_condition(tm_shape(NLD_prov, crs = 4326) +
		tm_basemap("OpenStreetMap") +
		tm_borders(lwd = 4))
	
	
	tm_shape(NLD_muni, crs = 4326) +
		tm_basemap("OpenStreetMap") +
		tm_polygons(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"), fill.free = FALSE, fill_alpha = 0.5)
})

test_that("tm_graticules(labels.show = FALSE) doesn't show labels. (#795)", {
	no_lab <- tm_shape(World) +
		tm_fill() +
		tm_graticules(labels.show = FALSE)
	lab <- tm_shape(World) +
		tm_fill() +
		tm_graticules(labels.show = TRUE)
	expect_false(identical(lab, no_lab))
})
