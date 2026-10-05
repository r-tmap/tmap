test_that("tm_title works (#796)", {
	skip_on_cran()
	a_line <- matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE) %>%
		sf::st_linestring() %>%
		sf::st_sfc() %>%
		sf::st_sf(crs = 4326)
	
	expect_no_warning({
		tm_shape(a_line) +
			tm_lines() +
			tm_title(text = "A line")
	})
})

# Opus5.5
test_that("inset map of a full globe in an orthographic crs works (#1265)", {
	skip_on_cran()
	globe = tm_shape(World) +
		tm_polygons() +
		tm_crs(bbox = "FULL", "+proj=ortho +lat_0=30 +lon_0=0") +
		tm_layout(earth_boundary = TRUE)
	Africa = World[World$continent == "Africa", ]

	pdf(NULL)
	on.exit(dev.off())
	expect_no_error(print(tm_shape(World) +
		tm_fill(fill = "gray85") +
		tm_shape(Africa, is.main = TRUE) +
		tm_polygons(fill = "well_being") +
		tm_inset(globe, width = 10, height = 10)))
})

# Opus5.5
test_that("inset maps do not affect the layout of other components (#1264)", {
	skip_on_cran()
	pdf(NULL)
	on.exit(dev.off())
	expect_no_message(print(tm_shape(NLD_muni) +
		tm_polygons() +
		tm_inset(position = c("left", "top"), height = 4, width = 8) +
		tm_inset(sf::st_bbox(NLD_muni[1:3, ]), height = 5, width = 5, position = c("right", "bottom"))))
})
