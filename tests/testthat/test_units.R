context("units")

data(NLD_prov)
NLD_prov$area <- tmaptools::approx_areas(NLD_prov)
NLD_prov$area2 <- sf::st_area(NLD_prov)

NLD_prov_lines <- NLD_prov
NLD_prov_lines$geometry <- sf::st_cast(NLD_prov_lines$geometry, "MULTILINESTRING", group_or_split = FALSE)
	
NLD_prov_lines$length_m <- sf::st_length(NLD_prov_lines)
NLD_prov_lines$length_km <- set_units(sf::st_length(NLD_prov_lines), km)

test_that("fill with units (single)", {
	tmap_mode("plot")
	expect_silent({
		print({
			tm_shape(NLD_prov) +
				tm_polygons(col = "area")
		})
	})
})

test_that("fill with units (facets, free scale", {
	tmap_mode("plot")
	expect_silent({
		print({
			tm_shape(NLD_prov) +
				tm_polygons(col = c("population", "area"))
		})
	})
})

test_that("fill with units (facets, !free scale", {
	tmap_mode("plot")
	expect_silent({
		print({
			tm_shape(NLD_prov) +
				tm_polygons(col = c("area2", "area")) +
			tm_facets(free.scales = FALSE)
		})
	})
})

test_that("bubble size with units (single)", {
	tmap_mode("plot")
	expect_silent({
		print({
			tm_shape(NLD_prov) +
				tm_bubbles(size = "area")
		})
	})
})

test_that("bubble size with units (facets, free scale", {
	tmap_mode("plot")
	expect_silent({
		print({
			tm_shape(NLD_prov) +
				tm_bubbles(size = c("population", "area"))
		})
	})
})

test_that("bubble size with units (facets, !free scale", {
	tmap_mode("plot")
	expect_silent({
		print({
			tm_shape(NLD_prov) +
				tm_bubbles(size = c("area2", "area")) +
			tm_facets(free.scales = FALSE)
		})
	})
})

test_that("line with units (single)", {
	tmap_mode("plot")
	expect_silent({
		print({
			tm_shape(NLD_prov_lines) +
				tm_lines(lwd = "length_m")
		})
	})
})

test_that("line with units (facets, free scale", {
	tmap_mode("plot")
	expect_silent({
		print({
			tm_shape(NLD_prov_lines) +
				tm_lines(lwd = c("length_km", "length_m"))
		})
	})
})

test_that("line with units (facets, !free scale", {
	tmap_mode("plot")
	expect_silent({
		print({
			tm_shape(NLD_prov_lines) +
				tm_lines(lwd = c("length_km", "length_m")) +
				tm_facets(free.scales = FALSE)
		})
	})
})
