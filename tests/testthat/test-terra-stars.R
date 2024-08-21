
test_that("terra works", {
	
	skip_on_cran()
	skip_if_not_installed("spDataLarge")
	landsat = terra::rast(system.file("raster/landsat.tif", package = "spDataLarge"))
	# Probably a bug in terra?
	# names become cover_cover, cover_cls_cover_cls
	land_terra = terra::rast(tmap::land)
	names(land_terra) <-  names(tmap::land)
	tm_shape(land_terra) + tm_raster("trees")
	
	expect_snapshot(error = TRUE, {
		tm_shape(land_terra) + tm_raster("treess")
	})
	
	# FIXME this is not expected.
	
	tm_shape(landsat) +
		tm_rgb(tm_vars(c("landsat_4", "landsat_3", "landsat_2"), multivariate = TRUE), col.scale = tm_scale_rgb(maxColorValue = 31961))
	# Defaults of stars and terra are identical.
	tm_shape(landsat) + tm_raster(col.free = FALSE)
	tm_shape(landsat) +
		tm_raster(c("landsat_1", "landsat_2", "landsat_3", "landsat_4"), col.free = FALSE) + 
		tm_options(max.raster = 10000)
	
	skip_on_os("linux")
	tm_shape(landsat) + tm_raster("landsat_1", col.free = FALSE)
	
})

test_that("stars works", {
	
	skip_on_cran()
	skip_if_not_installed("spDataLarge")
	landsat = stars::read_stars(system.file("raster/landsat.tif", package = "spDataLarge"))
	land_stars = tmap::land
	
	tm_shape(land_stars) + tm_raster("trees")
	
	tm_shape(land_stars) + tm_raster("treess")
		
	# Fixed (issue #789)
	# Removed the argument raster.warp from v4, because it is not useful. 
	# Now the first strategy is warp (st_warp) and if unsuccessful,
	# then throw a warning and try a (slow) transformation (st_transform).
	# Somehow, the warp won't work with "robin". However, with crs = "+proj=eck4" is does work.
	expect_no_warning(tm_shape(landsat) + tm_raster("landsat.tif", col.free = FALSE))
	
	p <- tm_shape(landsat) + tm_raster(col.free = FALSE)
	tm_shape(landsat) +
		tm_raster("landsat.tif") +
		tm_facets("band") + tm_options(max.raster = 10000)
})





test_that("multi rast works.", {
	skip_on_cran()
	skip_if_not_installed("spDataLarge")
	multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
	multi_rast = terra::rast(multi_raster_file)
	expect_no_error({
	tm_shape(multi_rast[[3:1]]) +
			tm_rgb()
	})
	
	tm_shape(multi_rast) +
		tm_rgb(tm_vars(c("landsat_4", "landsat_3", "landsat_2"), multivariate = TRUE), col.scale = tm_scale_rgb(maxColorValue = 31961)) 
	
})

test_that("Both approaches work for stars.", {
	skip_on_cran()
	skip_if_not_installed("spDataLarge")
	# idea: tm_attr to specify an attribute as mv
	# direct approach
	landsat_stars = stars::read_stars(system.file("raster/landsat.tif", package = "spDataLarge"))
	expect_no_condition(tm_shape(landsat_stars) +
		tm_rgb(tm_vars("band", dimvalues = c(4,3,2)), col.scale = tm_scale_rgb(maxColorValue = 31961))
	)
	
	# indirect approach
	landsat_stars2 = split(landsat_stars, "band")
	tm_shape(landsat_stars2) +
		tm_rgb(tm_vars(c("X4", "X3", "X2"), multivariate = TRUE), col.scale = tm_scale_rgb(maxColorValue = 31961))
})
