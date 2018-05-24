context("tm_sf")

test_that("tm_sf (polygons)", {
	data(World)
	
	expect_silent({
		tm <- tm_shape(World) + tm_sf()
	})
	
	expect_silent(print(tm))
	ttm(); expect_silent(print(tm))
})

test_that("tm_sf (lines)", {
	data(rivers)
	
	expect_silent({
		tm <- tm_shape(rivers) + tm_sf()
	})
	
	expect_silent(print(tm))
	ttm(); expect_silent(print(tm))
})

test_that("tm_sf (points)", {
	data(metro)
	
	expect_silent({
		tm <- tm_shape(metro) + tm_sf()
	})
	
	expect_silent(print(tm))
	ttm(); expect_silent(print(tm))
})

test_that("tm_sf (collection)", {
	data(World)
	
	World <- as(World, "sf")
	World$geometry[World$continent == "Africa"] <- 
		sf::st_centroid(World$geometry[World$continent == "Africa"])
	World$geometry[World$continent == "South America"] <- 
		sf::st_cast(World$geometry[World$continent == "South America"], 
					"MULTILINESTRING", group_or_split = FALSE)
	
	expect_silent({
		tm <- tm_shape(World) + tm_sf()
	})
	
	expect_silent(print(tm))
	ttm(); expect_silent(print(tm))
})


