data(World)

World$geometry[World$continent == "Africa"] <- 
  sf::st_centroid(World$geometry[World$continent == "Africa"])
World$geometry[World$continent == "South America"] <- 
  sf::st_cast(World$geometry[World$continent == "South America"], 
    "MULTILINESTRING", group_or_split = FALSE)

tm_shape(World, crs = "+proj=robin") + 
	tm_sf()
