download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_airports.zip", "../shapes/ne_50m_airports.zip")
unzip("../shapes/ne_50m_airports.zip", exdir="../shapes")
airports50 <- readOGR("../shapes", "ne_50m_airports")

airports50big <- airports50[airports50$scalerank==2,]

download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_airports.zip", "../shapes/ne_10m_airports.zip")
unzip("../shapes/ne_10m_airports.zip", exdir="../shapes")
airports10 <- readOGR("../shapes", "ne_10m_airports")

airports10$size <- 10 - as.numeric(as.character(airports10$scalerank))

data(World)
geo_shape(World) +
	geo_borders() +
	geo_fill() +
	geo_shape(airports10) +
	geo_bubblemap(size="size", scale=.5) +
	geo_theme_World()

data(Europe)
geo_shape(Europe) +
	geo_borders() +
	geo_fill() +
	geo_shape(airports10) +
	geo_bubblemap(col="size", size=.5, scale=.5, palette="Set1") +
	geo_theme_Europe()
