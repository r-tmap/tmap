### airports


download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_airports.zip", "../shapes/ne_10m_airports.zip")
unzip("../shapes/ne_10m_airports.zip", exdir="../shapes")
airports10 <- get_shape("../shapes/ne_10m_airports.shp")

names(airports10)

#airports10$size <- 10 - as.numeric(as.character(airports10$scalerank))

airports <- airports10
airports@data <- airports@data[, c("iata_code", "name", "scalerank", "natlscale")]

airports$name <- as.character(airports$name)

x <- grep("I_WAS_NOT_ASCII", iconv(airports$name, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))

airports$name[x] <- c("Rosario - Islas Malvinas Int", "Martin Miguel De Guemes Int", "Cayenne-Rochambeau", "Foz do Iguacu")



save(airports, file="./data/airports.rda", compress="xz")

### cities
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_populated_places_simple.zip", "../shapes/ne_50m_populated_places_simple.zip")
unzip("../shapes/ne_50m_populated_places_simple.zip", exdir="../shapes")
cities50 <- get_shape("../shapes/ne_50m_populated_places_simple.shp")

cities <- cities50
cities@data <- cities@data[, (c("nameascii", "adm0name", "adm0_a3",  "featurecla", "pop_max", "pop_min"))]

cities$capital <- cities$featurecla == "Admin-0 capital"

cities@data <- cities@data[, c("nameascii", "adm0name", "adm0_a3",  "capital", "pop_max", "pop_min")]
names(cities) <- c("name", "country", "iso_a3", "capital", "pop_max", "pop_min")

cities <- cities[cities$pop_max>=1e6 | cities$capital, ]

cities$name <- factor(as.character(cities$name), levels=intersect(levels(cities$name), as.character(cities$name)))

x <- grep("I_WAS_NOT_ASCII", iconv(levels(cities$name), "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
levels(cities$name)[x] <- "Montreal"


x <- grep("I_WAS_NOT_ASCII", iconv(levels(cities$name), "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))


cities$country <- factor(as.character(cities$country), levels=intersect(levels(cities$country), as.character(cities$country)))
x <- grep("I_WAS_NOT_ASCII", iconv(levels(cities$country), "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))



cities$iso_a3 <- factor(as.character(cities$iso_a3), levels=intersect(levels(cities$iso_a3), as.character(cities$iso_a3)))


save(cities, file="./data/cities.rda", compress="xz")

# data(Europe)
# data(World)
# 
# geo_shape(World) +
# 	geo_borders() +
# 	geo_shape(cities) +
# 	geo_bubbles(size="pop_max", col="capital") +
# 	geo_text("name", cex="pop_max", scale=5) +
# 	geo_theme_World()

geo_shape(Europe) +
	geo_borders() +
	geo_fill() +
	geo_shape(cities) +
	geo_text("name", cex="pop_max", scale=2, root=3, ymod=-.015, bg.alpha=0) +
	geo_bubbles(size="pop_max", col="capital", size.lim=c(0, 2e7)) +
	geo_theme_Europe("Metropolitan population", legend.titles=c(bubble.col="Capital"))
