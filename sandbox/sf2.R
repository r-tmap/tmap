library(osmdata)

getbb("Amsterdam")

library(tmaptools)

bb("Amsterdam")

x <- osm_objects(key='building', getbb("Maastricht"))


library(ggplot2)
library(sf)
data(World)

World <- as(World, "sf")

qtm(World) + tm_grid(projection = "longlat")

ggplot(World) + geom_sf()

data(NLD_muni)
NLD_muni <- as(NLD_muni, "sf")

ggplot(NLD_muni) + geom_sf(aes(fill=population))
?geom_sf

library(maps)
world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
ggplot() + geom_sf(data = world1)

world2 <- sf::st_transform(
	world1,
	"+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs"
)
ggplot() + geom_sf(data = world2)

qtm(world2) + tm_grid(projection = "longlat")

library(tmaptools)
bb(world2)

