library(tmaptools)
library(mapview)
brt <- read_shape("../tmapPresentations/data/buurt_2014.shp")


brtsel <- brt[1:5000,]


shp <- brtsel

system.time({
	sum(sapply(shp@polygons, function(p)sum(sapply(p@Polygons, function(pp)nrow(pp@coords)))))
})


qtm(brtsel)

library(profvis)

ttm()

p <- profvis({
	print(tm_shape(brt) +
		tm_fill("P_45_64_JR"))
})
