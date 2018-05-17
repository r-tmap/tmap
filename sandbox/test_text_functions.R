library(grid)

e1 <- expression(paste("Test ", n^2))
e2 <- expression(paste("Gsd ", sqrt(3)), paste("Gdf ", pi))

grid.newpage()
grid.text(e2[2])

data(NLD_muni)

## test credits
qtm(NLD_muni) + tm_credits(e1)

qtm(NLD_muni, fill=c("red", "blue")) + tm_credits(e1)

qtm(NLD_muni, fill=c("red", "blue")) + 
	tm_credits(c("red_TL", "blue_TL"), position=c("left", "top")) +
	tm_credits(c("red_BR", "blue_BR"), position=c("right", "bottom"))

qtm(NLD_muni, fill=c("red", "blue")) + 
	tm_credits(e1, position=c("left", "top")) +
	tm_credits(e2, position=c("right", "bottom"))

## test title
qtm(NLD_muni, title="test")
qtm(NLD_muni, title=e1)

qtm(NLD_muni, fill=c("red", "blue"), title=letters[1:2])
qtm(NLD_muni, fill=c("red", "blue"), title=e2)

## test legend title
tm_shape(NLD_muni) +
	tm_polygons("population", convert2density = TRUE, title="test")

tm_shape(NLD_muni) +
	tm_polygons("population", convert2density = TRUE, title=e1)

tm_shape(NLD_muni) +
	tm_polygons(c("pop_men", "pop_women"), title=e2)

tm_shape(NLD_muni) +
	tm_bubbles(size = "population", col=c("pop_men", "pop_women"), title.col=e2, title.size=e1)

## test legend items
tm_shape(NLD_muni) +
	tm_polygons("population", convert2density = TRUE, labels=letters[1:7])

e7 <- c(rep(e2,3), e1)

tm_shape(NLD_muni) +
	tm_polygons("population", convert2density = TRUE, labels=e7)


## test panel names
tm_shape(NLD_muni) +
	tm_polygons("population", convert2density = TRUE) +
	tm_facets("province", free.coords = T) +
	tm_layout(panel.labels = letters[1:12])

e12 <- as.expression(paste(letters[1:12], "^2"))

tm_shape(NLD_muni) +
	tm_polygons("population", convert2density = TRUE) +
	tm_facets("province", free.coords = T) +
	tm_layout(panel.labels = e12)

## nchar?
data(Europe)
qtm(Europe, fill="pop_est", format="Europe", fill.title="Population in km2")
qtm(Europe, fill="pop_est", format="Europe", fill.title=expression("Population in " * km2))

library(grid)
convertWidth(stringWidth("Population in km2"), "npc")
convertWidth(stringWidth(expression("Population in " * km2)), "npc")

convertWidth(stringWidth(paste(expression("Population in " * km2), " ")), "npc")


text_width_npc(c("Population in km2", ""))
text_width_npc(expression("Population in " * km2))
