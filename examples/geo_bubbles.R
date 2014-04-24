data(Europe)

geo_shape(Europe) + geo_borders() + geo_bubbles(size=.75, col="red") + geo_theme("Countries")

geo_shape(Europe) + geo_borders() + geo_bubbles(size=.75, col=ifelse(Europe$name=="Isle of Man", "red", "blue")) + geo_theme("Find Isle of Man")

