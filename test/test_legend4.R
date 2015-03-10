data(Europe)
data(cities)
data(rivers)

(tm <- tm_shape(Europe) +
	tm_fill("part") +
tm_shape(rivers) +
	tm_lines(col="type", lwd = "scalerank") +
tm_shape(cities) +
	tm_bubbles(size = "pop_max", col = "capital"))

tm + tm_layout_Europe(legend.is.portrait = TRUE)



tm_shape(Europe) +
 	tm_fill("part") +
#  	tm_shape(rivers) +
#  	tm_lines(col="type", lwd = "scalerank") +
 	tm_shape(cities) +
 	tm_bubbles(size = "pop_max", col = "capital", scale=.5) +
	tm_layout_Europe(legend.is.portrait = TRUE)
