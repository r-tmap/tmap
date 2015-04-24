data(Europe)
data(metro)
data(rivers)

(tm <- tm_shape(Europe) +
	tm_fill("part") +
tm_shape(rivers) +
	tm_lines(col="type", lwd = "scalerank") +
tm_shape(cities) +
	tm_bubbles(size = "pop_max", col = "capital"))

tm + tm_layout_Europe(legend.is.portrait = TRUE)


metro$growth <- (metro$X2020 - metro$X2010) / (metro$X2010 * 10) * 100
tm_shape(Europe) +
 	tm_fill("part") +
  	tm_shape(rivers) +
  	tm_lines(col="type", lwd = "scalerank") +
 	tm_shape(metro) +
 	tm_bubbles(size = "X2010", col = "growth", scale=1, n=10) +
	tm_layout(legend.is.portrait = TRUE, legend.hist.show = TRUE, legend.bg.color="white")
