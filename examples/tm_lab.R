data(World)

qtm(World, fill="#FFF8DC", projection="longlat", inner.margins=0) +
	tm_grid(x = seq(-180, 180, by=20), y=seq(-90,90,by=10), col = "gray70") +
	tm_xlab("Longitude") +
	tm_ylab("Latitude")
