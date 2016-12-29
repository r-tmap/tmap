library(tmap)
data(Europe)

tm_shape(Europe) +
	tm_polygons()

Europe2 <- spTransform(Europe, CRS("+proj=omerc +lat_0=42 +gamma=0 +lonc=10 +alpha=-90 +k_0=1 +x_0=0 +y_0=0"))

plot(Europe2) # works

tm_shape(Europe2, unit="m", unit.size=1) +
	tm_polygons() 
