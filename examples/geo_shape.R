data(World)

geo_shape(World) + geo_fill() + geo_borders() + geo_theme("Default Eckhart IV projection", inner.margins=c(0,0,.1,0))

geo_shape(World, projection="wintri") + geo_fill() + geo_borders() + geo_theme("Winkel-Tripel projection", inner.margins=c(0,0,.1,0))

geo_shape(World, projection="longlat") + geo_fill() + geo_borders() + geo_theme("Long lat coordinates", inner.margins=c(0,0,.1,0))
