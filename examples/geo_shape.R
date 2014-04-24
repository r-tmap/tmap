data(World)

geo_shape(World, projection="longlat") + geo_fill() + geo_borders() + geo_theme("Long lat coordinates (WGS84)", inner.margins=c(0,0,.1,0), title.cex=.8)

geo_shape(World, projection="merc", ylim=c(.2, 1)) + geo_fill(ifelse(World$iso_a3 %in% c("GRL", "AUS"), "gold", "gray75")) + geo_borders() + geo_theme("Mercator projection. Although used in Google Maps, it is discouraged for\nstatistical purposes. In reality, Australia is 3 times larger than Greenland!", inner.margins=c(0,0,.15,0), title.cex=.6)

geo_shape(World, projection="wintri") + geo_fill() + geo_borders() + geo_theme("Winkel-Tripel projection, adapted as default by the National Geographic Society for world maps", inner.margins=c(0,0,.1,0), title.cex=.8)

geo_shape(World) + geo_fill() + geo_borders() + geo_theme("Eckhart IV projection. Recommended in statistical maps for its equal-area property", inner.margins=c(0,0,.1,0))
