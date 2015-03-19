data(World)
data(cities)
data(rivers)

tm_shape(World, projection="longlat") + 
    tm_fill() +
    tm_borders() + 
    tm_layout("Long lat coordinates (WGS84)", inner.margins=c(0,0,.1,0), title.cex=.8)

World$highlighted <- ifelse(World$iso_a3 %in% c("GRL", "AUS"), "gold", "gray75")
tm_shape(World, projection="merc") + 
	tm_fill("highlighted") + 
	tm_borders() + 
	tm_layout("Mercator projection. Although used in Google Maps, it is discouraged for
statistical purposes. In reality, Australia is 3 times larger than Greenland!", 
			  inner.margins=c(0,0,.1,0), title.cex=.6)

tm_shape(World, projection="wintri") + 
    tm_fill() + 
    tm_borders() + 
    tm_layout(
"Winkel-Tripel projection, adapted as default by the National Geographic Society for world maps.", 
inner.margins=c(0,0,.1,0), title.cex=.8)

tm_shape(World) +
    tm_fill() + 
    tm_borders() + 
tm_layout("Eckhart IV projection. Recommended in statistical maps for its equal-area property.", 
    inner.margins=c(0,0,.1,0), title.cex=.8)

# three groups of layers, each starting with tm_shape
tm_shape(World) +
	tm_fill("darkolivegreen3") +
tm_shape(cities) +
	tm_bubbles("pop_max", col = "grey50", scale=.5) +
tm_shape(rivers) +
	tm_lines("lightcyan3") +
tm_layout_World(title = "", bg.color="lightcyan3", legend.show = FALSE)
