current.mode <- tmap_mode("plot")

data(World, metro, rivers)

tm_shape(World, projection="longlat") + 
    tm_polygons() + 
tm_layout("Long lat coordinates (WGS84)", inner.margins=c(0,0,.1,0), title.size=.8)

World$highlighted <- ifelse(World$iso_a3 %in% c("GRL", "AUS"), "gold", "gray75")
tm_shape(World, projection="merc", ylim=c(.1, 1), relative = TRUE) + 
    tm_polygons("highlighted") + 
tm_layout("Web Mercator projection. Although widely used, it is discouraged for
statistical purposes. In reality, Australia is 3 times larger than Greenland!",
    inner.margins=c(0,0,.1,0), title.size=.6)

tm_shape(World, projection="wintri") + 
    tm_polygons() +
tm_layout(
"Winkel-Tripel projection, adapted as default by the National Geographic Society for world maps.",
    inner.margins=c(0,0,.1,0), title.size=.8)

tm_shape(World) +
    tm_polygons() + 
tm_layout("Eckhart IV projection. Recommended in statistical maps for its equal-area property.",
    inner.margins=c(0,0,.1,0), title.size=.8)

# three groups of layers, each starting with tm_shape
\dontrun{
tm_shape(World) +
    tm_fill("darkolivegreen3") +
tm_shape(metro) +
    tm_bubbles("pop2010", col = "grey30", scale=.5) +
tm_shape(rivers) +
    tm_lines("lightcyan1") +
tm_layout(bg.color="lightcyan1", inner.margins=c(0,0,.02,0), legend.show = FALSE)
}

# restore current mode
tmap_mode(current.mode)
