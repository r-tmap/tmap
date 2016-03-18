data(World, Europe, rivers)

qtm(rivers)

\dontrun{
tm_shape(Europe) +
    tm_fill() +
tm_shape(rivers) +
    tm_lines(col="black", lwd="scalerank", scale=2, legend.lwd.show = FALSE) +
tm_layout("Rivers of Europe") +
tm_style_cobalt()
}
	
### Demo to visualise the route of the Amstel Gold Race, a professional cycling race
\dontrun{

tmpdir <- tempdir()
tmpfile <- tempfile()
download.file("http://www.gpstracks.nl/routes-fiets/f-limburg-amstel-gold-race-2014.zip", 
			  tmpfile, mode="wb")
unzip(tmpfile, exdir=tmpdir)

# read GPX file
AGR <- read_GPX(file.path(tmpdir, "f-limburg-amstel-gold-race-2014.gpx"))

# read OSM of Zuid-Limburg
Limburg_OSM <- read_osm(AGR$tracks, ext=1.05)

# change route part names
levels(AGR$tracks$name) <- paste(c("First", "Second", "Third", "Final"), "loop")
AGR$tracks_offset2 <- offset_line(AGR$tracks, offset=c(.0005,0,-.0005,-.001))

tm_shape(Limburg_OSM) + 
	tm_raster(saturation=.25) +
	tm_shape(AGR$tracks_offset2) +
	tm_lines(col = "name", lwd = 4, title.col="Amstel Gold Race", palette="Dark2") + 
	tm_shape(AGR$waypoints) +
	tm_bubbles(size=.1, col="gold", border.col = "black") + 
	tm_text("name", size = .75, bg.color="white", bg.alpha=.25, auto.placement = .25) +
	tm_legend(position=c("right", "top"), frame=TRUE, bg.color = "gold") +
	tm_view(basemaps = "Esri.WorldTopoMap")
}
# TIP: check out these examples in view mode, enabled with tmap_mode("view")
