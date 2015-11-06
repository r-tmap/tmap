\dontrun{
### Demo to visualise the route of the Amstel Gold Race, a professional cycling race
	
# download data
tmpdir <- tempdir()
tmpfile <- tempfile()
download.file("http://www.gpstracks.nl/routes-fiets/f-limburg-amstel-gold-race-2014.zip", 
			  tmpfile, mode="wb")
unzip(tmpfile, exdir=tmpdir)

# read GPX file
AGR <- read_GPX(file.path(tmpdir, "f-limburg-amstel-gold-race-2014.gpx"))

# read OSM of Zuid-Limburg (take bounding box and extend it by 1.05)
ZLim <- read_osm(bb(AGR$tracks, ext=1.05))

# change route part names
levels(AGR$tracks$name) <- paste(c("First", "Second", "Third", "Final"), "loop")

# plot it
tm_shape(ZLim) + 
  tm_raster(saturation=.25) +
tm_shape(AGR$tracks) +
	tm_lines(col = "name", lwd = 4, title.col="Amstel Gold Race", palette="Dark2") + 
tm_shape(AGR$waypoints) +
	tm_bubbles(size=.1, col="gold", border.col = "black") + 
tm_text("name", size = .75, bg.color="white", bg.alpha=.25, auto.placement = TRUE) +
tm_legend(position=c("right", "top"), frame=TRUE, bg.color = "gold")
}
