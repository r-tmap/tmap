options(shp_dir=system.file("shapes", package="geoNL"))
shp.gm <- getShape("gm", 2012)
shp2kml(shp.gm, "test.kml")

