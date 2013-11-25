options(shp_dir=system.file("shapes", package="geoNL"))
shp.gm_rd <- getShape("gm", 2012)
shp.gm_wgs84 <- rd2wgs84(shp.gm_rd)


