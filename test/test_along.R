data(Europe)

Europe$cat <- c(rep("AJ", 37), rep("KZ", 31))
Europe$size <- ifelse(Europe$pop_est>40000000, "Large", "Small")
Europe$loc <- ifelse(is.na(Europe$part), NA,  ifelse(Europe$part %in% c("Northern Europe", "Western Europe"), "NW", "SE"))

tm_shape(Europe) +
	tm_polygons("red") +
	tm_facets(by=c("size", "loc"))

tm_shape(Europe) +
	tm_polygons("red") +
	tm_facets(by=c("size", "loc"), along = "cat")

tm_shape(Europe) +
	tm_polygons("red") +
	tm_facets(by=c("size", "loc"), along = "loc")

tm_shape(Europe) +
	tm_polygons("red") +
	tm_facets(by="size", along = "loc")


tm_shape(Europe) +
	tm_polygons("red") +
	tm_facets(by="loc")
