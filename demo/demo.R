zipfile = "../demo/shape/forenkeltafstemningshapeformat.zip"
zipdir <- tempdir()
unzip(zipfile, exdir=zipdir)

# library(tmap)
shp <-  read_shape(file = paste0(zipdir, "/simpelt.shp"))
shp <- unionSpatialPolygons(shp, shp$KommuneNav)

shp_muni <- get_IDs(shp)

pop <- read.csv("../demo/data/Danish_population.csv", header = FALSE, )
names(pop) <- c("muni", "count")

pop2 <- read.csv("../demo/data/Dutch_in_Denmark.csv", header = FALSE, )
names(pop2) <- c("x", "muni", "count")

all(pop$muni==pop2$muni)
pop$Dutch <- pop2$count


str(pop)
pop_muni <- as.character(pop$muni)


res <- amatch(shp_muni, pop_muni, method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1), maxDist=10)
cat("Matched", sum(!is.na(res)), "of", length(res), "\n")

shp_data <- pop[res, ]

shp <- append_data(shp_data, shp, fixed.order = TRUE)

tm_shape(shp) +
	tm_fill("count", style="kmeans", n=10, convert2density = TRUE) +
	tm_borders()
