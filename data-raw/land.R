library(rgdal)
library(raster)
library(RColorBrewer)

#http://www.iscgm.org/gm/glcnmo.html


data(World)
library(rgdal)
library(sp)
library(raster)
x = readGDAL("../shapes/gm_lc_v2_0_1_simple1_25p.tif")
y = readGDAL("../shapes/gm_ve_v2_simple_1_25p.tif")
z = readGDAL("../shapes/gm_el_v2_1_25p.tif")

x$band1 <- factor(x$band1, levels=1:20, labels=
				  	c("Broadleaf Evergreen Forest", "Broadleaf Deciduous Forest",
				  	  "Needleleaf Evergreen Forest", "Needleleaf Deciduous Forest",
				  	  "Mixed Forest", "Tree Open",
				  	  "Shrub", "Herbaceous",
				  	  "Herbaceous with Sparse Tree/Shrub", "Sparse vegetation",
				  	  "Cropland", "Paddy field",
				  	  "Cropland / Other Vegetation Mosaic", "Mangrove",
				  	  "Wetland", "Bare area,consolidated (gravel,rock)",
				  	  "Bare area,unconsolidated (sand)", "Urban",
				  	  "Snow / Ice", "Water bodies")
)
y$band1[y$band1==254] <- NA

land <- x
names(land)[1] <- "cover"

ids <- c(1,1,1,1,1,1,2,2,2,5,3,3,3,4,4,5,5,6,7,8)

land$cover_cls <- factor(ids[as.numeric(land$cover)], levels=1:8, labels=c("Forest", "Other natural vegetation", "Cropland", "Wetland", "Bare area/Sparse vegetation", "Urban", "Snow/ice", "Water"))

land$trees <- y$band1
land$elevation <- z$band1

land$elevation <- ifelse(land$elevation< -1000, NA, land$elevation)


pal20 <- do.call("rgb", c(unname(as.list(read.table("../shapes/read_me_color_lc.txt", skip = 2)))[-1], list(maxColorValue=255)))

cat(paste0("pal20 <- c(\"", paste(pal20, collapse = "\", \""), "\")"))

#pal8 <- pal20[c(2, 8, 11, 15, 16, 18:20)]

paired <- brewer.pal(12, "Paired")

pal8 <- paired[c(4,3,7,2,NA,6, NA,1)]
pal8[c(5,7)] <- gray(c(.6, .9))

cat(paste0("pal8 <- c(\"", paste(pal8, collapse = "\", \""), "\")"))

save(land, file="./data/land.rda", compress="xz")


# convert to raster
#land <- st_as_stars(land)
#save(land, file="./data/land.rda", compress="xz")


data(land)
library(raster)

land <- brick(land)
save(land, file="./data/land.rda", compress="xz")


# convert to stars
library(stars)
library(sf)
library(raster)
data(land, package = "tmap")

lvls <- levels(land)
m <- land[]
ms <- list()
for (i in 1:4) {
	if (i %in% 1:2) {
		mi <- factor(m[,i], levels = lvls[[i]]$ID, labels = lvls[[i]]$cover)
	} else {
		mi <- m[,i]
	}
	dim(mi) <- dim(land)[2:1]
	#if (i %in% 1:2) class(mi) <- c("factor", "matrix")
	names(dim(mi)) <- c("x", "y")
	ms[[i]] <- mi
}
names(ms) <- names(land)

ncols <- ncol(land)
nrows <- nrow(land)
bbx <- st_bbox(land)
crs <- st_crs(land)

dimensions <- structure(list(
	x = structure(list(from = 1, to = ncols, offset = bbx["xmin"],
				  delta = (bbx["xmax"] - bbx["xmin"]) / ncols, refsys = crs$proj4string, point = NULL, values = NULL),
				  class = "dimension"),
	y = structure(list(from = 1, to = nrows, offset = bbx["ymax"],
					   delta = (bbx["ymin"] - bbx["ymax"]) / nrows, refsys = crs$proj4string, point = NULL, values = NULL),
				  class = "dimension")),
	raster = stars:::get_raster(), class = "dimensions")
	
s1 <- stars::st_as_stars(ms)
attr(s1, "dimensions") <- dimensions

land <- s1

save(land, file="./data/land.rda", compress="xz")

