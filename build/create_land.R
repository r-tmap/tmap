library(rgdal)
library(raster)

#http://www.iscgm.org/gm/glcnmo.html


data(World)
library(rgdal)
library(sp)
library(raster)
x = readGDAL("../shapes/gm_lc_v1_simple2p.tif")
y = readGDAL("../shapes/gm_ve_v1_simple2p.tif")

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
land$trees <- y$band1
names(land)[1] <- "cover"

save(land, file="./data/land.rda", compress="xz")

