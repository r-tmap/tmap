Sys.setenv(FFTW3_DIR="c:/fftw")

install_github(repo = "mtennekes/Rcartogram")

demo(synthetic, package = "Rcartogram")


library(Rcartogram)
library(sp)
library(rgdal)
library(rgeos)
#library(ggplot2)

#Get the map you want to use


#Add rough estimates of population and calculate population density
shp@data$population <- c(53000000, 2000000, 5000000, 3000000 )
osgb <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")
shp@data$area.sq.km <- gArea(spTransform(shp, osgb), TRUE)/1000000
shp@data$density <- with(shp@data, population/area.sq.km)

data(NLD_muni)
NLD_muni$density <- calc_densities(NLD_muni, var = "population", units = "km2")
shp <- NLD_muni

# #plot a choropleth map using the ggplot method just to see it
# shp@data$id<- rownames(shp@data)
# shp.points<- fortify(shp)
# lowres.shp<- shp.points[seq(1,nrow(shp.points), 200),]
# lowres.shp<- merge(lowres.shp, shp@data)
# map<- ggplot(lowres.shp, aes(long, lat, group=group, fill=density, map_id=id)) 
# map<- map + geom_map(map=lowres.shp) + coord_equal() +geom_path()
# map<- map + theme(title=element_blank())
# map<- map + scale_fill_continuous(high="#132B43", low="#56B1F7")
# map

#Find the map's bounding box, and make a grid over this (including the 'sea' area)
bb <- bbox(shp)
N <- 100  #pick the number of cells you want along the side of the map
C <- 1.5    #set the proportion by which to expand the map to allow for 'sea' along the long side

big.bb <- bb
big.bb[,"min"] <- bb[,"min"] - ((bb[,"max"]-bb[,"min"])*(C-1)/2)
big.bb[,"max"] <- bb[,"max"] + ((bb[,"max"]-bb[,"min"])*(C-1)/2)
cells.dim <- c(N*C, N*C)
cellsize <- (bb[,"max"]-bb[,"min"])/N
cellcentre.offset <- big.bb[,"min"]
grid <- GridTopology(cellcentre.offset, cellsize, cells.dim)
grid <- SpatialGrid(grid, proj4string(shp))
data <- over(grid, shp)
mean.dens <- mean(data$density, na.rm=TRUE)
data$density[is.na(data$density)] <- mean.dens
grid <- SpatialGridDataFrame(grid, data)

## try to plot grid
library(raster)
library(classInt)
library(RColorBrewer)

cInt <- classIntervals(grid$density, n = 5, style="kmeans")

cols <- rev(brewer.pal(length(cInt$brks)-1, "Blues"))[findCols(cInt)]

rast <- raster(grid, layer=12)

plot(rast)

#Run cartogram() on the density grid to generate the coordinate offsets
#These offsets are in grid coordinates only i.e. (1:N, 1:N)
dens.matrix <- as.matrix(grid["density"])
cart <- cartogram(dens.matrix)

#Interpolate all the points of all the polygons of the original map into their new position using predict(grid,...)
#Cannot put original polygon coordinates in here, since they are in projected grid.
#?First project the points onto the grid, then interpolate to their new position using the cartogram,
#?then re-project back to projection.

#Attempt to use the projection transformation function to do the projection for me, unsuccessfully. 
#CRS.sp <- do.call(rbind ,strsplit(unlist(strsplit(proj4string(shp), " ")), "="))
# k <- as.numeric(CRS.sp[CRS.sp[,1]=="+k",2])
# k <- k/max((big.bb[,"max"]-big.bb[,"min"])/N)
# CRS.sp[CRS.sp[,1]=="+k",2] <- k
# CRS.sp[CRS.sp[,1]=="+x_0",2] <- (as.numeric(CRS.sp[CRS.sp[,1]=="+x_0",2]) + big.bb["x","min"])*k
# CRS.sp[CRS.sp[,1]=="+y_0",2] <- (as.numeric(CRS.sp[CRS.sp[,1]=="+y_0",2]) + big.bb["y","min"])*k
# myCRS <- CRS(paste(paste(CRS.sp[,1], CRS.sp[,2], sep="="), collapse=" "))
# 
# trans.sha <- spTransform(sha, myCRS)
# shp@polygons[[1]]@Polygons[[1]]@coords[,1]


# Attempt to acess every point in every polygon, interpolate it and regroup them in the correct order
# interp.SpPdf <- function(SpPdf, cart) {
#   require(Rcartogram)
#   for(i in 1:length(SpPdf)) {
#     SpP <- SpPdf@polygons[[i]]
#     for(j in 1:length(SpP)) {
#       poly <- SpP@Polygons[[j]]
#       t.poly <- predict(cart, poly@coords[,1], poly@coords[,2])
#     }
#   }
