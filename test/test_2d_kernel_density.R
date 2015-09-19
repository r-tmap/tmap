data(land)
data(World)

library(spatstat)
library(KernSmooth)
library(maptools)
library(maptools)
library(raster)
library(fields)
library(mgcv)
library(sp)
library(rgeos)


#### http://stackoverflow.com/questions/20848740/smoothing-surface-plot-from-matrix
#### http://stackoverflow.com/questions/13668110/smoothing-of-spatial-data
#### http://stackoverflow.com/questions/10047870/plotting-interpolated-data-on-map
#### http://vita.had.co.nz/papers/density-estimation.pdf

#### KernSmooth

co <- coordinates(land)
colist <- lapply(1:100, function(i) co[which(land$trees>=i), ])
co2 <- do.call("rbind", colist)

x <- bkde2D(co2, bandwidth = c(2,2), gridsize=c(1080, 540), range.x=list(land@bbox[1,], land@bbox[2,]))
land$fhat <- as.vector(x$fhat[, ncol(x$fhat):1])
land$fhat[is.na(land$trees)] <- NA

# normalize
land$fhat <- land$fhat * (sum(land$trees, na.rm=TRUE) / sum(land$fhat, na.rm=TRUE))

cl <- contourLines(x$x1, x$x2, x$fhat, nlevels=5) 
cl2 <- ContourLines2SLDF(cl)
qtm(cl2)


qtm(land, raster="fhat", raster.style="fixed", raster.breaks=seq(0, 100, by=10)) + qtm(cl)
qtm(land, raster="trees", raster.style="fixed", raster.breaks=seq(0, 100, by=10)) + qtm(World, fill=NULL)

raster:::contour(x$x1, x$x2, x$fhat)


shp <- land
var <- "trees"
var <- "elevation"


tm_shape(land) + tm_raster("trees") + tm_shape(cl2) + tm_lines()

land2 <- iso_lines(land, "trees", bandwidth1 = c(.9,.9))
land2 <- iso_lines(land, "elevation")

qtm(land, raster="elevation")

qtm(land2, "level", fill.palette="Blues")


shp2@polygons

str(cl2@data)

lapply(rev(levels(cl2$level)), {
	
})

qtm(shp2)











#### mgcv



df <- as.data.frame(cbind(coordinates(land), z=land$trees, z2=land$elevation))
mod <- gam(z ~ te(x, y, k=15), data = df)
mod2 <- gam(z2 ~ te(x, y, k=15), data = df)

land$elevation2 <- NA
land$elevation2[!is.na(land$elevation)] <- fitted(mod2)


land$trees2 <- NA
land$trees2[!is.na(land$trees)] <- fitted(mod)
qtm(land, raster="trees", raster.style="fixed", raster.breaks=seq(0, 100, by=10))
qtm(land, raster="trees2", raster.style="fixed", raster.breaks=seq(0, 100, by=10))

qtm(land, raster="elevation")
qtm(land, raster="elevation2", raster.n=10)


#### fields

## project raster
land2 <- projectRaster(land, crs=get_projection(World))
tm_shape(land) + tm_raster("trees")
tm_shape(land2) + tm_raster("trees")

df2 <- df[!is.na(df$z), ]
test.spline <- Tps(df2[,c("x","y")], df2$z)
new.grid <- predictSurface(test.spline, nx = 200, ny = 200)
image(new.grid)


image.smooth
str(land@data)

landm <- as.matrix(raster(land, layer="trees"))
landsm <- image.smooth(landm, theta=.25)

land$sm <- as.vector(t(landsm$z))
qtm(land, raster="sm")

######### ks
landm <- as.matrix(raster(land, layer="trees"))

fhat <- kde(landm)
