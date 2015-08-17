data(NLD_muni)

# download Dutch neighborhoods (called "buurten") from http://www.cbs.nl/nl-NL/menu/themas/dossiers/nederland-regionaal/publicaties/geografische-data/archief/2015/wijk-en-buurtkaart-2014-art.htm
brt <- read_shape("../../shape_files/buurt_2014.shp")

brt$OPP_LAND[brt$OPP_LAND<0] <-  0
brt$AANT_INW[brt$AANT_INW<0] <- 0
brt$P_N_W_AL[brt$P_N_W_AL<0] <- 0
brt$P_WEST_AL[brt$P_WEST_AL<0] <- 0

brt$dens <- brt$AANT_INW / brt$OPP_LAND


brt$non_west <- brt$dens * brt$P_N_W_AL / 100
brt$west <- brt$dens * brt$P_WEST_AL / 100
brt$dutch <- brt$dens - brt$non_west - brt$west

r <- raster(extent(bb(brt)), nrows=500, ncols=500, crs=brt@proj4string)

brt$ID <- as.integer(brt$BU_CODE)
r2 <- rasterize(brt, r, field=brt$ID) #raster with buurt id's
r2@data@names <- "ID"
g <- as(r2, "SpatialGridDataFrame")


g <- append_data(g, data=brt@data[, c("ID", "dens", "dutch", "non_west", "west")], key.shp = "ID", key.data="ID", ignore.na=TRUE)

tm_shape(g) + tm_raster("west")

sm <- sum(g$dens, na.rm=TRUE)

probs <- sapply(g@data[, c("dutch", "non_west", "west")], function(x) {
	ifelse(is.na(x), 0, x/sm)	
})


k <- 3
lvls <- c("Dutch", "Non-western immigrants", "western immigrants") #letters[1:k]
n <- 10000

probs2 <- apply(probs, MARGIN = 1, function(x){
	pos <- sum(x!=0)
	if (pos > 1) {
		id <- sample.int(k, 1, prob=x)
		y <- rep(0, k)
		y[id] <- sum(x)
		y
	} else x
})


notNA <- which(!is.na(g$ID))
N <- length(notNA)


samples <- apply(probs2, MARGIN=1, function(x) {
	(1:N)[match(sample.int(n=length(x), size=n * sum(x), prob=x), notNA)]
})


p <- as(g, "SpatialPointsDataFrame")
p2 <- p[unlist(samples), ]
p2$cat <- unlist(mapply(rep, lvls, sapply(samples, length)))


tm_shape(p2) + tm_bubbles(col="cat", size=.01, palette="Dark2", title.col = "Dutch population")
