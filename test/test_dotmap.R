data(NLD_muni)

# download Dutch neighborhoods (called "buurten") from http://www.cbs.nl/nl-NL/menu/themas/dossiers/nederland-regionaal/publicaties/geografische-data/archief/2015/wijk-en-buurtkaart-2014-art.htm
brt <- read_shape("../../shape_files/buurt_2014.shp")
brt <- gBuffer(brt, byid=TRUE, width=0)


brt$OPP_LAND[brt$OPP_LAND<0] <-  0
brt$OPP_LAND <- brt$OPP_LAND / 100 # convert from hectare (ha) to km2
brt$AANT_INW[brt$AANT_INW<0] <- 0
brt$P_N_W_AL[brt$P_N_W_AL<0] <- 0
brt$P_WEST_AL[brt$P_WEST_AL<0] <- 0

brt$dens <- (brt$AANT_INW / brt$OPP_LAND)
brt$dens[is.nan(brt$dens)] <- 0

brt$non_west <- brt$dens * brt$P_N_W_AL / 100
brt$west <- brt$dens * brt$P_WEST_AL / 100
brt$dutch <- brt$dens - brt$non_west - brt$west


ams <- brt[which(brt$GM_NAAM=="Amsterdam"), ]
utr <- brt[which(brt$GM_NAAM=="Utrecht"), ]




## Den Haag

dhbb <- bb(brt[which(brt$GM_NAAM=="'s-Gravenhage"), ])

dh <- crop(brt[which(brt$OPP_LAND > 0), ], dhbb)
dh3 <- read_osm(bb(dhbb, current.projection="rd", projection="longlat"), type = "mapquest")

dh2 <- sample_dots(dh, c("dutch", "west", "non_west"), N=1e6, w=150, var.labels = c("Dutch (native)", "Western immigrants", "Non-western immigrants"))

tm_shape(dh3) + tm_raster(saturation=.2) +
	tm_shape(dh2) + tm_bubbles(col="class", size=.035, alpha=.5, palette="Dark2", title.col = "Dutch population") +
	tm_layout(inner.margins=0)
#+	tm_shape(dh) + tm_borders(lwd=1)


## Randstad
rst2 <- dotmap(rst, c("non_west", "west", "dutch"), N=1e6, n=1e4)
rstbb <- bb(xlim=c(60000, 150000), ylim=c(430000, 500000))

rst3 <- read_osm(bb(rstbb, current.projection="rd", projection="longlat"))
qtm(rst3)

rst <- crop(brt, rstbb)

rst2 <- dotmap(rst, c("non_west", "west", "dutch"), N=1e6, n=1e4)

tm_shape(rst3) + tm_raster() +
	tm_shape(rst2) + tm_bubbles(col="class", size=.02, palette=c("red", "forestgreen", "lightblue"), title.col = "Dutch population") +
	tm_shape(rst) + tm_borders(lwd=1)



# shp <- brt
# vars <- c("non_west", "west", "dutch")

shp2 <- dotmap(brt, c("non_west", "west", "dutch"))
ams2 <- dotmap(ams, c("non_west", "west", "dutch"))
utr2 <- dotmap(utr, c("non_west", "west", "dutch"))

utr3 <- read_osm(bb(utr, projection="longlat"))


tm_shape(ams) + tm_borders() + 
	tm_shape(ams2) + tm_bubbles(col="class", size=.03, palette=c("red", "forestgreen", "lightblue"), title.col = "Dutch population")

tm_shape(utr3) + tm_raster() +
	tm_shape(utr2) + tm_bubbles(col="class", size=.03, palette=c("red", "forestgreen", "lightblue"), title.col = "Dutch population") +
tm_shape(utr) + tm_borders(lwd=2)
	

shp2 <- dotmap(brt, c("non_west", "west", "dutch"), N=1e6, n=2e5)
tm_shape(shp2) + tm_bubbles(col="class", size=.01, palette=c("red", "forestgreen", "lightblue"), title.col = "Dutch population")

png("dotmap_high_res.png", width=2915, height=3431)
tm_shape(shp2) + tm_bubbles(col="class", size=.08, palette="Set1", title.col = "Dutch population") + tm_layout(inner.margins=0, outer.margins = 0)
dev.off()



	
	
	
tm_shape(p2) + tm_bubbles(col="cat", size=.01, palette="Dark2", title.col = "Dutch population")
