get_HCL <- function(v, H1 = 0, L.inverse = TRUE, to.colors=TRUE, L.lim=c(0,100), L.sqrt=FALSE) {
	v <- as.matrix(v)
	rs <- rowSums(v)
	a <- rs / max(rs) # normalized row sums (for L)
	m <- v / rs # matrix of probabilities
	
	m[is.nan(m)] <- NA
	
	y <- (m[,3] * sqrt(3) / 2) - (sqrt(3) / 6)
	x <- (m[,1] + m[,3] * .5) - .5
	
	
	H <- atan2(x,y)/pi*180 - 120 + H1
	H[which(H < 0)] <- H[which(H < 0)] + 360
	H[is.na(H)] <- 0
	C <- sqrt(x^2 + y^2) * (3 / sqrt(3)) * 100
	C[is.na(C)] <- 0
	Lext <- L.lim[2] - L.lim[1]
	if (L.sqrt) a <- sqrt(a)
	if (L.inverse) {
		L <- L.lim[2] - (a * Lext)
	} else {
		L <- L.lim[1] + a * Lext
	}
	
	if (to.colors) {
		hcl(h=H, c=C, l=L)
	} else {
		data.frame(h=H, c=C, l=L)	
	}
}




#####################
## HCL Parameters
#####################
H1 <- 0
L.lim <- c(40, 100)
light.is.dense <- TRUE
L.sqrt <- FALSE
#####################
## DH
#####################

DH_nbhd$HCL_colors <- get_HCL(DH_nbhd@data[, c("dutch", "west", "non_west")], H1=H1, L.inverse = light.is.dense, L.lim=L.lim, L.sqrt=L.sqrt)
DH_nbhd$temp <- factor(rep(1:3, length.out=length(DH_nbhd)), labels=c("Dutch (native)", "Western immigrants", "Non-western immigrants"))

tm_shape(DH_nbhd) +
	tm_polygons("temp", palette=hcl(h=c(0,120,240)+H1, l=ifelse(light.is.dense, 40, 100), c=100), title="Dense population") +
tm_shape(DH_nbhd) +
	tm_polygons("temp", palette=hcl(h=c(0,120,240)+H1, l=ifelse(light.is.dense, 100, 40), c=100), title="Sparse population") +
tm_shape(DH_nbhd) +
	tm_polygons("HCL_colors") +
tm_layout(inner.margins=0)


### Hybrid
tm_shape(DH_nbhd) +
	tm_polygons("HCL_colors", alpha=.5) +
tm_shape(DH_nbhd_dots) + 
	tm_dots("class", size=.04, alpha=.75,
			palette=hcl(h=c(0,120,240)+H1, l=ifelse(light.is.dense, 40, 100), c=100), title = "The Hague population") +
tm_layout(inner.margins=0)


#####################
## Randstad
#####################
bb_rs <- bb(xlim=c(60000, 150000), ylim=c(410000, 500000))
RS_nbhd <- raster::crop(NLD_nbhd, bb_rs)
data(NLD_prov)

L.sqrt <- TRUE

RS_nbhd$HCL_colors <- get_HCL(RS_nbhd@data[, c("dutch", "west", "non_west")], H1=H1, L.inverse = light.is.dense, L.lim=L.lim, L.sqrt=L.sqrt)
RS_nbhd$temp <- factor(rep(1:3, length.out=length(RS_nbhd)), labels=c("Dutch (native)", "Western immigrants", "Non-western immigrants"))


tm_shape(RS_nbhd) +
	tm_polygons("temp", palette=hcl(h=c(0,120,240)+H1, l=ifelse(light.is.dense, 40, 100), c=100), title="Dense population") +
	tm_shape(RS_nbhd) +
	tm_polygons("temp", palette=hcl(h=c(0,120,240)+H1, l=ifelse(light.is.dense, 100, 40), c=100), title="Sparse population") +
tm_shape(RS_nbhd, is.master = TRUE) +
	tm_fill("HCL_colors") +
tm_shape(NLD_prov) +
	tm_borders() +
tm_layout(inner.margins=0)

