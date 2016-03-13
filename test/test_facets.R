####################################################################################################
## Test drop.units and free.coords combinations
####################################################################################################
data(NLD_muni)
data(NLD_prov)

tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
	tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", drop.units=FALSE, free.coords=FALSE) +
	tm_layout(legend.show = FALSE)

tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
	tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", drop.units=TRUE, free.coords=FALSE) +
	tm_layout(legend.show = FALSE)

tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
	tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", drop.units=FALSE, free.coords=TRUE) +
	tm_layout(legend.show = FALSE)

tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
	tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", free.coords=TRUE, drop.units=TRUE) +
	tm_layout(legend.show = FALSE)


####################################################################################################
## Test group by variants with polygons
####################################################################################################
N <- NLD_prov[1:4,]

N$by <- factor(c("R", "L", "R", "L"), levels=c("L", "R", "S"))
N$var <- factor(c(NA, "A", "A", "B"), levels=c("A", "B"))

N$by2 <- factor(c("R", "L", "R", NA), levels=c("L", "R"))
N$var2 <- factor(c("B", "A", "A", "B"), levels=c("A", "B"))


tm_shape(N) +
	tm_polygons("gold") +
	tm_facets(by="by", drop.empty.facets = F)

tm_shape(N) +
	tm_polygons("gold") +
	tm_facets(by="by", drop.units = TRUE, free.coords = T)


tm_shape(N) +
	tm_polygons("var") +
	tm_facets(by="by")

tm_shape(N) +
	tm_polygons("var") +
	tm_facets(by="by", drop.units = T)

tm_shape(N) +
	tm_polygons("var2") +
	tm_facets(by="by2")

tm_shape(N) +
	tm_polygons("var2") +
	tm_facets(by="by2", drop.units = T)


####################################################################################################
## Test group by variants with lines
####################################################################################################

data(rivers)
library(rgeos)
R <- rivers[order(gLength(rivers, byid = T), decreasing = T)[1:4], ]

R$by <- factor(c("R", "L", "R", "L"), levels=c("L", "R"))
R$var <- factor(c(NA, "A", "A", "B"), levels=c("A", "B"))

R$by2 <- factor(c("R", "L", "R", NA), levels=c("L", "R"))
R$var2 <- factor(c("B", "A", "A", "B"), levels=c("A", "B"))
R$len <- c(1,10,2,11)

tm_shape(R) +
	tm_lines("var", lwd=5, palette="Set1") +
	tm_facets(by="by")

tm_shape(R) +
	tm_lines("var", lwd="len", palette="Set1", scale=5) +
	tm_facets(by="by", free.scales = F)

tm_shape(R) +
	tm_lines("var", lwd=5, palette="Set1") +
	tm_facets(by="by", drop.units = T)

tm_shape(R) +
	tm_lines("var2", lwd=5, palette="Set1") +
	tm_facets(by="by2")

tm_shape(R) +
	tm_lines("var2", lwd=5, palette="Set1") +
	tm_facets(by="by2", drop.units = T)

####################################################################################################
## Test group by variants with points
####################################################################################################

data(metro)
metro$name

M <- metro[match(c("London", "Paris", "Berlin", "Rome"), metro$name), ]
qtm(M)

M$by <- factor(c("M", "L", "M", "L"), levels=c("L", "M"))
M$var <- factor(c(NA, "A", "A", "B"), levels=c("A", "B"))

M$by2 <- factor(c("M", "L", "M", NA), levels=c("L", "M"))
M$var2 <- factor(c("B", "A", "A", "B"), levels=c("A", "B"))
M$len <- c(1,10,2,11)

tm_shape(M) +
	tm_bubbles("var", size=5, palette="Set1") +
	tm_facets(by="by")

tm_shape(M) +
	tm_bubbles("var", size="len", palette="Set1", scale=5) +
	tm_facets(by="by", free.scales = F)

tm_shape(M) +
	tm_bubbles("var", size=5, palette="Set1") +
	tm_facets(by="by", drop.units = T)

tm_shape(M) +
	tm_bubbles("var2", size=5, palette="Set1") +
	tm_facets(by="by2")

tm_shape(M) +
	tm_bubbles("var2", size=5, palette="Set1") +
	tm_facets(by="by2", drop.units = T)

####################################################################################################
## Test group by variants with text
####################################################################################################

tm_shape(M) +
	tm_text("name", col =  "var", size=5, palette="Set1") +
	tm_facets(by="by")

tm_shape(M) +
	tm_text("name", "var", size="len", palette="Set1", scale=5) +
	tm_facets(by="by", free.scales = F)

tm_shape(M) +
	tm_text("name", "var", size=5, palette="Set1") +
	tm_facets(by="by", drop.units = T)

tm_shape(M) +
	tm_text("name", "var2", size=5, palette="Set1") +
	tm_facets(by="by2", free.coords = T, drop.units=T)

tm_shape(M) +
	tm_text("name", "var2", size=5, palette="Set1") +
	tm_facets(by="by2", drop.units = T)

####################################################################################################
## Test group by variants with rasters
####################################################################################################

data(land)
L <- land[as.integer(land$cover_cls) < 5,]

tm_shape(L) +
	tm_raster("cover") +
	tm_facets("cover_cls", free.coords = T)

tm_shape(L) +
	tm_raster("cover") +
	tm_facets("cover_cls", free.coords = T, showNA = F)


