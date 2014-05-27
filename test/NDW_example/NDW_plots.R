devtools::load_all()
library(sp)
library(rgeos)
library(RColorBrewer)
palDark <- c(brewer.pal(9, "Set1"), brewer.pal(8, "Set2"), brewer.pal(8, "Dark2"), brewer.pal(12, "Set3"))

rwb <- get_shape("../test/NDW_example/rijksweg2013simpel.shp")
rwb <- set_projection(rwb, projection="rd")

rwb_cr <- get_shape("../test/NDW_example/rijksweg2013simpel_corop.shp")


loops <- get_shape("../test/NDW_example/loops.shp")
loops <- set_projection(loops, projection="rd")






########## roads simplified
pdf("../test/NDW_example/rw_simple2.pdf", width=16, height=16)
geo_shape(corop) +
	#geo_borders() +
	geo_fill() +
	geo_shape(rwb) + 
	geo_lines(col="ID", lwd=.5, palette=palDark) +
	geo_text("ID", cex=1) +
	geo_shape(rw) +
	geo_lines(col="black", lwd=.02) +
	geo_shape(loops) +
	geo_bubbles(col="black", size=.01) +
	geo_theme("Origine rijkswegen (dunne zwarte lijntjes) +\nVerkeerlslussen (zwarte bolletjes) +\nVereenvoudigde rijkswegen (gekleurde lijnen)", grid.show=TRUE)
dev.off()

########## roads small multiples
pdf("../test/NDW_example/rw_mult.pdf", width=16, height=16)
geo_shape(corop) +
	geo_fill() +
	geo_borders(lwd=.5) +
	geo_shape(rwb) +
	geo_lines("ID", palette="red", by=TRUE)
dev.off()	




########## roads per corop
pdf("../test/NDW_example/rw_per_corop.pdf", width=6, height=6)
geo_shape(corop) +
	geo_fill("CR_2013", palette="Set2") +
	geo_shape(rwb_cr) +
	geo_lines("CR_2013", palette="Dark2") +
	geo_theme("Roads split by corop", legend.profile="hide", legend.max.categories=40)
dev.off()


corop@proj4string
rwb@proj4string
loops@proj4string
pdf("../test/NDW_example/loops_classified.pdf", width=6, height=6)
geo_shape(corop) +
	geo_fill() +
 	geo_shape(rwb) +
 	geo_lines("grey50", lwd=1) +
	geo_shape(loops) +
	geo_bubbles(col="type", palette="Set1", size=.2)+
	geo_theme("Within 25m range?", legend.config="bubble.col", legend.max.categories=40)
dev.off()





### plots for presentation and papers
########## roads simplified
rw_sub <- rw[rw$roadname %in% c("A2", "A12", "A27", "A28"),]
rw_sub$roadname <- factor(as.character(rw_sub$roadname), levels=c("A2", "A12", "A27", "A28"))
rwb_sub <- rwb[c(2,12, 20, 21),]
rwb_sub$ID <- factor(as.character(rwb_sub$ID), levels=c("A2", "A12", "A27", "A28"))
png("../test/NDW_example/simplifyRoad1.png", width=3077, height=2000, res=600)
geo_shape(rw_sub, xlim=c(132000, 142000), ylim=c(451000, 457500), relative=FALSE) +
	geo_lines(col="roadname", lwd=.5, palette="Dark2") +
	geo_shape(rwb_sub) + 
	geo_text("ID", cex=.5) +
	geo_theme("", legend.profile="hide")
dev.off()

png("../test/NDW_example/simplifyRoad2.png", width=3077, height=2000, res=600)
geo_shape(rw_sub, xlim=c(132000, 142000), ylim=c(451000, 457500), relative=FALSE) +
	geo_lines(col="roadname", lwd=.5, palette="Dark2") +
	geo_shape(loops) +
	geo_bubbles(col="black", size=.25) +
	geo_shape(rwb_sub) + 
	geo_text("ID", cex=.5) +
	geo_theme("", legend.profile="hide")
dev.off()

png("../test/NDW_example/simplifyRoad3.png", width=3077, height=2000, res=600)
geo_shape(rwb_sub, xlim=c(132000, 142000), ylim=c(451000, 457500), relative=FALSE) +
	geo_lines(col="ID", lwd=3, palette="Set2") +
	geo_text("ID", cex=.5) +
	geo_shape(rw_sub) + 
	geo_lines(col="roadname", lwd=.25, palette="Dark2") +
	geo_shape(loops) +
	geo_bubbles(col="black", size=.25) +
	geo_theme("", legend.profile="hide")
dev.off()


png("../test/NDW_example/simplifyRoad4.png", width=3077, height=2000, res=600)
geo_shape(rwb_sub, xlim=c(132000, 142000), ylim=c(451000, 457500), relative=FALSE) +
	geo_lines(col="ID", lwd=3, palette="Set2") +
	geo_text("ID", cex=.5) +
	geo_shape(rw_sub) + 
	geo_lines(col="roadname", lwd=.25, palette="Dark2") +
	geo_shape(loops) +
	geo_bubbles(col="type", size=.25) +
	geo_theme("", legend.config="bubble.col")
dev.off()


png("../test/NDW_example/rw_per_corop.png", width=1500, height=1500, res=600)
geo_shape(corop) +
	geo_fill("CR_2013", palette="Set2") +
	geo_shape(rwb_cr) +
	geo_lines("CR_2013", palette="Dark2", lwd=1.5) +
	geo_shape(loops) +
	geo_bubbles(col="black", size=.1) +
	geo_theme("", legend.profile="hide", legend.max.categories=40, draw.frame=FALSE, bg.color="white")
dev.off()

