rw <- get_shape("../test/NDW_example/rijksweg2013.shp")
rw <- set_projection(rw, current.projection="rd")

corop <- get_shape("../test/NDW_example/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")


rwb_cr <- get_shape("../test/NDW_example/rw2013_doorgaand_cr.shp")
loops <- get_shape("../test/NDW_example/loops2013_classified.shp")

library(RColorBrewer)

pal <- c(brewer.pal(9, "Pastel1")[-9], 
		 brewer.pal(8, "Pastel2")[-7],
		 brewer.pal(12, "Set3")[-9])

rwb_cr <- rwb_cr[!is.na(rwb_cr$ID), ]

is_N_weg <- substr(as.character(rwb_cr$ID), 1, 1)=="N"
require(sp)
rwb_cr$length <- SpatialLinesLengths(rwb_cr, longlat=FALSE)/1000
rwb_cr$label <- format(rwb_cr$length, trim=TRUE, digits=2)

pdf("../test/NDW_example/rw_poster_v2.pdf", width=33.11, height=46.81)
geo_shape(corop) +
	geo_fill("white") +
	geo_borders("grey70") +
  	geo_shape(rwb_cr) + 
  	geo_lines(col="ID", lwd=6, palette=pal, max.categories=52) +
 	#geo_shape(rw) +
  	#geo_lines(col="black", lwd=.001) +
	geo_shape(loops) +
	geo_bubbles(col="black", size=.001) +
	geo_shape(rwb_cr[!is_N_weg, ]) + 
	geo_text("ID", cex=.75, fontface="bold", fontcolor="white", bg.color=rgb(238, 28, 35, maxColorValue=255), bg.alpha=255) +
	geo_shape(rwb_cr[is_N_weg, ]) + 
	geo_text("ID", cex=.75, fontcolor="black", bg.color=rgb(255,221, 0, maxColorValue=255), bg.alpha=255) +
	geo_shape(rwb_cr) + 
	geo_text("label", cex=.5, ymod=-.003, fontcolor="white", bg.color=rgb(0, 126, 196, maxColorValue=255), bg.alpha=255) +
	geo_shape(corop) +
	geo_text("CR_LABEL", cex=1, bg.alpha=00, fontcolor="grey50") +
	geo_theme("* Doorgaande rijkswegen in Nederland\n
* Afstanden (geschat) per corop\n
* Verkeerslussen weergegeven als zwarte punten\n
* Projectie: rijksdriehoekstelsel\n
* Gemaakt met R package geo",bg.color="grey90", title.position=c(.05,.82), title.cex=3, legend.config="", grid.show=TRUE, grid.color="grey80")
dev.off()


bboxZL <- unlist(get_polygon_ranges(corop)[[1]][39,])

# pdf("../test/NDW_example/rw_poster_ZL.pdf", width=33.11, height=46.81)
# geo_shape(corop, xlim=bboxZL[1:2], ylim=bboxZL[3:4], relative=FALSE) +
# 	geo_fill("grey90") +
# 	geo_borders("black") +
# 	geo_shape(rwb_cr) + 
# 	geo_lines(col="ID", lwd=6, palette=pal, max.categories=52) +
# 	geo_text("ID", cex=1) +
# 	geo_shape(rw) +
# 	geo_lines(col="black", lwd=.5) +
# 	geo_shape(loops) +
# 	geo_bubbles(col="black", size=.005) +
# 	geo_shape(corop) +
# 	geo_text("CR_LABEL", cex=1, bg.alpha=00, fontcolor="grey50") +
# 	geo_theme("Doorgaande rijkswegen per corop\n(Gemaakt met R package geo)",bg.color="white", legend.config="", grid.show=TRUE, grid.color="grey75")
# dev.off()


bboxADAM <- unlist(get_polygon_ranges(corop)[[1]][23,])
bboxADAM <- c(105000, 130000, 475000 , 510000)

pdf("../test/NDW_example/rw_poster_ADAM_v2.pdf", width=33.11, height=46.81)
geo_shape(corop, xlim=bboxADAM[1:2], ylim=bboxADAM[3:4], relative=FALSE) +
	geo_fill("white") +
	geo_borders("grey70") +
	geo_shape(rwb_cr) + 
	geo_lines(col="ID", lwd=16, palette=pal, max.categories=52) +
	geo_shape(rw) +
	geo_lines(col="black", lwd=1) +
	geo_shape(loops) +
	geo_bubbles(col="black", size=.005) +
	geo_shape(rwb_cr[!is_N_weg, ]) + 
	geo_text("ID", cex=2, fontface="bold", fontcolor="white", bg.color=rgb(238, 28, 35, maxColorValue=255), bg.alpha=255) +
	geo_shape(rwb_cr[is_N_weg, ]) + 
	geo_text("ID", cex=2, fontcolor="black", bg.color=rgb(255,221, 0, maxColorValue=255), bg.alpha=255) +
	geo_shape(rwb_cr) + 
	geo_text("label", cex=1.5, ymod=-.0075, fontcolor="white", bg.color=rgb(0, 126, 196, maxColorValue=255), bg.alpha=255) +
	geo_shape(corop) +
	geo_text("CR_LABEL", cex=4, bg.alpha=00, fontcolor="grey50") +
	geo_theme("* Doorgaande rijkswegen rond Amsterdam\n
* Afstanden (geschat) per corop\n
* Verkeerslussen weergegeven als zwarte punten\n
* Ruwe rijkswegen weergegeven als zwarte lijntjes\n
* Projectie: rijksdriehoekstelsel\n
* Gemaakt met R package geo",bg.color="grey90", title.position=c(.3,.75), title.cex=3, legend.config="", grid.show=TRUE, grid.color="grey80")
dev.off()
	
	
