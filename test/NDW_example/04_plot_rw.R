rw <- get_shape("../test/NDW_example/rijksweg2013.shp")
rw <- set_projection(rw, current.projection="rd")

corop <- get_shape("../test/NDW_example/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")


rwb_cr <- get_shape("../test/NDW_example/rw2013_doorgaand_cr.shp")
loops <- get_shape("../test/NDW_example/loops2013_classified.shp")

library(RColorBrewer)

pal <- c(brewer.pal(9, "Set1")[-9], 
		 brewer.pal(8, "Set2")[-7],
		 brewer.pal(12, "Set3")[-9])

rwb_cr <- rwb_cr[!is.na(rwb_cr$ID), ]

pdf("../test/NDW_example/rw_poster.pdf", width=33.11, height=46.81)
geo_shape(corop) +
	geo_fill("grey70") +
	geo_borders("white") +
  	geo_shape(rwb_cr) + 
  	geo_lines(col="ID", lwd=6, palette=pal, max.categories=52) +
  	geo_text("ID", cex=1) +
 	#geo_shape(rw) +
  	#geo_lines(col="black", lwd=.001) +
	geo_shape(loops) +
	geo_bubbles(col="black", size=.001) +
	geo_shape(corop) +
	geo_text("CR_LABEL", cex=1, bg.alpha=00, fontcolor="grey50") +
	geo_theme("Doorgaande rijkswegen en verkeerslussen per corop\n(Gemaakt met R package geo)",bg.color="white", title.cex=5, legend.config="", grid.show=TRUE, grid.color="grey75")
dev.off()


bboxZL <- unlist(get_polygon_ranges(corop)[[1]][39,])

pdf("../test/NDW_example/rw_poster_ZL.pdf", width=33.11, height=46.81)
geo_shape(corop, xlim=bboxZL[1:2], ylim=bboxZL[3:4], relative=FALSE) +
	geo_fill("grey90") +
	geo_borders("black") +
	geo_shape(rwb_cr) + 
	geo_lines(col="ID", lwd=6, palette=pal, max.categories=52) +
	geo_text("ID", cex=1) +
	geo_shape(rw) +
	geo_lines(col="black", lwd=.5) +
	geo_shape(loops) +
	geo_bubbles(col="black", size=.005) +
	geo_shape(corop) +
	geo_text("CR_LABEL", cex=1, bg.alpha=00, fontcolor="grey50") +
	geo_theme("Doorgaande rijkswegen per corop\n(Gemaakt met R package geo)",bg.color="white", legend.config="", grid.show=TRUE, grid.color="grey75")
dev.off()


bboxADAM <- unlist(get_polygon_ranges(corop)[[1]][23,])
bboxADAM <- c(105000, 130000, 475000 , 510000)

pdf("../test/NDW_example/rw_poster_ADAM.pdf", width=33.11, height=46.81)
geo_shape(corop, xlim=bboxADAM[1:2], ylim=bboxADAM[3:4], relative=FALSE) +
	geo_fill("grey90") +
	geo_borders("black") +
	geo_shape(rwb_cr) + 
	geo_lines(col="ID", lwd=16, palette=pal, max.categories=52) +
	geo_text("ID", cex=3) +
	geo_shape(rw) +
	geo_lines(col="black", lwd=1) +
	geo_shape(loops) +
	geo_bubbles(col="black", size=.01) +
	geo_shape(corop) +
	geo_text("CR_LABEL", cex=5, bg.alpha=00, fontcolor="grey50") +
	geo_theme("",bg.color="white", legend.config="bubble.col", title.cex=5, grid.show=TRUE, grid.color="grey75")
dev.off()
