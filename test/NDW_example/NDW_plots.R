library(RColorBrewer)
palDark <- c(brewer.pal(9, "Set1"), brewer.pal(8, "Set2"), brewer.pal(8, "Dark2"), brewer.pal(12, "Set3"))


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
	geo_theme("Origine rijkswegen (dunne zwarte lijntjes) +\nVerkeerlslussen (zwarte bolletjes) +\nVereenvoudigde rijkswegen (gekleurde lijnen)")
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