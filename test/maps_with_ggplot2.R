shp <- getShape("gm", 2010)
shp2 <- getShape("pv", 2010)
regionNL$Gemcode <- paste("GM", sprintf("%04g", regionNL$Gemcode), sep="")


library(ggplot2)
library(RColorBrewer)

shpdf <- fortify(shp, region = "STATCODE")
shpdf2 <- fortify(shp2, region = "STATCODE")

shpdf <- merge(shpdf, regionNL, by.x="id", by.y="Gemcode")

ggplot(shpdf, aes(x = long, y = lat, group = group)) + 
	geom_polygon(aes(fill=factor(Stedgem, levels=5:1)), colour = "white", size = 0.25) +
	geom_polygon(data = shpdf2, colour = "black", fill = NA, size = 1.25) +
	scale_fill_brewer() +
	theme(axis.text.x = element_blank(), axis.title.x=element_blank(), 
		  axis.text.y = element_blank(), axis.title.y=element_blank(),
		  axis.ticks = element_blank(), panel.background = element_blank(), 
		  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		  legend.position="none")



ggplot(shpdf2, aes(x = long, y = lat, group = group)) + 
	geom_polygon(colour = "black", fill = NA, size = 1.25) +
	scale_fill_brewer() +
	theme(axis.text.x = element_blank(), axis.title.x=element_blank(), 
		  axis.text.y = element_blank(), axis.title.y=element_blank(),
		  axis.ticks = element_blank(), panel.background = element_blank(), 
		  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		  legend.position="none")
