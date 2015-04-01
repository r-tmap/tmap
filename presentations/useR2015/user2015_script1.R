data(NLD_muni)
data(NLD_prov)

qtm(NLD_muni)

library(ggplot2)



NLD_data <- NLD_muni@data

NLD_data$name <- factor(NLD_data$name, levels=NLD_data$name[order(NLD_data$population)])

NLD_data_sel <- head(NLD_data[order(NLD_data$population, decreasing=TRUE), ], 20)

ggplot(NLD_data_sel, aes(y=population, x=name)) + geom_bar(stat="identity") + coord_flip()


qtm(NLD_muni, fill="gold", borders="purple", title="Netherlands")

# p <- par(mar=c(0,0,0,0))
# plot(NLD_muni, col = "gold", border="purple")
# title("Netherlands", line = -2)

qtm(NLD_muni, fill="gold", borders="purple", title="Netherlands")
qtm(NLD_muni, fill="population", convert2density=TRUE, title="Population per km2")
qtm(NLD_muni, fill="population", convert2density=TRUE, fill.style="kmeans", title="Population per km2")

png("../presentations/useR2015/NLD1.png", width=475, height=525, res=96)
	qtm(NLD_muni) +
	tm_layout(outer.margins=0, asp=0, scale=.9)
dev.off()

png("../presentations/useR2015/NLD2.png", width=475, height=525, res=96)
qtm(NLD_muni, fill="gold", borders="purple", title="Netherlands") +
	tm_layout(outer.margins=0, asp=0, scale=.9)
dev.off()

png("../presentations/useR2015/NLD3.png", width=475, height=525, res=96)
qtm(NLD_muni, fill="population", convert2density=TRUE, title="Population per km2") +
	tm_layout(outer.margins=0, asp=0, scale=.9)
dev.off()

png("../presentations/useR2015/NLD4.png", width=475, height=525, res=96)
qtm(NLD_muni, fill="population", convert2density=TRUE, fill.style="kmeans", title="Population per km2") +
	tm_layout(outer.margins=0, asp=0, scale=.9)
dev.off()


png("../presentations/useR2015/NLD5.png", width=475, height=525, res=96)
tm_shape(NLD_muni) +
	tm_borders() +
	tm_fill("population", convert2density = TRUE, style= "kmeans") +
	tm_layout(outer.margins=0, asp=0, scale=.9)
dev.off()

png("../presentations/useR2015/NLD6.png", width=475, height=525, res=96)
tm_shape(NLD_muni) +
	tm_borders(alpha = .5) +
	tm_fill("population", convert2density = TRUE, style= "kmeans") +
tm_shape(NLD_prov) +
	tm_borders(lwd=3) +
	tm_layout(outer.margins=0, asp=0, scale=.9)
dev.off()

png("../presentations/useR2015/NLD7.png", width=475, height=525, res=96)
tm_shape(NLD_muni) +
	tm_borders(alpha = .5) +
	tm_fill("population", convert2density = TRUE, style= "kmeans") +
tm_shape(NLD_prov) +
	tm_borders(lwd=3) +
	tm_text("name", shadow=TRUE, bg.color="white", bg.alpha=.25) +
	tm_layout(outer.margins=0, asp=0, scale=.9) 
dev.off()



png("../presentations/useR2015/NLD8.png", width=525, height=525, res=96)
tm_shape(NLD_muni) +
	tm_borders(alpha = .5) +
	tm_fill("population", convert2density = TRUE, style= "kmeans") +
tm_shape(NLD_prov) +
	tm_borders(lwd=3) +
	tm_text("name", shadow=TRUE, bg.color="white", bg.alpha=.25) +
tm_layout("Population per km2", draw.frame=FALSE, bg.color="white", inner.margins=c(.02, .1, .02, .02)) +
tm_layout(outer.margins=0, asp=0, scale=.9)
dev.off()


data(Europe)


