

data(NLD_muni)
data(NLD_prov)

tm_shape(NLD_muni) +
	tm_borders(alpha = .5) +
	tm_fill("population", convert2density = TRUE, style= "kmeans") +
tm_shape(NLD_prov) +
	tm_borders(lwd=3) +
	tm_text("name", shadow=TRUE, bg.color="white", bg.alpha=.25) +
	tm_layout("Population per km2", draw.frame=FALSE, bg.color="white", inner.margins=c(.02, .1, .02, .02)) +
	tm_layout(outer.margins=0, asp=0, scale=.9)

data(Europe)
data(rivers)

tm_shape(Europe) +
	tm_fill("darkolivegreen3") +
tm_shape(rivers) +
	tm_lines(col="navy", lwd="scalerank", scale=2) +
	tm_layout("Rivers of Europe", legend.show=FALSE)


data(Europe)
qtm(Europe, fill="gdp_cap_est", text = "iso_a3", text.size = "AREA", title="GDP per capita", theme="Europe", fill.textNA="Non-European countries")


data(NLD_muni)
qtm(NLD_muni, fill="population", convert2density=TRUE, fill.style="kmeans", title="Population (per km2)", inner.margins=c(.02,.2,.06,.02))


data(World)
data(metro)

pdf("../presentations/paper/bubbleMap.pdf", width=7.08, height=3.48, pointsize = 12)
metro$growth <- (metro$X2020 - metro$X2010) / (metro$X2010 * 10) * 100
tm_shape(World) +
	tm_fill("income_grp", style="kmeans", palette="-Blues") +
	tm_borders() +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_shape(metro) +
	tm_bubbles("X2010", col = "growth", border.col = "black", border.alpha = .5, style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf) ,palette="-RdYlGn", contrast=1) + 
	tm_layout_World(title="Income and urbanization", legend.titles=c(fill="Income class", bubble.size="Metro population (2010)", bubble.col="Annual growth rate (%)"), legend.hist.show=FALSE, outer.margins=0, scale=.5)
dev.off()



pdf("../presentations/paper/bubbleMap2.pdf", width=7.08, height=3.98, pointsize = 12)
tm_shape(World) +
	tm_fill("income_grp", style="kmeans", palette="-Blues") +
	tm_borders() +
 	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_shape(metro) +
	tm_bubbles("X2010", col = "growth", border.col = "black", border.alpha = .5, style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf) ,palette="-RdYlGn", contrast=1) + 
# 	tm_shape(World) +
# 	tm_text("iso_a3", size="AREA", scale=1.5, alpha = .75) +
	tm_layout_World(title="", legend.titles=c(fill="Income class", bubble.size="Metro population (2010)", bubble.col="Annual growth rate (%)"), legend.hist.show=FALSE, outer.margins=0, scale=.7, inner.margins=c(0,-.06,.02, -.03), legend.bg.color = NA)
dev.off()