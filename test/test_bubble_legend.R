
data(NLD_muni)
data(NLD_prov)
data(World)
data(Europe)
data(metro)
data(rivers)
data(land)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

tm1 <- tm_shape(World) +
	tm_fill("income_grp", style="kmeans", palette="-Blues", title="Income class", contrast=.7) +
	tm_borders() +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_shape(metro) +
	tm_bubbles("pop2010", col = "growth", border.col = "black", border.alpha=.25, style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf) ,palette="-RdYlGn", contrast=1, title.size = "Metro population (2010)", title.col="Annual growth rate (%)") + 
	# 	tm_shape(World) +
	# 	tm_text("iso_a3", size="AREA", scale=1.5, alpha = .75) +
	tm_layout_World(outer.margins=0, asp=0, scale=.7, inner.margins=c(0,-.06,.02, -.03), bg.color = "gray80")


tm2 <- tm_shape(World) +
	tm_fill("income_grp", style="kmeans", palette="-Blues", title="Income class", contrast=.7) +
	tm_borders() +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_shape(metro) +
	tm_bubbles("pop2010", col = "growth", border.col = "black", style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf) ,palette="-RdYlGn", contrast=1, title.size = "Metro population (2010)", title.col="Annual growth rate (%)") + 
	# 	tm_shape(World) +
	# 	tm_text("iso_a3", size="AREA", scale=1.5, alpha = .75) +
	tm_layout_World(outer.margins=0, asp=0, scale=.7, inner.margins=c(0,-.06,.02, -.03), bg.color = "gray80")


pdf("../test/circle_borders_partly_transparent.pdf", width=7, height=3.75, pointsize = 12) # 3.98
	print(tm1)
dev.off()

pdf("../test/circle_borders_solid.pdf", width=7, height=3.75, pointsize = 12) # 3.98
	print(tm2)
dev.off()


tm
