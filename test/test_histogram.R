data(World)


tm_shape(World) +
	tm_fill("income_grp", legend.hist=TRUE) +
	tm_layout()


tm_shape(World) +
	tm_fill(c("income_grp", "economy"), legend.hist=TRUE) +
	tm_layout()



data(land)

tm_shape(land) +
	tm_raster("trees", legend.hist=TRUE) +
	tm_layout(bg.color = "lightblue")


data(rivers)

names(rivers)

tm_shape(rivers) + 
	tm_lines(col = "type", legend.hist = TRUE)


data(World)
data(metro)
metro$growth <- (metro$X2020 - metro$X2010) / (metro$X2010 * 10) * 100
tm_shape(World) +
	tm_fill("grey70") +
	tm_shape(metro) +
	tm_bubbles("X2010", col = "growth", 
			   border.col = "black", border.alpha = .5, 
			   style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
			   palette="-RdYlBu", contrast=1, 
			   title.size="Metro population", 
			   title.col="Growth rate (%)",
			   legend.hist=TRUE) + 
	tm_layout_World()


data(World)
data(metro)
metro$growth <- (metro$X2020 - metro$X2010) / (metro$X2010 * 10) * 100
tm_shape(World) +
	tm_fill("grey70") +
	tm_shape(metro) +
	tm_bubbles("X2010", col = "growth", 
			   border.col = "black", border.alpha = .5, 
			   style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
			   palette="-RdYlBu", contrast=1, 
			   title.size="Metro population", 
			   title.col="Growth rate (%)",
			   legend.hist=TRUE,
			   legend.hist.z=14) + 
	tm_layout_World()