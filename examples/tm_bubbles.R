data(World)
data(Europe)
data(metro)
metro$growth <- (metro$X2020 - metro$X2010) / (metro$X2010 * 10) * 100

tm_shape(World) +
	tm_fill("grey70") +
	tm_shape(metro) +
	tm_bubbles("X2010", col = "growth", 
			   border.col = "black", border.alpha = .5, 
			   style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
			   palette="-RdYlBu", contrast=1, title.size="Metro population", title.col="Growth rate (%)") + 
	tm_layout_World()


tm_shape(Europe) +
    tm_borders() +
    tm_fill() +
tm_shape(metro) +
    tm_bubbles(size="X2010", col="purple", size.lim=c(0, 1.2e7), title.size="Metro population") +
	tm_text("name", size="X2010", scale=2, root=3, ymod=-.015 , bg.alpha=0) +
tm_layout_Europe()

