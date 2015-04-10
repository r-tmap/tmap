data(World)
data(Europe)
data(cities)
data(metro)


metro$growth <- (metro$X2020 - metro$X2010) / (metro$X2010 * 10) * 100

tm_shape(World) +
	tm_fill("continent", palette = "Dark2") +
	tm_shape(metro) +
	tm_bubbles("X2010", col = "growth", border.col = "black", border.alpha = .5, style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf) ,palette="-RdYlBu", contrast=1, scale=1.5) + 
	tm_layout_World("Metropolitan areas", legend.titles=c(fill="Continent", bubble.col="Growth rate (%)", bubble.size="Population"), legend.bg.color="white")



tm_shape(World) +
	tm_fill("grey70") +
	tm_shape(metro) +
	tm_bubbles("X2010", col = "growth", border.col = "black", border.alpha = .5, style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf) ,palette="-RdYlBu", contrast=1, scale=1.5) + 
	tm_layout_World("Metropolitan population", legend.titles=c(bubble.col="Growth rate (%)"))



tm_shape(Europe) +
    tm_borders() +
    tm_fill() +
tm_shape(metro) +
    tm_bubbles(size="X2010", col="purple", size.lim=c(0, 1.2e7)) +
	tm_text("name", size="X2010", scale=2, root=3, ymod=-.015, bg.alpha=0) +
tm_layout_Europe("Metropolitan population", legend.titles=c(bubble.col="Capital"))

