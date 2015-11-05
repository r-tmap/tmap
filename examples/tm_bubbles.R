data(World, metro)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

tm_shape(World) +
    tm_fill("grey70") +
tm_shape(metro) +
    tm_bubbles("pop2010", col = "growth", 
        border.col = "black", border.alpha = .5, 
        style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
        palette="-RdYlBu", contrast=1, 
        title.size="Metro population", 
        title.col="Growth rate (%)") + 
tm_format_World()

\dontrun{
x <- sample_dots(World, vars="gdp_md_est", convert2density = TRUE, w = 100000)
tm_shape(x) + 
	tm_dots() + 
tm_layout("World GDP (one dot is 100 billon dollars)", title.position = c("right", "bottom"))
}
