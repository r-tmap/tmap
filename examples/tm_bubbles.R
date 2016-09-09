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

tm_shape(metro) +
	tm_symbols(size = "pop2010", col="pop2010", shape="pop2010") +
tm_layout(legend.outside = TRUE, legend.outside.position = "bottom", legend.stack = "horizontal")


\dontrun{
x <- sample_dots(World, vars="gdp_md_est", convert2density = TRUE, w = 100000)
tm_shape(x) + 
	tm_dots() + 
tm_layout("World GDP (one dot is 100 billon dollars)", title.position = c("right", "bottom"))
}
# TIP: check out these examples in view mode, enabled with tmap_mode("view")

\dontrun{
# plot all available symbol shapes:
library(ggplot2)
ggplot(data.frame(p=c(0:25,32:127))) +
	geom_point(aes(x=p%%16, y=-(p%/%16), shape=p), size=5, fill="red") +
	geom_text(mapping=aes(x=p%%16, y=-(p%/%16+0.25), label=p), size=3) +
	scale_shape_identity() +
	theme(axis.title=element_blank(),
		  axis.text=element_blank(),
		  axis.ticks=element_blank(),
		  panel.background=element_blank())
}