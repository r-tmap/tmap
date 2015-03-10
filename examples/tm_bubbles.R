# World example
data(World)

tm_shape(World) +
    tm_fill() +
    tm_bubbles("pop_est") +
tm_layout_World("World population")

# Europe example
data(Europe)
data(cities)

tm_shape(Europe) +
    tm_borders() +
    tm_fill() +
tm_shape(cities) +
    tm_text("name", cex="pop_max", scale=2, root=3, ymod=-.015, bg.alpha=0) +
    tm_bubbles(size="pop_max", col="capital", size.lim=c(0, 2e7)) +
tm_layout_Europe("Metropolitan population", legend.titles=c(bubble.col="Capital"))


# Netherlands example
data(NLD_muni)
data(NLD_prov)

tm_shape(NLD_prov) +
    tm_borders() +
    tm_fill("name", palette="Pastel1") +
tm_shape(NLD_muni) +
    tm_bubbles(size="population", col="steelblue",style="kmeans") +
tm_layout_NLD(title="Population", legend.digits=0, legend.config="bubble.size", legend.is.portrait=TRUE,
              legend.width=.4, bg.color="white")
