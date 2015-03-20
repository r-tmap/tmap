data(World)
data(Europe)
data(cities)

tm_shape(World) +
    tm_fill() +
tm_shape(cities) +
    tm_bubbles("pop_max", scale=.5) +
tm_layout_World("Cities of the World")

tm_shape(Europe) +
    tm_borders() +
    tm_fill() +
tm_shape(cities) +
    tm_text("name", cex="pop_max", scale=2, root=3, ymod=-.015, bg.alpha=0) +
    tm_bubbles(size="pop_max", col="capital", size.lim=c(0, 2e7)) +
tm_layout_Europe("Metropolitan population", legend.titles=c(bubble.col="Capital"))

