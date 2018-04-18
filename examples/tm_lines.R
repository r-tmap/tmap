data(World, rivers)

qtm(rivers)

\dontrun{
tm_shape(World) +
    tm_fill() +
tm_shape(rivers) +
    tm_lines(col="black", lwd="scalerank", scale=2, legend.lwd.show = FALSE) +
tm_style("cobalt", title = "Rivers of the World") +
tm_format("World")
}
