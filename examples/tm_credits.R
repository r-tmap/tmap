data(NLD_muni, NLD_prov)

tm_shape(NLD_muni) +
    tm_fill(col="population", convert2density=TRUE, 
        style="kmeans", title="Population (per km2)") +
    tm_borders("grey25", alpha=.5) + 
    tm_shape(NLD_prov) +
    tm_borders("grey40", lwd=2) +
tm_format_NLD(bg.color="white", frame = TRUE) +
tm_credits("(c) Statistics Netherlands (CBS) and\nKadaster Nederland", position=c("left", "bottom"))
