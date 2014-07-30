# Europe example
data(Europe)
tm_shape(Europe) +
    tm_fill("gdp_cap_est", style="kmeans", textNA = "Non-European countries") +
    tm_borders() +
    tm_text("iso_a3", cex="AREA", scale=2, bg.alpha=0) +
tm_layout_Europe("GDP per capita")


data(rivers)
data(cities)

tm_shape(Europe) +
    tm_fill("pop_est_dens", style="kmeans", textNA="Non-European countries") +
    tm_borders() +
tm_shape(rivers) +
    tm_lines("dodgerblue3") +
tm_shape(cities) +
    tm_text("name", cex="pop_max", scale=1, ymod=-.02, root=4, cex.lowerbound = .60, 
            bg.color="yellow", bg.alpha = 150) + 
    tm_bubbles("pop_max", "red", border.col = "black", border.lwd=1, size.lim = c(0, 2e7)) +
tm_shape(Europe) +
    tm_text("name", cex="area", scale=1.5, root=8, cex.lowerbound = .40, 
            fontface="bold", case=NA, fontcolor = "gray35") + 
tm_layout_Europe("Map of Europe", 
    legend.titles = c(fill="Country population density (people per km2)", 
                      bubble.size="City Population"))
