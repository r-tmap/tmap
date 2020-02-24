data(World, NLD_muni, NLD_prov, land, metro)

current.mode <- tmap_mode("plot")

# CASE 1: Facets defined by constant values
tm_shape(World) +
    tm_fill(c("forestgreen", "goldenrod")) +
tm_format("World", title=c("A green world", "A dry world"), bg.color="lightskyblue2", 
    title.position=c("left", "bottom"))

# CASE 2: Facets defined by multiple variables
tm_shape(World) +
    tm_polygons(c("well_being", "life_exp"),
    	style=c("pretty", "fixed"), breaks=list(NULL, seq(45, 85, by = 5)),
    	palette=list("Oranges", "Purples"),
    	border.col = "black",
        title=c("Well-Being Index", "Life Expectancy")) +
tm_format("World")

\dontrun{
tm_shape(NLD_muni) +
    tm_fill(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"),
        style="kmeans", 
        palette=list("Oranges", "Greens", "Blues", "Purples", "Greys"),
        title=c("Population 0 to 14", "Population 15 to 24", "Population 25 to 44",
            "Population 45 to 64", "Population 65 and older")) +
tm_shape(NLD_prov) +
    tm_borders() +
tm_format("NLD", frame = TRUE, asp=0)
}

# CASE 3: Facets defined by group-by variable(s)
# A group-by variable that divides the objects spatially
tm_shape(NLD_prov) +
    tm_polygons("gold2") +
    tm_facets(by="name")

\dontrun{
tm_shape(NLD_muni) +
    tm_borders() +
    tm_facets(by="province") +
    tm_fill("population", style="kmeans", convert2density = TRUE) +
tm_shape(NLD_prov) +
    tm_borders(lwd=4) +
    tm_facets(by="name")
}

# The objects are divided by a non-spatial variable (e.g. date/time)
if (require(dplyr) && require(tidyr)) {
	metro_long <- metro %>% 
		gather(year, population, -name, -name_long, -iso_a3, -geometry) %>% 
		mutate(year = as.integer(substr(year, 4, 7)))
	
	tm_shape(metro_long) +
		tm_bubbles("population") +
		tm_facets(by = "year")
}
\dontrun{
tm_shape(land) +
	tm_raster("black") +
	tm_facets(by="cover_cls")
}

# Facets defined by two group-by variables
\dontrun{
World$HPI3 <- cut(World$HPI, breaks = c(20, 35, 50, 65), 
    labels = c("HPI low", "HPI medium", "HPI high"))
World$GDP3 <- cut(World$gdp_cap_est, breaks = c(0, 5000, 20000, Inf), 
    labels = c("GDP low", "GDP medium", "GDP high"))

tm_shape(World) + 
	tm_fill("HPI3", palette="Dark2", colorNA="grey90", legend.show = FALSE) + 
	tm_facets(c("HPI3", "GDP3"), showNA=FALSE, free.coords = FALSE, drop.units = FALSE)

metro_edited <- metro %>% 
	mutate(pop1950cat = cut(pop1950, breaks=c(0.5, 1, 1.5, 2, 3, 5, 10, 40)*1e6),
		   pop2020cat = cut(pop2020, breaks=c(0.5, 1, 1.5, 2, 3, 5, 10, 40)*1e6))

tm_shape(World) +
	tm_fill() +
tm_shape(metro_edited) +
tm_dots("red", size = .5) +
	tm_facets(c("pop1950cat", "pop2020cat"), free.coords = FALSE) +
tm_layout(panel.label.rot = c(0, 90), panel.label.size = 2)
}

# restore current mode
tmap_mode(current.mode)
