data(World, NLD_muni, NLD_prov, land, metro)

current.mode <- tmap_mode("plot") # small multiples don't work in view mode

# Facets defined by constant values
tm_shape(World) +
    tm_fill(c("forestgreen", "goldenrod")) +
tm_format("World", title=c("A green world", "A dry world"), bg.color="lightskyblue2", 
    title.position=c("left", "bottom"))

# Facets defined by multiple variables
tm_shape(World) +
    tm_polygons(c("well_being", "life_exp"),
    	style=c("pretty", "fixed"), breaks=list(NULL, c(65,70,75,80,85)),
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

# Facets defined by groupings
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

tm_shape(land) +
    tm_raster("black") +
    tm_facets(by="cover_cls")
}
	
# Facets defined by groupings defined by two variables
\dontrun{
World$HPI3 <- cut(World$HPI, breaks = c(20, 35, 50, 65), 
    labels = c("HPI low", "HPI medium", "HPI high"))
World$GDP3 <- cut(World$gdp_cap_est, breaks = c(0, 5000, 20000, Inf), 
    labels = c("GDP low", "GDP medium", "GDP high"))

tm_shape(World) + 
	tm_fill("HPI3", palette="Dark2", colorNA="grey90", legend.show = FALSE) + 
	tm_facets(c("HPI3", "GDP3"), showNA=FALSE, free.coords = FALSE)

metro$pop1950cat <- cut(metro$pop1950, breaks=c(0.5, 1, 1.5, 2, 3, 5, 10, 40)*1e6)
metro$pop2020cat <- cut(metro$pop2020, breaks=c(0.5, 1, 1.5, 2, 3, 5, 10, 40)*1e6)

tm_shape(World) +
	tm_fill() +
tm_shape(metro) +
tm_dots("red", size = .5) +
	tm_facets(c("pop1950cat", "pop2020cat"), free.coords = FALSE) +
tm_layout(panel.label.rot = c(0, 90), panel.label.size = 2)
}

# example: Meuse data
\dontrun{
require(sf)
require(tmaptools)
data(meuse)
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")
meuse_osm <- read_osm(meuse, ext=1.1)

qtm(meuse_osm) + 
	tm_shape(meuse) +
	tm_bubbles(size=c("cadmium", "copper", "lead", "zinc"),
	    col=c("orange", "orange3", "grey40", "grey70"),
	    border.col="black",
        border.alpha = .75,
        scale=.7)
}

# restore current mode
tmap_mode(current.mode)
