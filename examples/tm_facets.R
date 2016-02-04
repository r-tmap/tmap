data(World, Europe, NLD_muni, NLD_prov)

current.mode <- tmap_mode("plot") # small multiples don't work in view mode

# Facets defined by constant values
tm_shape(World) +
    tm_fill(c("forestgreen", "goldenrod")) +
tm_format_World(title=c("A green world", "A dry world"), bg.color="lightskyblue2", 
    title.position=c("left", "bottom"))

# Facets defined by multiple variables
tm_shape(Europe) +
    tm_borders() +
    tm_fill(c("gdp_cap_est", "pop_est_dens"), style="kmeans", 
        title=c("GDP per capita", "Population density")) +
tm_format_Europe()

tm_shape(NLD_muni) +
    tm_fill(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"),
        style="kmeans", 
        palette=list("Oranges", "Greens", "Blues", "Purples", "Greys"),
        title=c("Population 0 to 14", "Population 15 to 24", "Population 25 to 44",
            "Population 45 to 64", "Population 65 and older")) +
tm_shape(NLD_prov) +
    tm_borders() +
tm_format_NLD(frame = TRUE, asp=0)

# Facets defined by groupings
tm_shape(NLD_prov) +
    tm_borders() +
    tm_fill("gold2") +
    tm_facets(by="name") +
    tm_layout()

tm_shape(NLD_prov) +
    tm_fill("gold2") + tm_borders() +
    tm_facets(by="name", free.coords = TRUE, drop.shapes=TRUE) +
tm_layout()

tm_shape(NLD_muni) +
    tm_borders() +
    tm_facets(by="province") +
    tm_fill("population", style="kmeans", convert2density = TRUE) +
tm_shape(NLD_prov) +
    tm_borders(lwd=4) +
    tm_facets(by="name", free.coords=TRUE, drop.shapes=TRUE) +
tm_layout(legend.show = FALSE)

# example: Meuse data
\dontrun{
library(sp)
library(OpenStreetMap)
data(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- get_proj4("rd")

meuse_osm <- read_osm(bb(meuse, ext=1.1, current.projection = "rd", projection = "longlat"))

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
