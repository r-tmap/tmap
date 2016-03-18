## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, fig.width=8, fig.height=4)
library(tmap)

## ------------------------------------------------------------------------
data(Europe)

## ---- fig.height=5-------------------------------------------------------
qtm(Europe)

## ---- fig.height=5-------------------------------------------------------
qtm(Europe, fill="gdp_cap_est", text="iso_a3", text.size="AREA", root=5, fill.title="GDP per capita", 
	fill.textNA="Non-European countries", format="Europe", style="grey")

## ---- eval=FALSE---------------------------------------------------------
#  tm_shape(Europe) +
#  	tm_fill("gdp_cap_est", textNA="Non-European countries", title="GDP per capita") +
#  	tm_borders() +
#  	tm_text("iso_a3", size="AREA", root=5) +
#  tm_format_Europe() +
#  tm_style_grey()

## ---- fig.width=10-------------------------------------------------------
data(rivers, metro)

tm_shape(Europe) +
    tm_fill() +
    tm_borders() +
tm_shape(rivers) +
    tm_lines() +
tm_shape(metro) +
    tm_text("name", size="pop2010", scale=1, root=4, size.lowerbound = .6, 
        bg.color="white", bg.alpha = .5, 
        auto.placement = TRUE, legend.size.show = FALSE) + 
	tm_bubbles("pop2010", "red", border.col = "black", border.lwd=1, 
		size.lim = c(0, 11e6), sizes.legend = c(1e6, 2e6, 4e6, 6e6, 10e6), 
		title.size="Metropolitan Population") +
tm_shape(Europe) +
	tm_text("iso_a3", size="AREA", col = "gray35", scale=1.5, root=5, 
		size.lowerbound = .40, fontface="bold", case=NA) + 
tm_format_Europe(title="Map of Europe") +
	tm_style_natural()

## ---- fig.width=10, fig.height=3-----------------------------------------
tm_shape(Europe) +
	tm_polygons(c("pop_est_dens", "gdp_cap_est"), style="kmeans", 
        title=c("Population density", "GDP per capita")) +
tm_format_Europe() + 
tm_style_grey()

## ---- fig.width=10-------------------------------------------------------
tm_shape(Europe) +
    tm_polygons("gdp_cap_est", style="kmeans", title="GDP per capita") +
    tm_facets("part") +
tm_style_grey()

## ---- fig.width=10-------------------------------------------------------
tm_shape(Europe[Europe$continent=="Europe",]) +
    tm_fill("part", legend.show = FALSE) +
    tm_facets("name", free.coords=TRUE, drop.units=TRUE)

## ---- fig.width=10-------------------------------------------------------
data(land)
data(World)
pal8 <- c("#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", "#999999", "#E31A1C", "#E6E6E6", "#A6CEE3")
tm_shape(land, ylim = c(-88,88), relative=FALSE) +
    tm_raster("cover_cls", palette = pal8, title="Global Land Cover", legend.hist=TRUE, legend.hist.z=0) +
tm_shape(World) +
    tm_borders() +
tm_format_World(inner.margins=0) +
tm_legend(text.size=1,
		  title.size=1.2,
		  position = c("left","bottom"), 
		  bg.color = "white", 
		  bg.alpha=.2, 
		  frame="gray50", 
		  height=.6, 
		  hist.width=.2,
		  hist.height=.2, 
		  hist.bg.color="gray60", 
		  hist.bg.alpha=.5)

## ---- fig.width=10-------------------------------------------------------
qtm(Europe, style="natural", title="Natural style") # equivalent to: qtm(Europe) + tm_style_natural(title="Natural style")

## ---- fig.width=10-------------------------------------------------------
qtm(Europe, style="cobalt", title="Cobalt style") # equivalent to: qtm(Europe) + tm_style_cobalt(title="Cobalt style")

## ---- fig.width=10-------------------------------------------------------
# make a categorical map
qtm(Europe, fill="economy", title=paste("Map according to style:", tmap_style()))

# change to color-blind-friendly style
current_style <- tmap_style("col_blind")

# make a categorical map
qtm(Europe, fill="economy", title=paste("Map according to style:", tmap_style()))

# change back
tmap_style(current_style)

## ---- fig.width=10-------------------------------------------------------
(tm <- qtm(World) +
tm_layout(outer.margins=c(.05,0,.05,0), 
    inner.margins=c(0,0,.02,0), asp=0))

## ---- fig.width=10-------------------------------------------------------
tm + tm_layout(design.mode=TRUE)

## ---- fig.width=10-------------------------------------------------------
land_eck4 <- set_projection(land, "eck4")

tm_shape(land_eck4) +
	tm_raster("elevation", breaks=c(-Inf, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, Inf),  
		palette = terrain.colors(9), title="Elevation", auto.palette.mapping=FALSE) +
tm_shape(World) +
	tm_borders("grey20") +
	tm_grid(projection="longlat", labels.size = .5) +
	tm_text("name", size="AREA") +
tm_compass(position = c(.65, .15), color.light = "grey90") +
tm_credits("Eckert IV projection", position = c(.85, 0)) +
tm_style_classic(inner.margins=c(.04,.03, .02, .01), legend.position = c("left", "bottom"), 
	legend.frame = TRUE, bg.color="lightblue", legend.bg.color="lightblue", 
	earth.boundary = TRUE, space.color="grey90")

## ------------------------------------------------------------------------
tm <- tm_shape(World) +
	tm_fill("well_being", id="name", title="Well-being") +
	tm_format_World()

## ------------------------------------------------------------------------
save_tmap(tm, "World_map.png", width=1920, height=1080)

## ------------------------------------------------------------------------
save_tmap(tm, "World_map.html")

## ---- fig.height=4-------------------------------------------------------
tm_shape(Europe[Europe$name=="Austria", ]) +
	tm_polygons()

## ---- fig.height=3-------------------------------------------------------
data(World)

rivers$constant <- factor("Rivers")
tm_shape(World) +
	tm_fill() +
tm_shape(rivers) +
	tm_lines(col="constant", palette="dodgerblue3", title.col="World map") +
tm_format_World()

## ---- fig.height=5-------------------------------------------------------
tm_shape(World, bbox = "India") +
	tm_polygons("MAP_COLORS", palette="Pastel2") +
tm_shape(metro) +
	tm_bubbles("pop2010", title.size = "Population") +
	tm_text("name", size = "pop2010", legend.size.show = FALSE, root=8, size.lowerbound = .7, auto.placement = TRUE)

