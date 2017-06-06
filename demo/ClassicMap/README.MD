### 1. Classic World map




![Figure 1. Classic world map, consisting of gridded elevation data, rivers, country borders, and country names.](http://www.von-tijn.nl/tijn/research/classic.png)


Figure 1 illustrates that the *tmap* package is not limited to maps for statistical purposes. A classic styled world map is shown with elevation data, country borders and names, the largest world rivers, projected grid lines and a map compass.

```r
# load elevation raster data, and country polygons
data(land, World)

# convert to Eckert IV projection
land_eck4 <- set_projection(land, "eck4")

# plot
tm_shape(land_eck4) +
  tm_raster("elevation", 
    breaks=c(-Inf, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, Inf),  
    palette = terrain.colors(9), 
    title="Elevation") +
  tm_shape(World) +
    tm_borders("grey20") +
    tm_grid(projection="longlat", labels.size = .5) +
    tm_text("name", size="AREA") +
  tm_compass(position = c(.65, .15), color.light = "grey90") +
  tm_credits("Eckert IV projection", position = c("RIGHT", "BOTTOM")) +
  tm_layout(inner.margins=c(.04,.03, .02, .01), 
    legend.position = c("left", "bottom"), 
    legend.frame = TRUE, 
    bg.color="lightblue", 
    legend.bg.color="lightblue", 
    earth.boundary = TRUE, 
    space.color="grey90") + 
  tm_style_classic()
```


The `SpatialGridDataFrame` object `land` is reprojected from longitude latitude coordinates to the Eckert IV projection, since `is.master = TRUE` in the specification of shape object `World`.

A quick map of elevation data and country borders can be created with the following line of code.

```{r}
qtm(land, raster = "elevation") + qtm(World, fill = NULL)
```

The color classes are by default based on rounded equidistant breaks. Since there are also negative elevation numbers, that is below mean sea level, a diverging color palette is used in this case.
