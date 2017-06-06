### Metropolitan Areas


![Figure 1. World map created with *tmap*. The bottom layer is a choropleth of income class on country level. The top layer is a bubble map of metropolitan areas where the size of the bubbles corresponds to population and the color to the annual growth rate.](http://www.von-tijn.nl/tijn/research/tmap/bubble.png)



The code required to create the map depicted in Figure 1 is the
following. 

```r
# load spatial data included in the tmap package
data("World", "metro")

# calculate annual growth rate
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

# plot
tm_shape(World) +
  tm_polygons("income_grp", palette = "-Blues", 
    title = "Income class", contrast = 0.7, border.col = "gray30", id = "name") +
    tm_text("iso_a3", size = "AREA", col = "gray30", root=3) +
tm_shape(metro) +
  tm_bubbles("pop2010", col = "growth", border.col = "black", 
    border.alpha = 0.5,
    breaks = c(-Inf, 0, 2, 4, 6, Inf) ,
    palette = "-RdYlGn",
    title.size = "Metro population (2010)", 
    title.col = "Annual growth rate (%)",
    id = "name",
    popup.vars=c("pop2010", "pop2020", "growth")) + 
tm_format_World() + 
tm_style_gray()
```
First, two shapes that are included in the *tmap* package are loaded. The shape `World` is a `SpatialPolygonsDataFrame` of the countries of the world. The shape `metro` is a `SpatialPointsDataFrame` containing a population time series for metropolitan areas. In the second line of code, the annual growth rate percentage is calculated.

The first group of the plot call defines the choropleth and adds the country codes using the `World` shape. A bubble map is drawn on top of that using the `metro` shape.

In this example, `World` is served as the master shape, so its
projection, the equal-area Eckert IV projection, is used in this map.
This map can also be drawn in latitude longitude (WGS84) coordinates by
letting the `metro` shape be the master, which can be done by setting `is.master = TRUE`. However, in that case, also the bounding box will be taken from the `metro` shape, which will result in
a map that is cropped too much. Better is to reproject the `World` shape to latitude longitude coordinates by replacing by `tm_shape(World)` with
`tm_shape(World, projection = "longlat")`.

Figure 2 shows the map in view mode. The code to produce this map is the following. After the mode is set to view with `ttm`, the map is redrawn with the function `last_map`.


```r
# view
tmap_mode("view")
last_map()
tmap_mode("plot")
```


![Figure 2. Screenshot of the interactive view mode of the map depicted in Figure 1.](http://www.von-tijn.nl/tijn/research/tmap/view_metro4.jpg)

Text labels in thematic maps often cause occlusion. Common ways to prevent this are using different font sizes, hiding less important text labels, and changing placement of the text labels. These options can be enabled by using the specification  of `tm_text`. In the code chunk that reproduces Figure 1, the polygon area sizes are used to scale the text labels, where `size.lowerbound`, by default $0.4$, determines which labels are shown. Alternatively, the arguments `auto.placement` and `remove.overlap` can be used to change the placement of the text labels. However, in view mode, occlusion is usually less of a problem due to the ability to zoom in. Therefore, text sizes are kept constant in view mode by default, as shown in Figure 2. 


The \proglang{Javascript} library `leaflet` does not support legend items for bubble sizes yet. However, by clicking on a bubble, its data values are shown in a small pop-up window. The argument `id` of the layer element functions `tm_polygons` and `tm_bubbles` are both specified with `"name"`, which is the data variable that contains the names of the countries and the metropolitan areas. These names are included in the pop-up text as titles.

The size of the bubbles are not fixed, but scale proportionally with the zoom level. This behavior can be changed by stetting `tm_view`'s argument `bubble.size.fixed` to `TRUE`.

Both the choropleth and the bubble map can be created quickly with `qtm` as the following two lines of code show.

```r
# quick plots
qtm(World, fill = "income_grp", text = "iso_a3", text.size = "AREA")
qtm(metro, symbols.size = "pop2010", symbols.col = "growth")
```


The map depicted in Figure 1 can also be reproduced with `qtm`. To do this, the two function calls above need to be extended with the remaining arguments, e.g., `fill.palette = "-Blues"` in the first line, and to be stacked with the `+` operator. However, for readability issues, we only recommend `qtm` for quick plotting.
