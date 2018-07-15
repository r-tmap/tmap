### Two world views

\subsection{Two world views}\label{exsmall}

Two thematic world maps are depicted in Figure 1 as small multiples. The map at the top shows the Happy Planet Index across countries and the map at the bottom shows the Gross Domestic Product (GDP) per capita. The following code is needed to produce these maps.

![Figure 1. Two choropleths on country level. The top one shows the Happy Planet Index and the bottom one shows the GDP per capita.](http://www.von-tijn.nl/tijn/research/tmap/world_facets2.png)

```r
data("World")

tm_shape(World, projection = "robin") +
  tm_polygons(c("HPI", "gdp_cap_est"),
              palette = list("RdYlGn", "Purples"),
              style = c("pretty", "fixed"), n = 7, 
              breaks = list(NULL, c(0, 500, 2000, 5000, 10000, 25000, 50000, Inf)),
              title = c("Happy Planet Index", "GDP per capita")) +
  tm_style("natural", earth.boundary = c(-180, -87, 180, 87))  +
  tm_format("World", inner.margins = 0.02, frame = FALSE) +
  tm_legend(position = c("left", "bottom"), bg.color = "gray95", frame = TRUE) +
  tm_credits(c("", "Robinson projection"), position = c("RIGHT", "BOTTOM"))
```


By assigning two data variables to an aesthetic, in this case polygon fill color, two small multiples are generated. The call of the function `tm_polygons` shows how other, non-aesthetic, arguments can be specified per small multiple. Arguments that normally take a single value can also take a vector of values, one per small multiple. Likewise, arguments that normally take a vector can also take a list of vectors, one per small multiple.

The arrangement of the small multiples in terms of rows and columns is determined automatically based on the aspect ratios of the map and the graphics device, but can be changed with the function `tm_facets`.

Small multiples can be created with `qtm` in a similar way. The following line of code generates two small multiples of the same data variables, using default values for the non-aesthetic arguments.

```{r}
qtm(World, fill = c("HPI", "gdp_cap_est"), style = "natural")
```
