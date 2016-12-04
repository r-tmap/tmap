### Two world views

```r
data("World")

tm_shape(World, projection = "robin") +
	tm_polygons(c("HPI", "gdp_cap_est"), auto.palette.mapping = c(FALSE, TRUE), 
		palette = list("RdYlGn", "Purples"),
		style = c("pretty", "fixed"), n = 7, 
		breaks = list(NULL, c(0, 500, 2000, 5000, 10000, 25000, 50000, Inf)),
		title = c("Happy Planet Index", "GDP per capita")) +
tm_format_World(inner.margins = 0.02, frame = FALSE) +
tm_style_natural(earth.boundary = c(-180, 180, -87, 87))  +
tm_legend(position = c("left", "bottom"), bg.color = "gray95", frame = TRUE) +
tm_credits(c("", "Robinson projection"), position = c("RIGHT", "BOTTOM"))

qtm(World, fill = c("HPI", "gdp_cap_est"), style = "natural")
```

![Two choropleths of the World](http://www.von-tijn.nl/tijn/research/tmap/world_facets2.png)
