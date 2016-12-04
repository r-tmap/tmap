### Bubble Map of Metropolitan Areas

```r
# load spatial data included in the tmap package
data("World", "metro")

# calculate annual growth rate
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

# plot
tm_shape(World) +
	tm_polygons("income_grp", palette = "-Blues", 
		title = "Income class", contrast = 0.7, border.col = "grey30", id = "name") +
	tm_text("iso_a3", size = "AREA", col = "grey30", root=3) +
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

![World map of metropolitan areas](http://www.von-tijn.nl/tijn/research/tmap/bubble.png)

```r
# view
tmap_mode("view")
last_map()
tmap_mode("plot")

# quick plots
qtm(World, fill = "income_grp", text = "iso_a3", text.size = "AREA")
qtm(metro, symbols.size = "pop2010", symbols.col = "growth")
```

![Interactive mode](http://www.von-tijn.nl/tijn/research/tmap/view_metro2.png)
