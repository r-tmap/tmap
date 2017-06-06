### Crimes in Greater London

(See [the full reproducible R script](https://github.com/mtennekes/tmap/blob/master/demo/LondonCrimes/crimes_in_Greater_London.R))

Data about crime in England, Wales, and Northern Ireland have been made publicly available by Police UK. In this example, we will explore the crimes that are registered by the City of London Police and the Metropolitan Police Service during October 2015. The crime data set, which has been downloaded manually from <https://data.police.uk/data>, is available as a supplementary file of this paper.

The data set `crimes` is a `data.frame` that contains latitude, longitude, and crime type, among other variables. With the following code, a `SpatialPointsDataFrame` is created. In the second code line, records of crimes with unknown location are removed. Next, the `crimes` object is converted to a `SpatialPointsDataFrame` by assigning the coordinate variables. Finally, the map projection is set to the British National Grid, which corresponds to EPSG code 27700.

```{r}
library("sp")
crimes <- crimes[!is.na(crimes$Longitude) & !is.na(crimes$Latitude), ]
coordinates(crimes) <- ~ Longitude + Latitude
crimes <- set_projection(crimes, current.projection = "longlat",
  projection = 27700)
```


A first glance at the data can be done with `qtm`.

Figure 1a. Dot map of crime locations.             |  Figure 1b. Dot map of the crime data.
:-------------------------:|:-------------------------:
![](http://www.von-tijn.nl/tijn/research/tmap/crimes1.png) | ![](http://www.von-tijn.nl/tijn/research/tmap/crimes2.jpg)



```{r}
tmap_mode("plot")
qtm(crimes)
```


The result, a dot map, is shown in Figure 1a. This map tells that the vast majority of the dots are clustered, whereas the other dots can be marked as outliers. In order to locate the outliers, the same map can be executed in viewing mode with `tmap_mode("view")` and `last_map()`. However, since there are over 80,000 points, the interactive viewer will probably be slow. Another approach is to add a basemap in plotting mode. A basemap from OpenStreetMap can be loaded with `read_osm`, as the following code shows. The bounding box of the basemap is set to be identical to the bounding box of `crimes`, but extended with 5 percent. The function `bb` can be used to obtain this bounding box explicitly. The produced map is depicted in Figure 1b.

```{r}
crimes_osm <- read_osm(crimes, ext = 1.05)
qtm(crimes_osm) + qtm(crimes, symbols.col = "red", symbols.size = 0.5)
```

![Figure 2. Map of crimes colored by type.](http://www.von-tijn.nl/tijn/research/tmap/crimes3.png) 


It may be interesting to find out what types of crime were commited outside London. The following code produces the map in Figure 2. The basemap is shown with 50 percent transparency to better highlight the bubbles. From this map, it is clear the most of these outlier crimes are related to violence and sexual offences.

```{r}
qtm(crimes_osm, raster.alpha = 0.5) + 
qtm(crimes, symbols.col= "Crime.type", symbols.size = 0.5) + 
tm_legend(outside = TRUE)
```

In the remaining of this example, we will focus in the crimes committed in Greater London. Therefore, we crop the `crimes` object in the following way.

```{r}
library("rnaturalearth")
regions <- ne_download(scale = "large", type = "states", 
  category = "cultural")
london <- regions[which(regions$region == "Greater London"),]
london <- set_projection(london, projection = 27700)
crimes_london <- crop_shape(crimes, london, polygon =  TRUE)
```

The object `london` is a `SpatialPolygonsDataFrame` of the districts within Greater London. This object is obtained with the package `rnaturalearth`. The function `crop_shape` returns the intersection of the first two arguments if the argument `polygon` is set to `TRUE`. In this case, the intersection is taken of the crime locations and the outer borders of Greater London.

![Figure 3. Dot map of crimes in Greater London.](http://www.von-tijn.nl/tijn/research/tmap/crimes3b.png) 


A quick plot of `crimes_london` with `qtm` still shows a large amount of occlusion. This can be prevented by applying an alpha transparency to the points. In the following code, we apply ten percent alpha transparency. As a result, individual dots are hard to see, but the overall data distribution is more clear. The borders of the districts of Greater London are added for geographical reference.

```{r}
qtm(crimes_london, dots.alpha = 0.1) +
tm_shape(london) + tm_borders()
```


This map, which is shown in Figure 3, clearly illustrates the distribution of crimes in Greater London. However, it is hard to infer the actual density values. This can be done by showing two-dimensional kernel densities instead of dots. The bandwidth of the kernel density estimator determines the smoothness of the kernel and is set to half a kilometer in this case.

```{r}
crime_densities <- smooth_map(crimes_london, bandwidth = 0.5, 
  breaks = c(0, 50, 100, 250, 500, 1000), cover = london)
```

The variable `crime_densities` is a list consisting of a spatial raster of smoothed density values, contour lines, and spatial polygons that represent areas with uniform density values. The latter is used to draw the map depicted in Figure 4. This map is made with publication in mind. Besides the borders of the districts of Greater London, the Thames is also drawn as an additional layer since it is a main natural border between the districts. Spatial data of the Thames is downloaded via the package `rnaturalearth` and cropped using `crop_shape`. With the default specification `polygon = FALSE`, the function `crop_shape` determines the intersection of the first object and the bounding box of the second object. Therefore, the Thames is also drawn outside the borders of Greater London.

```
rivers <- ne_download(scale="large", type="rivers_lake_centerlines", 
  category="physical")
thames <- crop_shape(rivers, london)
```

The following code is used to produce this map in Figure 4.  A map compass and scale bar are added to embellish the map. Further, a gray background is chosen, because of the contrast with the yellow areas.

```{r}
tm_shape(crime_densities$polygons) +
  tm_fill(col = "level", palette = "YlOrRd", 
    title = expression("Crimes per " * km^2)) + 
tm_shape(london) + tm_borders() +
tm_shape(thames) + tm_lines(col = "steelblue", lwd = 4) +
tm_compass(position = c("left", "bottom")) +
tm_scale_bar(position = c("left", "bottom")) + 
tm_style_gray(title = "Crimes in Greater London\nOctober 2015")
```

![Figure 4. Kernel density map of crimes in Greater London.](http://www.von-tijn.nl/tijn/research/tmap/crimes4.png) 

The crimes are classified into fourteen types as Figure 2 already showed. Next, we will analyze the distribution of crimes per type in the City of London. In the following code, the district of the City of London is selected from `london` and assigned to a new `SpatialPolygonsDataFrame` object called `london_city`. An OpenStreetMap basemap is obtained from Stamen-Watercolor, a tile server with artistically designed basemaps. The subset of crimes, that are located in the City of London, are assigned to `crimes_city`.

```{r}
london_city <- london[london$name == "City",  ]
london_osm <- read_osm(london_city, type = "stamen-watercolor", zoom = 13)
crimes_city <- crop_shape(crimes_london, london_city, polygon = TRUE)
```

The following code shows how to create small multiples in which the crimes are split by type. The produced plot is shown in Figure 5.

```{r}
qtm(london_osm) +
qtm(crimes_city, dots.size = 0.2, by = "Crime.type")
```

![Figure 5. Small multiples that show the crimes per type in the City of London.](http://www.von-tijn.nl/tijn/research/tmap/crimes5.png) 


The view mode can be used to explore the crime data into depth. Figure 6 shows an screenshot of the widget. The dots are colors by type, classified into seven groups. Jittering is enabled since many crimes have the exact same location.

```{r}
tmap_mode("view")
tm_shape(crimes_city) +
  tm_dots(jitter = 0.2, col = "Crime.group", palette = "Dark2", 
    popup.vars = TRUE) +
tm_view(alpha = 1,
  basemaps = "Esri.WorldTopoMap")
```

This interactive map can be saved to a stand-alone HTML page with `save_tmap`. 

```{r}
save_tmap(filename = "index.html")
```

![Figure 6. Screenshot of an interactive map that shows the crimes colored by type group.](http://www.von-tijn.nl/tijn/research/tmap/view_crimes2.JPG)
