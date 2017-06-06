### Obesity in the United States

(See [the full reproducible R script](https://github.com/mtennekes/tmap/blob/master/demo/USChoropleth/US_choropleth.R))

In this example, we will explore obesity in the United States. Percentages of adult obesity per county have been published in the Food Environment Atlas~\citep{FEA}. This data is assigned to the object \code{FEA}, which is a \code{data.frame}. The shape with county borders for 2010~\citep{TIGER} has been downloaded manually and assigned to the \code{SpatialPolygonsDataFrame} object \code{US}. The package \pkg{tigris}~\citep{tigris} provides a convenient interface for shapes published by the US Census Bureau that are more recent.

First, we plot the \code{US} shape to ensure it is the correct shape. The result is shown in Figure~\ref{fig:us1}. 

```{r}
qtm(US)
```

![Figure 1. Shape of the counties of the United States.](http://www.von-tijn.nl/tijn/research/tmap/US1.png)


Note that some of the islands of Alaska are drawn at the right-hand side of the map, because they have positive longitude values. The map with coordinates grid lines can be shown with `qtm(US) + tm\_grid()`.

When the data is separate from the shape, which is the case in this example, the first thing to do is to append the data to the shape file. We use the function `append_data` where the data is matched by Federal Information Processing Standard (FIPS) code.


```{r}
US <- append_data(US, FEA, key.shp = "FIPS", key.data = "FIPS",
  ignore.duplicates = TRUE)
```

```
Data contains duplicated keys: 08014
Over coverage: 34 out of 3255 data records were not appended. Run 
over_coverage() to get the corresponding data row numbers and key values.
```


It turns out that there are two identical data records for county 08014 (Broomfield, Colorado). Without setting `ignore.duplicates` to `TRUE`, the duplicated FIPS codes would have caused an error message. In this example, there are 34 unmatched data records, which can be identified as follows.

```{r}
unmatched_data <- over_coverage()
str(unmatched_data)
```

```
List of 4
 $ result: chr "Over coverage: 34 out of 3255 data records were not 
   appended."
 $ call  : chr [1:2] "append_data(shp = US, data = FEA, key.shp = \"FIPS\", 
   key.data = \"FIPS\", " "    ignore.duplicates = TRUE)"
 $ id    : int [1:34] 68 90 93 94 99 339 559 1663 2931 2959 ...
 $ value : chr [1:34] "02010" "02201" "02231" "02232" ...
```

The list item `id` contains the row numbers of the unmatched data records. Inspection of these records with `View(FEA[unmatched_data$id, ])` reveals that the corresponding values of the variable of interest, `PCT_DIABETES_ADULTS10`, are missing. Hence, the over coverage can be safely ignored.

An interactive choropleth of adult obesity can be created with the following code. The produced map, depicted in Figure 2, is already useful for data exploration and, with some minor improvements, also for publication. These improvements include the adjustment of the legend title and the addition of thick state borders. In the remaining of the example, we illustrate these improvements for the plot mode, but they can also be applied in view mode. Adjustments that are specific to the view mode, such as the basemap specification and the overall alpha transparency of the layers on top, can be set with `tm_view`.

```{r}
ttm()
qtm(US, fill = "PCT_OBESE_ADULTS10")
```

![Figure 2. Interactive choropleth of adult obesity percentages per county.](http://www.von-tijn.nl/tijn/research/tmap/US2b.jpg)

Static maps of the United States usually contain insets of Alaska and Hawaii. To accomplish this, the county polygons of Alaska and Hawaii need to be separated from the contiguous United States. In the following code chunk, we create three shapes. The shape `US_cont` consists of county polygons of the contiguous United States. The shapes `US_AK` and `US_HI` represent the counties of Alaska and Hawaii respectively. Furthermore, we simplify each shape in order to reduce the level of detail, which is locally very high, especially along the coastlines. The simplification factor of $0.2$ is chosen by trial and error. For these processing steps, we use the pipe operator `%>%`, which is imported from the `magrittr` package.

```{r}
US_cont <- US %>%
  subset(!STATE %in% c("02","15","72")) %>% 
  simplify_shape(0.2) 
US_AK <- US %>% 
  subset(STATE == "02") %>% 
  simplify_shape(0.2) 
US_HI <- US %>% 
  subset(STATE == "15") %>% 
  simplify_shape(0.2) 
```

Thick borders of aggregated regions, in this example the states, usually improve the readability of choropleths. We create a shape of state polygons by aggregating the county polygons of the contiguous United States.

```{r}
US_states <- US_cont %>% 
  aggregate_map(by = "STATE")
```

The choropleth of the contiguous United States is created as follows. The result is shown in Figure 3.

![Figure 3. Choropleth of the United States on county level with additional state borders. Alaska and Hawaii are placed as map insets.](http://www.von-tijn.nl/tijn/research/tmap/USchoro.png)

```{r}
m_cont <- tm_shape(US_cont, projection = 2163) +
  tm_polygons("PCT_OBESE_ADULTS10", title = "", showNA = TRUE,
  border.col = "gray50", border.alpha = .5) +
tm_shape(US_states) +
  tm_borders(lwd=1, col = "black", alpha = .5) +
tm_credits("Data @ Unites States Department of Agriculture\n
  Shape @ Unites States Census Bureau",
  position = c("right", "bottom")) +
tm_layout(title = "2010 Adult Obesity by County, percent", 
  title.position = c("center", "top"), 
  legend.position = c("right", "bottom"), 
  frame = FALSE, 
  inner.margins = c(0.1, 0.1, 0.05, 0.05))
```

Note that the choropleth of continuous United States is not plotted yet, but assigned to the variable `m_cont, which is a `tmap` object. Likewise, the maps of Alaska and Hawaii are created and assigned to the variables `m_AK` and `m_HI` respectively. For these two maps, we define viewports in which the insets will be printed.

```{r}
library("grid")
vp_AK <- viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3)
vp_HI <- viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1)
```

The following code chunk shows how these maps are printed. The map of contiguous United States is plotted in the normal way. The maps of Alaska and Hawaii are printed as insets in the viewports `vp_AK` and `vp_HI` respectively.

```{r}
tmap_mode("plot")
m_cont
print(m_AK, vp = vp_AK)
print(m_HI, vp = vp_HI)
```

This map is exported to a `png` file in the following code chunk. The width is set to $6.125$ inches, equal to the text width of this journal. The height is not specified. Therefore, it is configured automatically based on the width and the aspect ratio of the map. The `scale` argument determines the overall size of all map objects that are scalable, such as the font size and the line width. Finally, the map insets and the corresponding viewports are specified with the arguments `insets_tm` and `insets_vp` respectively.

```{r}
save_tmap(m_cont, "US_adult_obesity.png", scale = 0.7, width = 6.125, 
  insets_tm = list(m_AK, m_HI), 
  insets_vp = list(vp_AK, vp_HI))
```
