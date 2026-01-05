# tmap advanced: options

## Shiny integration

``` r
library(shiny)
```

Three functions take care of integrating tmap maps in shiny:
[`tmapOutput()`](https://r-tmap.github.io/tmap/reference/renderTmap.md),
[`renderTmap()`](https://r-tmap.github.io/tmap/reference/renderTmap.md),
and
[`tmapProxy()`](https://r-tmap.github.io/tmap/reference/renderTmap.md)

## Plot mode

An (almost) minimal example of a choropleth where the data variable is
selected via a select input widget:

``` r
NLD_vars <- setdiff(names(NLD_dist), c("code", "name"))
tmap_mode("plot")
shinyApp(
  ui = fluidPage(
    tmapOutput("map", height = "1000px"),
    selectInput("var", "Variable", NLD_vars)
  ),
  server <- function(input, output, session) {
    output$map <- renderTmap({
      tm_shape(NLD_dist) +
        tm_polygons(input$var, zindex = 401, lwd = 0.5) +
      tm_shape(NLD_muni) +
        tm_borders(lwd = 1, col = "black") +
      tm_shape(NLD_prov) +
        tm_borders(lwd = 2, col = "black") +
      tm_layout(meta.margins = c(0, 0, 0, 0.15)) # fixed margin for the legend
    })
  }, 
  options = list(launch.browser = TRUE)
)
```

![tmap plot mode in
shiny](https://r-tmap.github.io/tmap/reference/figures/shiny_plot.jpg)

tmap plot mode in shiny

Note that setting `meta.margins` in the last tmap line is needed to make
sure the map stays in the same position after rendering. Without this
line (so by default) the horizontal position of the map depends on the
legend width, which in turn depends on the legend title /item labels.

Also note that because the map is redrawn every time the inputs change,
proxy objects via
[`tmapProxy()`](https://r-tmap.github.io/tmap/reference/renderTmap.md)
cannot be used. The are used in `"view"` mode as well see next.

## View mode

``` r
tmap_mode("view")
#> ℹ tmap modes "plot" - "view"
#> ℹ toggle with `tmap::ttm()`
```

When inputs are changed in `"view"` mode, it is preferable that the
focus (zoom and panning location) does not change. That is where
[`tmapProxy()`](https://r-tmap.github.io/tmap/reference/renderTmap.md)
comes into play. It updates the mape. Old layer are removed via
[`tm_remove_layer()`](https://r-tmap.github.io/tmap/reference/renderTmap.md)
and new layer can be added.

It is recommended to:

- Set `zindex` for each map layer, because this determines the plotting
  order (which is not necessarily the order of call anymore, because of
  these removals and additions).
- To use basemaps, transform spatial vector data to `sf` objects in the
  crs (EPSG) 4326 and spatial raster data to `stars` objects in the crs
  (EPSG) 3857. Why? Because these operations take time, albeit less than
  a second. If you let tmap do this job, it happens every time the map
  is updated. In an interactive session, every millisecond counts.
  Therefore it is advisable to do this as one-time operation before
  using tmap.

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
NLD_dist_4326 = st_transform(NLD_dist, 4326)
NLD_muni_4326 = st_transform(NLD_muni, 4326)
NLD_prov_4326 = st_transform(NLD_prov, 4326)
```

``` r
shinyApp(
  ui = fluidPage(
    tmapOutput("map", height = "1000px"),
    selectInput("var", "Variable", NLD_vars)
),
server <- function(input, output, session) {
  output$map <- renderTmap({
    tm = tm_shape(NLD_dist_4326) +
      tm_polygons(
        fill = NLD_vars[1], 
        fill.scale = tm_scale(style = "kmeans", values = "matplotlib.plasma"),
        id = "code", zindex = 401, lwd = 0.5) +
      tm_shape(NLD_muni_4326) +
        tm_borders(lwd = 1, col = "black", zindex = 402) +
      tm_shape(NLD_prov_4326) +
        tm_borders(lwd = 2.5, col = "black", zindex = 403) +
      tm_layout(meta.margins = c(0, 0, 0, 0.3))  # fixed margin for the legend
  })
  observe({
    var <- input$var
    tmapProxy("map", session, {
      tm_remove_layer(401) + 
      tm_shape(NLD_dist_4326) +
        tm_polygons(input$var, 
          fill.scale = tm_scale(style = "kmeans", values = "matplotlib.plasma"),
          id = "code", zindex = 401, lwd = 1)
    })
  })
}, 
options = list(launch.browser = TRUE)
)
```

![tmap view mode in
shiny](https://r-tmap.github.io/tmap/reference/figures/shiny_view.jpg)
Note that we improved the scale that we applied for each variable.
Because most data variables are numeric, and because most of them are
skewed (e.g. dwelling value), we’ve used the general purpose function
[`tm_scale()`](https://r-tmap.github.io/tmap/reference/tm_scale.md) to
set the class interval to `kmeans` and the color palette to
`"matplotlib.plasma"`. Run
[`cols4all::c4a_gui()`](https://cols4all.github.io/reference/c4a_gui.html)
to explore color palettes.
