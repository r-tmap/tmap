# Wrapper functions for using **tmap** in **shiny**

- `tmapOutput()` creates a UI element

- `renderTmap()` renders a tmap map

- `tmapProxy()` updates a tmap map in `view` mode

Adding layers is as usual via the map layer functions like
[`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md).
Removing layers can be done, removing with `tm_remove_layer()`.

## Usage

``` r
renderTmap(
  expr,
  env = parent.frame(),
  quoted = FALSE,
  execOnResize = TRUE,
  mode = NA
)

tmapOutput(outputId, width = "100%", height = 400, mode = NA)

tmapProxy(mapId, session = shiny::getDefaultReactiveDomain(), x, mode = NA)

tm_remove_layer(zindex)
```

## Arguments

- expr:

  A tmap object. A tmap object is created with
  [`qtm()`](https://r-tmap.github.io/tmap/reference/qtm.md) or by
  stacking
  [`tmap-element`](https://r-tmap.github.io/tmap/reference/tmap-element.md)s.

- env:

  The environment in which to evaluate expr

- quoted:

  Is `expr` a quoted expression (with
  [`quote()`](https://rdrr.io/r/base/substitute.html))? This is useful
  if you want to save an expression in a variable

- execOnResize:

  If `TRUE` (default), when the plot is resized, the map is regenerated.
  When set to `FALSE` the map is rescaled: the aspect ratio is kept, but
  the layout will be less desirable.

- mode:

  tmap mode, see
  [`tmap_mode()`](https://r-tmap.github.io/tmap/reference/tmap_mode.md)
  If not defined, the current mode is used

- outputId:

  Output variable to read from

- width, height:

  the width and height of the map

- mapId:

  single-element character vector indicating the output ID of the map to
  modify (if invoked from a Shiny module, the namespace will be added
  automatically)

- session:

  the Shiny session object to which the map belongs; usually the default
  value will suffice

- x:

  the tmap object that specifies the added and removed layers.

- zindex:

  the z index of the pane in which the layer is contained that is going
  to be removed. It is recommended to specify the `zindex` for this
  layer when creating the map (inside `renderTmap()`).

## Details

Two features from tmap are not (yet) supported in Shiny: small multiples
(facets) and colored backgrounds (argument `bg.color` of
[`tm_layout()`](https://r-tmap.github.io/tmap/reference/tm_layout.md)).
Workarounds for small multiples: create multiple independent maps or
specify `as.layers = TRUE` in
[`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md).

## Examples

``` r
if (interactive() && require("shiny")) {

  data(World)
  world_vars <- setdiff(names(World), c("iso_a3", "name", "sovereignt", "geometry"))

  current.mode <- tmap_mode("plot")

  shinyApp(
    ui = fluidPage(
      tmapOutput("map", height = "600px"),
      selectInput("var", "Variable", world_vars)
    ),
    server <- function(input, output, session) {
      output$map <- renderTmap({
        tm_shape(World) +
          tm_polygons(input$var, zindex = 401)
      })
    }
  )

  tmap_mode("view")

  shinyApp(
    ui = fluidPage(
      tmapOutput("map", height = "600px"),
      selectInput("var", "Variable", world_vars)
    ),
    server <- function(input, output, session) {
      output$map <- renderTmap({
        tm_shape(World, id = "iso_a3") +
          tm_polygons(fill = world_vars[1], zindex = 401)
      })
      observe({
        var <- input$var
        tmapProxy("map", session, {
          tm_remove_layer(401) +
            tm_shape(World, id = "iso_a3") +
            tm_polygons(fill = var, zindex = 401)
        })
      })
    },options = list(launch.browser=TRUE)
  )

  tmap_mode(current.mode)
}
```
