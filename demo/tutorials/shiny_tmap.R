library(shiny)
library(leaflet)
library(tmap)

data(World)

varlist <- setdiff(names(World), "geometry")

runApp(list(
  ui = fluidPage(
    titlePanel("Shiny tmap!"),
    sidebarLayout(
      sidebarPanel(
        selectInput("var", label = "Variable", choices = varlist, selected = "pop_est_dens")  
      ),
      mainPanel(
        leafletOutput("map")
      )
    )
  ),
  server = function(input, output) {
    output$map = renderLeaflet({
      if (packageVersion("tmap") >= 2.0) {
        tm <- tm_basemap(leaflet::providers$Stamen.TerrainBackground, group = "Basemap") +
          tm_shape(World) +
          tm_polygons(input$var) +
          tm_tiles(leaflet::providers$Stamen.TonerLabels, group = "Labels")  
      } else {
        tm <- tm_shape(World) +
          tm_polygons(input$var) + 
          tm_view(basemaps = "Stamen.TerrainBackground")
      }
      
      tmap_leaflet(tm)
    })
  }
))
