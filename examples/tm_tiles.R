current.mode <- tmap_mode("view")

data(World, metro)

tm_basemap(leaflet::providers$Stamen.Watercolor) +
tm_shape(metro, bbox = "India") + tm_dots(col = "red", group = "Metropolitan areas") +
tm_tiles(paste0("http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
    "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"), group = "Labels")


\dontrun{
# Use tmap optinos to set the basemap and overlay map permanently during the R session:
opts <- tmap_options(basemaps = c(Canvas = "Esri.WorldGrayCanvas", Imagery = "Esri.WorldImagery"),
    overlays = c(Labels = paste0("http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
                                 "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}")))
	
	qtm(World, fill = "HPI", fill.palette = "RdYlGn")

	# restore options
	tmap_options(opts)
}

# restore current mode
tmap_mode(current.mode)
