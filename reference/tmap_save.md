# Save tmap

Save tmap to a file. This can be either a static plot (e.g. png) or an
interactive map (html).

## Usage

``` r
tmap_save(
  tm = NULL,
  filename = NA,
  device = NULL,
  width = NA,
  height = NA,
  units = NA,
  dpi = NA,
  outer.margins = NA,
  asp = NULL,
  scale = NA,
  insets_tm = NULL,
  insets_vp = NULL,
  add.titles = TRUE,
  in.iframe = FALSE,
  selfcontained = !in.iframe,
  verbose = NULL,
  ...
)
```

## Arguments

- tm:

  tmap object

- filename:

  filename including extension, and optionally the path. The extensions
  pdf, eps, svg, wmf (Windows only), png, jpg, bmp, tiff, and html are
  supported. If the extension is missing, the file will be saved as a
  static plot in `"plot"` mode and as an interactive map (html) in
  `"view"` mode (see details). The default format for static plots is
  png, but this can be changed using the option `"output.format"` in
  [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md).
  If `NA` (the default), the file is saved as "tmap01" in the default
  format, and the number incremented if the file already exists.

- device:

  graphic device to use. Either a device function (e.g.,
  [`png`](https://rdrr.io/r/grDevices/png.html) or `cairo_pdf`) or a
  text indicating selected graphic device: "pdf", "eps", "svg", "wmf"
  (Windows only), "png", "jpg", "bmp", "tiff". If `NULL`, the graphic
  device is guessed based on the `filename` argument.

- height, width:

  The dimensions of the plot (not applicable for html files). Units are
  set with the argument `units`. If one of them is not specified, this
  is calculated using the formula asp = width / height, where asp is the
  estimated aspect ratio of the map. If both are missing, they are set
  such that `width * height` is equal to the option `"output.size"` in
  [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md).
  This is by default 49, meaning that is the map is a square (so aspect
  ratio of 1) both width and height are set to 7.

- units:

  units for width and height (`"in"`, `"cm"`, or `"mm"`). By default,
  pixels (`"px"`) are used if either width or height is set to a value
  greater than 50. Else, the units are inches (`"in"`).

- dpi:

  dots per inch. Only applicable for raster graphics. By default it is
  set to 300, but this can be changed using the option `"output.dpi"` in
  [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md).

- outer.margins:

  overrides the outer.margins argument of
  [`tm_options()`](https://r-tmap.github.io/tmap/reference/tm_options.md)
  (unless set to `NA`)

- asp:

  if specified, it overrides the asp argument of
  [`tm_options()`](https://r-tmap.github.io/tmap/reference/tm_options.md).
  **Tip**: set to `0` if map frame should be placed on the edges of the
  image.

- scale:

  overrides the scale argument of
  [`tm_options()`](https://r-tmap.github.io/tmap/reference/tm_options.md)
  (unless set to `NA`)

- insets_tm:

  tmap object of an inset map, or a list of tmap objects of multiple
  inset maps. The number of tmap objects should be equal to the number
  of viewports specified with `insets_vp`.

- insets_vp:

  [`viewport`](https://rdrr.io/r/grid/viewport.html) of an inset map, or
  a list of [`viewport`](https://rdrr.io/r/grid/viewport.html)s of
  multiple inset maps. The number of viewports should be equal to the
  number of tmap objects specified with `insets_tm`.

- add.titles:

  add titles to leaflet object.

- in.iframe:

  should an interactive map be saved as an iframe? If so, two HTML files
  will be saved; one small parent HTML file with the iframe container,
  and one large child HTML file with the actual widget. See
  [`widgetframe::saveWidgetframe()`](https://rdrr.io/pkg/widgetframe/man/saveWidgetframe.html)
  for details. By default `FALSE`, which means that one large HTML file
  is saved (see
  [saveWidget()](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html)).

- selfcontained:

  when an interactive map is saved, should the resources (e.g.
  JavaScript libraries) be contained in the HTML file? If `FALSE`, they
  are placed in an adjacent directory (see also
  [`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html)).
  Note that the HTML file will often still be large when
  `selfcontained = FALSE`, since the map data (polygons and popups),
  which are also contained in the HTML file, usually take more space
  then the map resources.

- verbose:

  Deprecated. It is now controlled by the tmap option `show.messages`
  (see
  [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md))

- ...:

  Arguments passed on to
  [`htmlwidgets::saveWidget`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html),
  [`widgetframe::saveWidgetframe`](https://rdrr.io/pkg/widgetframe/man/saveWidgetframe.html)

  `widget`

  :   Widget to save

  `file`

  :   File to save HTML into

  `libdir`

  :   Directory to copy HTML dependencies into (defaults to
      filename_files).

  `background`

  :   Text string giving the html background color of the widget.
      Defaults to white.

  `title`

  :   Text to use as the title of the generated page.

  `knitrOptions`

  :   A list of knitr chunk options.

## Value

the filename, invisibly, if export is successful.

## Examples

``` r
if (FALSE) { # \dontrun{
  data(NLD_muni, NLD_prov)
  m <- tm_shape(NLD_muni) +
         tm_fill(col="population", convert2density=TRUE, 
                 style="kmeans", 
                 title=expression("Population (per " * km^2 * ")")) +
         tm_borders("black", alpha=.5) + 
     tm_shape(NLD_prov) +
         tm_borders("grey25", lwd=2) +
    tm_style("classic") +
    tm_format("NLD", inner.margins = c(.02, .15, .06, .15)) + 
      tm_scale_bar(position = c("left", "bottom")) +
      tm_compass(position=c("right", "bottom"))
  
  tmap_save(m, "choropleth.png", height = 7) # height interpreted in inches
  tmap_save(m, "choropleth_icon.png", height = 100, scale = .1) # height interpreted in pixels
  
  data(World)
  m2 <- tm_shape(World) +
    tm_fill("well_being", id="name", title="Well-being") +
    tm_format("World")

  # save image
  tmap_save(m2, "World_map.png", width=1920, height=1080, asp=0)

  # cut left inner margin to make sure Antarctica is snapped to frame
  tmap_save(m2 + tm_layout(inner.margins = c(0, -.1, 0.05, 0.01)), 
        "World_map2.png", width=1920, height=1080, asp=0)
  
  # save interactive plot
  tmap_save(m2, "World_map.html")
} # }
```
