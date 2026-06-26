# Save tmap to file

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
  another mode. The default format for static plots is png, but this can
  be changed using the option `"output.format"` in
  [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md).
  If `NA` (the default), the file is saved as "tmap01" in the default
  format, and the number incremented if the file already exists.

- device:

  graphic device to use. Either a device function (e.g.,
  [`png`](https://rdrr.io/r/grDevices/png.html) or
  [`cairo_pdf`](https://rdrr.io/r/grDevices/cairo.html)) or a text
  indicating selected graphic device: "pdf", "eps", "svg", "wmf"
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

  additional arguments passed on to the underlying output function (see
  Details). For interactive (html) maps these are passed to
  [`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html)
  or
  [`widgetframe::saveWidgetframe()`](https://rdrr.io/pkg/widgetframe/man/saveWidgetframe.html).
  For static maps they are passed to the graphic device; the most useful
  are:

  `colormodel` (pdf, eps)

  :   Color model of the output. The default is `"srgb"`; use
      `colormodel = "cmyk"` for print workflows that require CMYK. See
      [`grDevices::pdf()`](https://rdrr.io/r/grDevices/pdf.html).

  `compression` (tiff)

  :   Compression method, e.g. `"lzw"`, `"zip"`, or `"jpeg"`. The
      default `"none"` can produce very large files; the lossless
      `"lzw"` or `"zip"` is usually a good choice for publication.

  `quality` (jpg)

  :   JPEG quality, from `0` to `100` (default `75`).

  `bg` (all)

  :   Background color. Use `bg = "transparent"` for a transparent
      background (png, tiff, and svg support transparency; jpg and bmp
      do not).

  `pointsize` (all)

  :   Default point size of text, which scales the text drawn by the
      device.

  `family` (pdf, eps, svg), `fonts` (pdf, eps)

  :   Font family, and fonts to embed in the file.

  `antialias` (png, jpg, bmp, tiff)

  :   Type of anti-aliasing, e.g. `"default"`, `"none"`, `"gray"`, or
      `"subpixel"`. This only applies to cairo-type devices. `png`
      always uses cairo (`type = "cairo-png"`), but `jpg`, `bmp`, and
      `tiff` use the platform default device type, so on a build without
      cairo support `antialias` is ignored for those formats.

## Value

the filename, invisibly, if export is successful.

## Details

Extra arguments (`...`) are passed on to the function that writes the
output. For interactive (html) maps this is
[`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html),
or
[`widgetframe::saveWidgetframe()`](https://rdrr.io/pkg/widgetframe/man/saveWidgetframe.html)
when `in.iframe = TRUE`. For static maps it is the graphic device: the
function supplied to `device`, or, when `device = NULL`, the default
device for the file extension:
[`grDevices::pdf()`](https://rdrr.io/r/grDevices/pdf.html) (`"pdf"`),
[`grDevices::postscript()`](https://rdrr.io/r/grDevices/postscript.html)
(`"eps"`), [`grDevices::svg()`](https://rdrr.io/r/grDevices/cairo.html)
(`"svg"`), [`grDevices::png()`](https://rdrr.io/r/grDevices/png.html)
(`"png"`), [`grDevices::jpeg()`](https://rdrr.io/r/grDevices/png.html)
(`"jpg"`/`"jpeg"`),
[`grDevices::bmp()`](https://rdrr.io/r/grDevices/png.html) (`"bmp"`),
and [`grDevices::tiff()`](https://rdrr.io/r/grDevices/png.html)
(`"tiff"`). The most useful device arguments are listed under `...`.

Note that `filename`, `width`, and `height` are always set by
`tmap_save`, as are `res` and `units` for the raster formats (png, jpg,
bmp, tiff); these cannot be overridden through `...`. Use the `dpi`,
`width`, `height`, and `units` arguments of `tmap_save` instead.

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
      tm_scalebar(position = c("left", "bottom")) +
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
