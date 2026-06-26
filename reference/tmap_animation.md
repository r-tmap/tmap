# Save tmap animation to file

Create a gif animation or video from an animated tmap plot. First use
[`tm_animate()`](https://r-tmap.github.io/tmap/reference/tm_animate.md)
or
[`tm_animate_fast()`](https://r-tmap.github.io/tmap/reference/tm_animate.md)
to animate the plot, and then apply `tmap_animation()` to save it as a
gif or video file (e.g. mp4).

## Usage

``` r
tmap_animation(
  tm,
  filename = NULL,
  width = NA,
  height = NA,
  dpi = NA,
  outer.margins = NA,
  asp = NULL,
  scale = NA,
  ...
)
```

## Arguments

- tm:

  tmap or a list of tmap objects. If `tm` is a tmap object, animation
  frames should be created using either
  [`tm_animate()`](https://r-tmap.github.io/tmap/reference/tm_animate.md)
  or
  [`tm_animate_fast()`](https://r-tmap.github.io/tmap/reference/tm_animate.md).

- filename:

  filename. If omitted (default), the animation will be shown in the
  viewer or browser. If specified, it should be a gif file or a video
  file (i.e. mp4). The package `gifski` is required to create a gif
  animation. The package `av` (which uses the `FFmpeg` library) is
  required for video formats. The mp4 format is recommended but many
  other video formats are supported, such as wmv, avi, and mkv.

- width, height:

  Dimensions of the animation file (in pixels). Required when `tm` is a
  list, and recommended to specify in advance when `tm` is a `tmap`
  object. If not specified in the latter case, it will be determined by
  the aspect ratio of the map.

- dpi:

  dots per inch. By default 100, but this can be set with the option
  `animation.dpi` in
  [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md).

- outer.margins:

  (passed on to
  [`tmap_save()`](https://r-tmap.github.io/tmap/reference/tmap_save.md))
  overrides the outer.margins argument of
  [`tm_layout()`](https://r-tmap.github.io/tmap/reference/tm_layout.md)
  (unless set to `NA`)

- asp:

  (passed on to
  [`tmap_save()`](https://r-tmap.github.io/tmap/reference/tmap_save.md))
  if specified, it overrides the `asp` argument of
  [`tm_layout()`](https://r-tmap.github.io/tmap/reference/tm_layout.md).
  Tip: set to `0` if map frame should be placed on the edges of the
  image.

- scale:

  (passed on to
  [`tmap_save()`](https://r-tmap.github.io/tmap/reference/tmap_save.md))
  overrides the scale argument of
  [`tm_layout()`](https://r-tmap.github.io/tmap/reference/tm_layout.md)
  (unless set to `NA`)

- ...:

  arguments passed on to
  [`av::av_encode_video()`](https://docs.ropensci.org/av//reference/encoding.html)

## Note

Not only tmap plots are supported, but any series of R plots.

## Examples

``` r
# \donttest{
  if (interactive()) {
    m1 <- tm_shape(NLD_prov) +
      tm_polygons("yellow") +
      tm_animate(frames = "name")

    tmap_animation(m1, filename = "countries.gif")

    m2 <- tm_shape(metro) +
      tm_symbols(size = paste0("pop", seq(1950, 2030, by=10)),
             size.free = FALSE,
             size.legend = tm_legend("Population")) +
      tm_layout(panel.labels = seq(1970, 2030, by=10)) +
      tm_animate()

    tmap_animation(m2, filename = "cities.gif")
  }
# }
```
