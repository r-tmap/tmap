# Create animation

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
if (FALSE) { # \dontrun{
data(NLD_prov)

m1 <- tm_shape(NLD_prov) + 
        tm_polygons("yellow") +
    tm_facets(along = "name")

tmap_animation(m1, delay=40)

data(World, metro)

m2 <- tm_shape(World, projection = "+proj=eck4", simplify = 0.5) +
          tm_fill() +
      tm_shape(metro) + 
          tm_bubbles(size = paste0("pop", seq(1970, 2030, by=10)),
                 col = "purple",
                 border.col = "black", border.alpha = .5,
                 scale = 2) +
      tm_facets(free.scales.symbol.size = FALSE, nrow=1,ncol=1) + 
      tm_format("World")

tmap_animation(m2, delay=100, outer.margins = 0)

m3 <- lapply(seq(50, 85, by = 5), function(age) {
  World$at_most <- World$life_exp <= age
  World_sel <- World[which((World$life_exp <= age) & (World$life_exp > (age - 5))), ]
  tm_shape(World) +
    tm_polygons("at_most", palette = c("gray95", "gold"), legend.show = FALSE) +
    tm_shape(World_sel) +
    tm_text("name", size = "AREA", root = 5, remove_overlap = TRUE) +
    tm_layout(main.title = paste0("Life expectency at most ", age), frame = FALSE)
})

tmap_animation(m3, width = 1200, height = 600, delay = 100)

m4 <- tm_shape(World) +
  tm_polygons() +
tm_shape(metro) +
  tm_bubbles(col = "red") +
  tm_text("name", ymod = -1) +
tm_facets(by = "name", free.coords = FALSE, nrow = 1, ncol = 1) +
  tm_layout(panel.show = FALSE, frame = FALSE)

tmap_animation(m4, filename = "World_cities.mp4", 
    width=1200, height = 600, fps = 2, outer.margins = 0)
} # }
```
