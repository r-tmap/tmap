# Specify an animation

`tm_animate`

## Usage

``` r
tm_animate_fast(
  frames = "VARS__",
  nframes = 60L,
  fps = 24L,
  play = c("loop", "pingpong", "once"),
  dpr = 2,
  ...
)

tm_animate(
  frames = "VARS__",
  nframes = 60L,
  fps = 2L,
  play = c("loop", "pingpong", "once"),
  dpr = 2,
  ...
)
```

## Arguments

- frames:

  group by variable used to create the animation frames. This is similar
  to the `by` argument of
  [`tm_facets_wrap()`](https://r-tmap.github.io/tmap/reference/tm_facets.md).
  Instead of showing facets next to each other, they are shown as
  animation frames. However, under the hood `frames` will be used to
  specify `pages` of
  [`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md).
  This makes it possible to create an animation of regular facets.

- nframes:

  number of animation frames. So far, this only applied experimentally
  in transition map variables. See the extension package tmap.cartogram.

- fps:

  frames per second. Default: 30 for `tm_facets_animate` and 2 for
  `tm_facets_animate_slow`.

- play:

  how should the animation be played? One of `"loop"` (default),
  `"pingpong"`, and `"once"`, where `"loop"` means that the animation
  will loop indefinitely, `"pingpong"` means that it will play forward
  and then backward, and `"once"` means that it will play only once.

- dpr:

  device pixel ratio. The ratio between the physical pixel density of a
  device and its logical pixel density.

- ...:

  passed on to
  [`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md).
  Note that for animated facets, `by` can be specified to create
  animated facet wraps, and `rows` and `cols` to created animated facet
  grids.

## Details

Specify an animation from a tmap plot. This is similar to creating
facets with
[`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md).
The animation subsequently can be exported to a gif or video file (e.g.
mp4) with
[`tmap_animation()`](https://r-tmap.github.io/tmap/reference/tmap_animation.md).
If the tmap plot with `tm_animate()` is printed, the animation will be
previewed. The default `tm_animate()` will show the individual frames
slowly (frame per seconds (fps) set to 2) whereas `tm_animate_fast()`
will show them like a movie (with a fps set to 24).

## See also

[`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.md)
which is the core function, and
[`tmap_animation()`](https://r-tmap.github.io/tmap/reference/tmap_animation.md)
used to save the animation
