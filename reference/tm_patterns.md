# Specify fill patterns

Specify fill patterns for the map variable `pattern` (see
[`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md)),
either as constant value or as `values` of a scale, e.g.
[`tm_scale_categorical()`](https://r-tmap.github.io/tmap/reference/tm_scale_categorical.md).
The arguments are vectorized, so one call can specify several patterns.
A plain character vector of pattern types and shortcuts can also be used
instead, as well as a list of `tm_patterns()` calls.

## Usage

``` r
tm_patterns(
  type = "hatch",
  angle = NA,
  random = NA,
  length = NA,
  amplitude = NA,
  grain = NA,
  levels = NA,
  contrast = NA,
  seed = NA
)
```

## Arguments

- type:

  Pattern type, shortcut, or pattern set. See details.

- angle:

  Angle of the motif in degrees, counter-clockwise from horizontal. For
  `"hatch"`, 0 means horizontal lines and 45 lines from lower left to
  upper right. For `"sketch"`, the direction of the pencil strokes.

- random:

  Randomness of the placement of dots, circles, or crosses, between 0
  (regular grid) and 1 (displacement of up to half a grid cell). Each
  grid cell contains one element, so elements are spread evenly,
  although they may touch at high densities. For `"mottle"`, the
  randomness of the shades of the patches, between 0 (the shades follow
  smooth noise, so neighboring patches differ by one shade) and 1 (each
  patch has a random shade).

- length:

  Length of dashes, bricks, or herringbone blocks, relative to the
  spacing.

- amplitude:

  Amplitude of waves and zigzag lines, relative to the spacing.

- grain:

  Size of the relief of `"terrain"`: the largest wavelength, relative to
  the spacing at density 1 (which is 1/20 inch by default, see the
  `scale` argument of
  [`tm_layout()`](https://r-tmap.github.io/tmap/reference/tm_layout.md)).
  By default 1. For `"mottle"`, the size of the patches (the largest are
  12 times as large as the relief of `"terrain"`), and for `"sketch"`,
  the width of the pencil strokes (about half the spacing at density 1).
  The relief cannot be finer than the resolution allows (wavelengths of
  at least 3 pixels), so smaller values only have an effect at high
  resolutions, e.g.
  [`tmap_save()`](https://r-tmap.github.io/tmap/reference/tmap_save.md)
  with 300 dpi.

- levels:

  Number of shades of `"mottle"`, by default 5: the fill color, two
  darker, and two brighter shades (with an even number, the fill color
  itself is not one of them).

- contrast:

  Contrast of `"mottle"`: a factor for the differences between the
  shades, by default 1 (at full strength, the darkest shade is 40%
  `pattern_col` over the fill color, and the brightest 20% white).

- seed:

  Seed (a positive integer) of the random placement of dots, circles, or
  crosses (with `random > 0`), and of the textures `"terrain"`,
  `"mottle"`, and `"sketch"`. Patterns with different seeds have
  elements at different positions, which is useful when several dot
  patterns are drawn on top of each other, e.g. a dot density map with
  one layer per category. By default 1.

## Value

A character vector of class `tmap_patterns` with one pattern
specification per element.

## Details

Pattern types:

|  |  |
|----|----|
| Type | Parameters |
| `"none"` | – |
| `"hatch"` | `angle` (default 45) |
| `"crosshatch"` | `angle` (default 0) |
| `"dots"` | `random` (default 0), `seed` (default 1) |
| `"dashes"` | `angle` (default 0), `length` (default 0.5) |
| `"bricks"` | `angle` (default 0), `length` (default 2) |
| `"herringbone"` | `angle` (default 0), `length` (default 2) |
| `"hexagons"` | `angle` (default 0) |
| `"waves"` | `angle` (default 0), `amplitude` (default 0.25) |
| `"zigzag"` | `angle` (default 0), `amplitude` (default 0.25) |
| `"circles"` | `random` (default 0), `seed` (default 1) |
| `"crosses"` | `random` (default 0), `seed` (default 1) |
| `"noise"` | – |
| `"terrain"` | `grain` (default 1), `seed` (default 1) |
| `"mottle"` | `grain` (default 1), `levels` (default 5), `contrast` (default 1), `random` (default 0), `seed` (default 1) |
| `"sketch"` | `angle` (default 45), `grain` (default 1), `seed` (default 1) |

Shortcuts: `"hatch_up"` (same as `"hatch"`), `"hatch_down"` (angle 135),
`"hatch_horizontal"` (angle 0), `"hatch_vertical"` (angle 90),
`"crosshatch_diagonal"` (crosshatch with angle 45), and `"stipple"`
(dots with `random = 0.7`).

Pattern sets, which are used like color palettes:

|  |  |  |
|----|----|----|
| Set | Patterns | Use |
| `"mixed"` | 14: hatch, dots, hatch_down, bricks, waves, ... | default; patterns that differ most |
| `"classic"` | 8: hatches in four directions, crosshatches, dots, dashes | the classic black-and-white look |
| `"lines"` | 10: hatches, waves, zigzag, dashes, herringbone, crosshatches | line patterns only |
| `"shapes"` | 6: dots, bricks, circles, hexagons, crosses, noise | no line patterns, e.g. to combine with a hatched layer |

Used as `values` of a scale, the first `n` patterns are taken. A `"-"`
prefix reverses the order. With `tm_patterns()`, a set is expanded to
all its patterns, which can be subsetted, e.g.
`tm_patterns("classic")[c(1, 3, 5)]`.

The patterns are shown with
[`tmap_show_values()`](https://r-tmap.github.io/tmap/reference/tmap_show_values.md):
all types and shortcuts with `tmap_show_values("pattern")`, and a set
with e.g. `tmap_show_values("pattern", values = "classic")`.

The spacing between the pattern elements is determined by the map
variable `pattern_density`, the line width or dot size by
`pattern_size`, and the color by `pattern_col`. The parameters `length`
and `amplitude` are relative to that spacing, so the whole motif is
scaled with the density.

The pattern type `"terrain"` is a texture rather than a motif: random
relief, shaded with light from the northwest, which looks like rough
terrain. It is drawn as highlights (white) and shadows (`pattern_col`)
on top of the fill color, which cancel out roughly, so the average color
does not change much. Its strength is determined by `pattern_density`
(between 0, no texture, and 1), which makes it suitable for showing
uncertainty: the rougher, the less certain. `pattern_size` is not used.

Two other textures are meant to show uncertainty about the fill color
rather than about heights:

- `"mottle"` looks like a surface that is painted poorly: large patches
  with sharp edges, painted in a few (`levels`) shades that are slightly
  darker (`pattern_col`) or brighter (white) than the intended color. As
  for `"terrain"`, `pattern_density` is the strength, and `contrast`
  scales the differences between the shades. By default, the shades
  follow smooth noise, so a patch only borders on patches of the next
  darker or brighter shade, which may look like a density surface or
  elevation. With `random = 1`, each patch gets a random shade, so that
  any shades can be neighbors.

- `"sketch"` looks like a sketchy pencil drawing: the fill color is only
  applied in pencil strokes, with white paper in between.
  `pattern_density` determines how much of the paper is visible, from 0
  (fully colored) to 1 (about two thirds paper). `pattern_col` and
  `pattern_size` are not used. Since the paper is drawn on top of the
  fill, it is always white, regardless of the background.

In plot mode, these textures require a graphics device that supports
clipping paths.

## See also

[`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md),
[`tmap_show_values()`](https://r-tmap.github.io/tmap/reference/tmap_show_values.md)

## Examples

``` r
tm_patterns("hatch", angle = c(0, 45, 90, 135))
#> [1] "hatch(angle=0)"   "hatch(angle=45)"  "hatch(angle=90)"  "hatch(angle=135)"
tm_patterns(c("dots", "stipple", "bricks"))
#> [1] "dots(random=0)"           "dots(random=0.7)"        
#> [3] "bricks(angle=0,length=2)"

# as values of a scale
tm_shape(World) +
  tm_polygons(pattern = "economy",
    pattern.scale = tm_scale_categorical(
      values = tm_patterns(c("hatch", "dots", "bricks"))))
#> Warning: [pattern] the number of patterns (3) is smaller than the number of classes (7),
#> so patterns are recycled
```
