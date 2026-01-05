# Set or get the default tmap style

Set or get the default tmap style. Without arguments, the current style
is returned. Also the available styles are displayed. When a style is
set, the corresponding tmap options (see
[`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md))
will be set accordingly. The default style (i.e. when loading the
package) is `"white"`.

## Usage

``` r
tmap_style(style)
```

## Arguments

- style:

  Name of the style. When omitted, `tmap_style()` returns the current
  style and also shows all available styles. When the style is
  specified,`tmap_style()` sets the style accordingly. Note that in that
  case, all tmap options (see
  [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md))
  will be reset according to the style definition. See
  [`tm_layout()`](https://r-tmap.github.io/tmap/reference/tm_layout.md)
  for predefined styles, and
  [`tmap_style_catalogue()`](https://r-tmap.github.io/tmap/reference/tmap_style_catalogue.md)
  for creating a catalogue.

## Value

The style before changing

## Details

Note that
[`tm_style()`](https://r-tmap.github.io/tmap/reference/tm_layout.md) is
used within a plot call (so it only affects that plot), whereas
`tmap_style()` sets the style globally.

After loading a style, the options that defined this style (i.e. the
difference with the default `"white"` style) can be obtained by
[`tmap_options_diff()`](https://r-tmap.github.io/tmap/reference/tmap_options.md).

The documentation of
[`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md)
(details and the examples) shows how to create a new style.

## See also

- [`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md)
  for tmap options

- [`tmap_style_catalogue()`](https://r-tmap.github.io/tmap/reference/tmap_style_catalogue.md)
  to create a style catalogue of all available styles.

## Examples

``` r
tmap_style()
#> current tmap style is "white" (tmap default)
#> other available styles are: "gray", "natural", "cobalt", "albatross", "beaver", "bw", "classic", "watercolor", "fancy"
#> tmap v3 styles: "v3" (tmap v3 default), "gray_v3", "natural_v3", "cobalt_v3", "albatross_v3", "beaver_v3", "bw_v3", "classic_v3", "watercolor_v3"

tm_shape(World) + tm_polygons("HPI")


tmap_style("cobalt")
#> style set to "cobalt"
#> other available styles are: "white" (tmap default), "gray", "natural", "albatross", "beaver", "bw", "classic", "watercolor", "fancy"
#> tmap v3 styles: "v3" (tmap v3 default), "gray_v3", "natural_v3", "cobalt_v3", "albatross_v3", "beaver_v3", "bw_v3", "classic_v3", "watercolor_v3"

tm_shape(World) + tm_polygons("HPI")


# for backwards compatibility, the styles of tmap versions 1-3 are also included:

tmap_style("v3")
#> style set to "v3" (tmap v3 default)
#> other available styles are: "white" (tmap default), "gray", "natural", "cobalt", "albatross", "beaver", "bw", "classic", "watercolor", "fancy"
#> tmap v3 styles: "gray_v3", "natural_v3", "cobalt_v3", "albatross_v3", "beaver_v3", "bw_v3", "classic_v3", "watercolor_v3"

tm_shape(World) + tm_polygons("HPI")


tmap_style("cobalt_v3")
#> style set to "cobalt_v3"
#> other available styles are: "white" (tmap default), "gray", "natural", "cobalt", "albatross", "beaver", "bw", "classic", "watercolor", "fancy"
#> tmap v3 styles: "v3" (tmap v3 default), "gray_v3", "natural_v3", "albatross_v3", "beaver_v3", "bw_v3", "classic_v3", "watercolor_v3"

tm_shape(World) + tm_polygons("HPI")
```
