# Specify icons

Specifies icons from a png images, which can be used as markers in
thematic maps. The function `marker_icon()` is the specification of the
default marker.

## Usage

``` r
tmap_icons(
  file,
  names = NULL,
  width = 48,
  height = 48,
  keep.asp = TRUE,
  just = c("center", "center"),
  merge = NA,
  as.local = TRUE,
  ...
)

marker_icon()
```

## Arguments

- file:

  character value/vector containing the file path(s) or url(s).

- names:

  names to be given to the icons. Useful when icons are assigned to
  factor levels.

- width:

  width of the icon. If `keep.asp`, this is interpreted as the maximum
  width.

- height:

  height of the icon. If `keep.asp`, this is interpreted as the maximum
  height.

- keep.asp:

  keep the aspect ratio of the png image. If `TRUE` and the aspect ratio
  differs from `width/height`, either `width` or `height` is adjusted
  accordingly.

- just:

  justification of the icons relative to the point coordinates. The
  first value specifies horizontal and the second value vertical
  justification. Possible values are: `"left"` , `"right"`, `"center"`,
  `"bottom"`, and `"top"`. Numeric values of 0 specify left alignment
  and 1 right alignment. The default value of `just` is
  `c("center", "center")`.

- merge:

  merge icons to one icon list (see return value)? If `FALSE`, a list is
  created per file. By default `TRUE`, unless `names` are specified.

- as.local:

  if the `file` is a url, should it be saved to local temporary file?

- ...:

  arguments passed on to
  [`leaflet::icons()`](https://rstudio.github.io/leaflet/reference/icons.html).
  When `iconWidth`, `iconHeight`, `iconAnchorX`, and `iconAnchorY` are
  specified, they override `width` and `height`, and `just`.

## Value

icon data (see
[`leaflet::icons()`](https://rstudio.github.io/leaflet/reference/icons.html))

## See also

[`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md)
