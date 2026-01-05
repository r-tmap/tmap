# Draw thematic map

Draw thematic map

## Usage

``` r
# S3 method for class 'tmap'
print(
  x,
  return.asp = FALSE,
  show = TRUE,
  vp = NULL,
  knit = FALSE,
  options = NULL,
  in.shiny = FALSE,
  proxy = FALSE,
  ...
)

# S3 method for class 'tmap'
knit_print(x, ..., options = NULL)
```

## Arguments

- x:

  tmap object.

- return.asp:

  should the aspect ratio be returned?

- show:

  show the map

- vp:

  viewport (for `"plot"` mode)

- knit:

  A logical, should knit?

- options:

  A vector of options

- in.shiny:

  A logical, is the map drawn in **shiny**?

- proxy:

  A logical, if `in.shiny`, is
  [`tmapProxy()`](https://r-tmap.github.io/tmap/reference/renderTmap.md)
  used?

- ...:

  passed on internally (for developers: in `"view"` mode, the proxy
  leaflet object is passed to `tmapLeafletInit`).
