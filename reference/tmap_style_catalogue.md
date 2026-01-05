# Create a style catalogue

Create a style catalogue for each predefined tmap style. The result is a
set of png images, one for each style.

## Usage

``` r
tmap_style_catalogue(path = "./tmap_style_previews", styles = NA)

tmap_style_catalog(path = "./tmap_style_previews", styles = NA)
```

## Arguments

- path:

  path where the png images are stored

- styles:

  vector of styles function names (see
  [`tmap_style()`](https://r-tmap.github.io/tmap/reference/tmap_style.md))
  for which a preview is generated. By default, a preview is generated
  for all loaded styles.
