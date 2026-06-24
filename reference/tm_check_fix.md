# Check and fix invalid geometries

Checks whether the geometries of the shapes specified via
[`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md) are
valid. Invalid geometries can cause errors or maps with artifacts.

## Usage

``` r
tm_check_fix()
```

## Value

A `tmap` element.

## Details

This tmap element checks the geometries with
[`sf::st_is_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
and, if needed, applies
[`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html).
If that is unsuccessful, it toggles the s2 backend of the **sf** package
(on or off, depending on whether it was already enabled) and tries
again. If the geometries are still invalid, the map is drawn with a
warning and the invalid geometries are removed.
