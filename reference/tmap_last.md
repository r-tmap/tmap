# Retrieve the last map to be modified or created

Retrieve the last map to be modified or created. Works in the same way
as
[`ggplot2::last_plot()`](https://ggplot2.tidyverse.org/reference/get_last_plot.html),
although there is a difference: `tmap_last()` returns the last call
instead of the stacked
[`tmap-element`](https://r-tmap.github.io/tmap/reference/tmap-element.md)s.

## Usage

``` r
tmap_last()
```

## Value

call

## See also

[`tmap_save()`](https://r-tmap.github.io/tmap/reference/tmap_save.md)
