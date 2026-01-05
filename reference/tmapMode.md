# Internal method for submitting a new mode (deprecated)

Internal method for submitting a new mode. Deprecated. Use
[`tmapSubmitOptions()`](https://r-tmap.github.io/tmap/reference/tmapSubmitOptions.md)
instead.

## Usage

``` r
tmapMode(id, name, ..., styleOptions = NULL)
```

## Arguments

- id:

  id of the mode: please use lowercase and one-word. This will be used
  with
  [`tmap_mode()`](https://r-tmap.github.io/tmap/reference/tmap_mode.md).

- name:

  name of the mode: please use title case. This will be used to
  recognize internal functions, e.g. `tmapLeafletInit`

- ...:

  mode specific options
