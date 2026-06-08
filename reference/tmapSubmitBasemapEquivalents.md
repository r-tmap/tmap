# Register cross-mode basemap equivalences

Used by tmap mode extensions to declare which basemap providers are
visually equivalent across modes. This lets a basemap be approximated
when a map is reproduced in a different mode, instead of falling back to
the mode default.

## Usage

``` r
tmapSubmitBasemapEquivalents(equivalents, replace = FALSE)
```

## Arguments

- equivalents:

  a named list of concept entries. Each entry is itself a named list
  with mode names (e.g. `plot`, `view`, `maplibre`, `mapbox`) as names
  and provider name(s) as values.

- replace:

  if `TRUE`, replace the whole registry; otherwise the entries are
  merged by concept name (existing names are overwritten), so repeated
  submissions are idempotent and multiple extensions compose.
