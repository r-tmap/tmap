# Deprecated: format

In tmap \< 4.0 it was possible to set shape-specific options, such as
margins and legend position. However, this has become superfluous
because in tmap \> 4.0 legends are by default placed outside the map
area. If needed, a shape-specific set of options can be stored as a
style with tmap_options_save.

## Usage

``` r
tmap_format(format)

tmap_format_add(..., name)

tm_format(format, ...)
```

## Arguments

- format:

  Name of the format

- ...:

  not used

- name:

  Name of the format
