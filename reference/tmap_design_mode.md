# Set the design mode

When the so-called "design mode" is enabled, the composition of the plot
is shown explicitly in plot mode. The used color codings is printed in
the console as well as information about plot size and aspect ratio.

## Usage

``` r
tmap_design_mode(design.mode)
```

## Arguments

- design.mode:

  Logical value that determines the design mode. If omitted then the
  design mode is toggled.

## Details

This function sets the global option `tmap.design.mode`. It can be used
as toggle function without arguments.

## See also

[`tmap_options()`](https://r-tmap.github.io/tmap/reference/tmap_options.md)
