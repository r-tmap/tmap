# Internal tmap function to add a default value for the layer functions. Deprecated

Internal tmap function to add a default value for the layer functions.
Deprecated. Use
[`tmapSubmitOptions()`](https://r-tmap.github.io/tmap/reference/tmapSubmitOptions.md)
instead.

## Usage

``` r
tmapAddLayerOptions(option, id, value)
```

## Arguments

- option, :

  one of: `"value.const"`, `"value.na"`, `"value.blank"`,
  `"values.var"`, `'values.range'`, `"value.neutral"`, `"scales.var"`

- id:

  name of the visual variable with layer, in the format `"x.y"`, where
  `x` is the visual variable and `y` is the layer. It is also possible
  to set `x` only; then it applies to all layer functions.

- value:

  value
