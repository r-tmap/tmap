# Leader lines of labels

Specifies the leader lines that connect labels to their features, used
by [`tm_labels()`](https://r-tmap.github.io/tmap/reference/tm_text.md)
for labels that are moved outside (the `"move"` step of
[`tm_placement()`](https://r-tmap.github.io/tmap/reference/tm_placement.md)).
Plot mode only.

## Usage

``` r
tm_leader(
  col = NA,
  lwd = 1,
  lty = "solid",
  type = "straight",
  min.length = 0.5
)
```

## Arguments

- col:

  color of the leader lines. `NA` (default) means the color of the text.

- lwd:

  line width.

- lty:

  line type.

- type:

  `"straight"` (default) or `"elbow"` (a short horizontal segment at the
  label, then straight).

- min.length:

  minimum distance (in line heights) between a label and its feature for
  which a leader line is drawn.

## Value

A `tm_leader` object.

## See also

[`tm_placement()`](https://r-tmap.github.io/tmap/reference/tm_placement.md),
[`tm_labels()`](https://r-tmap.github.io/tmap/reference/tm_text.md)
