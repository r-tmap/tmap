# Map layer: iso (contour)

Map layer that draws iso (contour) lines. Stack of
[`tm_lines()`](https://r-tmap.github.io/tmap/reference/tm_lines.md) and
[tm_labels_highlighted](https://r-tmap.github.io/tmap/reference/tm_text.md).

## Usage

``` r
tm_iso(
  col = tm_const(),
  text = tm_vars(x = 1),
  ...,
  options_lines = opt_tm_lines(),
  options_labels = opt_tm_labels()
)
```

## Arguments

- col:

  Visual variable that determines the color. See details.

- text:

  Visual variable that determines the text. See details.

- ...:

  passed on to
  [`tm_lines()`](https://r-tmap.github.io/tmap/reference/tm_lines.md)
  and
  [`tm_labels_highlighted()`](https://r-tmap.github.io/tmap/reference/tm_text.md).
  For the text color and alpha transparency of the text labels, please
  use `text_col` and `text_alpha` instead of `col` and `col_alpha`.

- options_lines:

  The options for
  [`tm_lines()`](https://r-tmap.github.io/tmap/reference/tm_lines.md)

- options_labels:

  The options for
  [`tm_labels_highlighted()`](https://r-tmap.github.io/tmap/reference/tm_text.md)
