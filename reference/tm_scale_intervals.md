# Scales: interval scale

Scales in tmap are configured by the family of functions with prefix
`tm_scale`. Such function should be used for the input of the `.scale`
arguments in the layer functions (e.g. `fill.scale` in
[`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md)).
The function `tm_scale_intervals()` is used for numerical data.

## Usage

``` r
tm_scale_intervals(
  n = 5,
  style = ifelse(is.null(breaks), "pretty", "fixed"),
  style.args = list(),
  breaks = NULL,
  interval.closure = "left",
  label.style = "discrete",
  label.select = TRUE,
  midpoint = NULL,
  as.count = FALSE,
  values = NA,
  values.repeat = FALSE,
  values.range = NA,
  values.scale = NA,
  value.na = NA,
  value.null = NA,
  value.neutral = NA,
  labels = NULL,
  label.na = NA,
  label.null = NA,
  label.format = tm_label_format()
)
```

## Arguments

- n:

  Number of intervals. For some styles (see argument `style` below) it
  is the preferred number rather than the exact number.

- style:

  Method to create intervals. Options are `"cat"`, `"fixed"`, `"sd"`,
  `"equal"`, `"pretty"`, `"quantile"`, `"kmeans"`, `"hclust"`,
  `"bclust"`, `"fisher"`, `"jenks"`, `"dpih"`, `"headtails"`, and
  `"log10_pretty"`. See the details in
  [`classInt::classIntervals()`](https://r-spatial.github.io/classInt/reference/classIntervals.html)
  (extra arguments can be passed on via `style.args`).

- style.args:

  List of extra arguments passed on to
  [`classInt::classIntervals()`](https://r-spatial.github.io/classInt/reference/classIntervals.html).

- breaks:

  Interval breaks (only used and required when `style = "fixed"`)

- interval.closure:

  value that determines whether where the intervals are closed: `"left"`
  or `"right"`. If `as.count = TRUE`, `inverval.closure` is always set
  to `"left"`.

- label.style:

  Either `"discrete"` or `"continuous"`. If `discrete` (default)
  intervals will be labeled. If `continuous`, the legend will be similar
  to a continuous scale legend (but with discrete colors), where the
  breaks are labeled.

- label.select:

  Which labels are shown? A logical vector is expected, which is
  repeated along the number of labels. By default `TRUE`, which means
  all.

- midpoint:

  The data value that is interpreted as the midpoint. By default it is
  set to 0 if negative and positive values are present. Useful when
  values are diverging colors. In that case, the two sides of the color
  palette are assigned to negative respectively positive values. If all
  values are positive or all values are negative, then the midpoint is
  set to `NA`, which means that the value that corresponds to the middle
  color class (see `style`) is mapped to the middle color. If it is
  specified for sequential color palettes (e.g. `"Blues"`), then this
  color palette will be treated as a diverging color palette.

- as.count:

  Should the data variable be processed as a count variable? For
  instance, if `style = "pretty"`, `n = 2`, and the value range of the
  variable is 0 to 10, then the column classes for `as.count = TRUE` are
  0; 1 to 5; 6 to 10 (note that 0 is regarded as an own category)
  whereas for `as.count = FALSE` they are 0 to 5; 5 to 10. Only
  applicable if `style` is `"pretty"`, `"fixed"`, or `"log10_pretty"`.
  By default `FALSE`.

- values:

  (generic scale argument) The visual values. For colors (e.g. `fill` or
  `col` for
  [`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md))
  this is a palette name from the `cols4all` package (see
  [`cols4all::c4a()`](https://cols4all.github.io/reference/c4a.html)) or
  vector of colors, for size (e.g. `size` for `tm_symbols`) these are a
  set of sizes (if two values are specified they are interpret as
  range), for symbol shapes (e.g. `shape` for `tm_symbols`) these are a
  set of symbols, etc. The tmap option `values.var` contains the default
  values per visual variable and in some cases also per data type.

- values.repeat:

  (generic scale argument) Should the values be repeated in case there
  are more categories?

- values.range:

  (generic scale argument) Range of the values. Vector of two numbers
  (both between 0 and 1) where the first determines the minimum and the
  second the maximum. Full range, which means that all values are used,
  is encoded as `c(0, 1)`. For instance, when a gray scale is used for
  color (from black to white), `c(0,1)` means that all colors are used,
  `0.25, 0.75` means that only colors from dark gray to light gray are
  used (more precisely `"gray25"` to `"gray75"`), and `0, 0.5` means
  that only colors are used from black to middle grey (`"grey50"`). When
  only one number is specified, this is interpreted as the second number
  (where the first is set to 0). Default values can be set via the tmap
  option `values.range`.

- values.scale:

  (generic scale argument) Scaling of the values. Only useful for
  size-related visual variables, such as `size` of
  [`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md)
  and `lwd` of
  [`tm_lines()`](https://r-tmap.github.io/tmap/reference/tm_lines.md).

- value.na:

  (generic scale argument) Value used for missing values. See tmap
  option `"value.na"` for defaults per visual variable.

- value.null:

  (generic scale argument) Value used for NULL values. See tmap option
  `"value.null"` for defaults per visual variable. Null data values
  occur when out-of-scope features are shown (e.g. for a map of Europe
  showing a data variable per country, the null values are applied to
  countries outside Europe).

- value.neutral:

  (generic scale argument) Value that can be considered neutral. This is
  used for legends of other visual variables of the same map layer. E.g.
  when both `fill` and `size` are used for
  [`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md)
  (using filled circles), the size legend items are filled with the
  `value.neutral` color from the `fill.scale` scale, and fill legend
  items are bubbles of size `value.neutral` from the `size.scale` scale.

- labels:

  (generic scale argument) Labels

- label.na:

  (generic scale argument) Label for missing values

- label.null:

  (generic scale argument) Label for null (out-of-scope) values

- label.format:

  (generic scale argument) Label formatting. Output of
  [`tm_label_format()`](https://r-tmap.github.io/tmap/reference/tm_label_format.md)

## See also

[tmap basics:
scales](https://r-tmap.github.io/tmap/articles/basics_scales)

[`tm_scale()`](https://r-tmap.github.io/tmap/reference/tm_scale.md)
