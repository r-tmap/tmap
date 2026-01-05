# tmap function to specify labels

tmap function to specify labels used in the scale functions, e.g. via
the argument `label.format` in
[`tm_scale_intervals()`](https://r-tmap.github.io/tmap/reference/tm_scale_intervals.md).

## Usage

``` r
tm_label_format(
  fun,
  scientific,
  format,
  digits,
  interval.disjoint,
  big.num.abbr,
  prefix,
  suffix,
  text.separator,
  text.less.than,
  text.less.than_as.prefix,
  text.or.more,
  text.or.more_as.prefix,
  text.align,
  text.to.columns,
  html.escape,
  ...
)
```

## Arguments

- fun:

  Function to specify the labels. It should take a numeric vector, and
  should return a character vector of the same size. By default it is
  not specified. If specified, the list items `scientific`, `format`,
  and `digits` (see below) are not used.

- scientific:

  Should the labels be formatted scientifically? If so, square brackets
  are used, and the `format` of the numbers is `"g"`. Otherwise,
  `format="f"`, and `text.separator`, `text.less.than`, and
  `text.or.more` are used. Also, the numbers are automatically rounded
  to millions or billions if applicable. By default, `FALSE`

- format:

  By default, `"f"`, i.e. the standard notation `xxx.xxx`, is used. If
  `scientific = TRUE` then `"g"`, which means that numbers are formatted
  scientifically, i.e. `n.dddE+nn` if needed to save space.

- digits:

  Number of digits after the decimal point if `format="f"`, and the
  number of significant digits otherwise. By default `NA`, meaning as
  many as needed to have distinct numbers

- interval.disjoint:

  In case of intervals (see
  [`tm_scale_intervals()`](https://r-tmap.github.io/tmap/reference/tm_scale_intervals.md)),
  should the intervals appear disjoint, e.g. `0 to 999`, `1000 - 1999`,
  `2000 - 2999` (`TRUE`, default), or not: `0 - 1000`, `1000 - 2000`,
  `2000- 3000`.

- big.num.abbr:

  Vector that defines whether and which abbrevations are used for large
  numbers. It is a named numeric vector, where the name indicated the
  abbreviation, and the number the magnitude (in terms on numbers of
  zero). Numbers are only abbrevation when they are large enough. Set it
  to `NA` to disable abbrevations. The default is
  `c("mln" = 6, "bln" = 9)`. For layers where `style` is set to `log10`
  or `log10_pretty`, the default is `NA`.

- prefix:

  Prefix of each number

- suffix:

  Suffix of each number

- text.separator:

  Character string to use to separate numbers in an interval legend
  (default: `"to"`).

- text.less.than:

  Character value(s) to use for 'less than'. Default `"Less than"`. When
  a character vector of length 2 is specified, one for each word, these
  words are aligned when `text.to.columns = TRUE`

- text.less.than_as.prefix:

  Should `text.less.than` be used as prefix?

- text.or.more:

  Character value(s) to use to 'or more'. Default is `"or more"`. When a
  character vector of length 2 is specified, one for each word, these
  words are aligned when `text.to.columns = TRUE`

- text.or.more_as.prefix:

  Should `text.or.more` be used as prefix?

- text.align:

  Not implemented in v4 (yet). Value that determines how the numbers are
  aligned, `"left"`, `"center"` or `"right"`. By default `"left"`.

- text.to.columns:

  Not implemented in v4 (yet). Logical that determines whether the text
  is aligned to three columns (from, text.separator, to). By default
  `FALSE`.

- html.escape:

  Logical that determins whther HTML code is escaped in the popups in
  view mode. By default `TRUE`. If set to `FALSE` HTML code can be
  added, e.g. to added white space via `&nbsp;`.

- ...:

  arguments passed on to
  [`formatC`](https://rdrr.io/r/base/formatc.html)

## Value

list with formatting options
