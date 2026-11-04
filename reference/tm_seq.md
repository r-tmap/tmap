# Specify a numeric sequence

Specify a numeric sequence of visual values, for numeric scales like
[`tm_scale_continuous()`](https://r-tmap.github.io/tmap/reference/tm_scale_continuous.md)
and
[`tm_scale_intervals()`](https://r-tmap.github.io/tmap/reference/tm_scale_intervals.md).
The sequence runs from `from` to `to`, and `power` determines the shape
of the curve in between. This is needed when there is a non-linear
relationship between the numeric data values and the map variables. E.g.
to make the area of bubbles proportional to the data values, the
diameters of the bubbles should be proportional to the square root of
the data values (`power = "sqrt"`). See also
[`tm_scale_continuous_area()`](https://r-tmap.github.io/tmap/reference/tm_scale_continuous_area.md).

## Usage

``` r
tm_seq(
  from = 0,
  to = 1,
  power = c("lin", "sqrt", "sqrt_perceptual", "quadratic")
)
```

## Arguments

- from, to:

  The first and last visual value, default 0 and 1 respectively. Both
  should be non-negative when `power` is not 1.

- power:

  The power component, a number or or one of `"lin"`, `"sqrt"`,
  `"sqrt_perceptual"`, `"quadratic"`, which correspond to 1, 0.5,
  0.5716, 2 respectively. See details.

## Details

The visual values are interpolated in the space where they are
proportional to the data: for `power = 0.5`, the squares of the values
(the areas) are interpolated linearly. So, the visual value at position
`t` (from 0 to 1) is
`(from^(1/power) + t * (to^(1/power) - from^(1/power)))^power`. Note
that `from` and `to` are the first and last visual values themselves
(before tmap 4.5, they were raised to the power `power`).

The perceived area of larger symbols is often underestimated. Flannery
(1971) experimentally derived a method to compensate this for symbols.
This compensation is obtained by using the power exponent of 0.5716
instead of 0.5, or by setting `power` to `"sqrt_perceptual"`.
