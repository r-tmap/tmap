# Specify a numeric sequence

Specify a numeric sequence, for numeric scales like
[`tm_scale_continuous()`](https://r-tmap.github.io/tmap/reference/tm_scale_continuous.md).
This function is needed when there is a non-linear relationship between
the numeric data values and the visual variables. E.g. to make
relationship with the area of bubbles linear, the square root of input
variables should be used to calculate the radius of the bubbles.

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

  The numeric range, default 0 and 1 respectively

- power:

  The power component, a number or or one of `"lin"`, `"sqrt"`,
  `"sqrt_perceptual"`, `"quadratic"`, which correspond to 1, 0.5,
  0.5716, 2 respectively. See details.

## Details

The perceived area of larger symbols is often underestimated. Flannery
(1971) experimentally derived a method to compensate this for symbols.
This compensation is obtained by using the power exponent of 0.5716
instead of 0.5, or by setting `power` to `"sqrt_perceptual"`
