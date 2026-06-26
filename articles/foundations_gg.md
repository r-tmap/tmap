# tmap foundations: Grammar of Graphics

The Grammar of Graphics \[1\] is a framework for building statistical
graphics by combining key components like data, aesthetic mappings,
geometric objects, scales, transformations, and statistical summaries.
It provides a systematic, modular approach to creating flexible and
consistent visualizations.

### Components:

- **Data**: The dataset being visualized.
- **Aesthetic Mappings**: How data maps to visual properties (e.g.,
  position, color, size).
- **Geometric Objects**: Shapes representing the data (e.g., points,
  lines, bars).
- **Scales**: Rules linking data values to aesthetic values.
- **Transformations**: Adjustments to data or coordinates (e.g., log
  scales).
- **Statistical Summaries**: Computed data representations (e.g., means,
  trends).
- **Facets**: Layouts for splitting data into subsets for comparison.

It is applied in **ggplot2** \[2\] and also in **tmap**, but slightly
differently.

## tmap

![](foundations_gg_files/figure-html/unnamed-chunk-4-1.png)

## ggplot2

- In ggplot2, the visual variables are defined on plot level (by
  default), but in tmap on layer level. This makes sense, since in
  non-spatial visualizations the x and y variables, the most important
  ones, are shared among the plot layers. However, in spatial
  visualizations the x and y variables are not considered data-driven
  but rather geometry-driven. The other visual variables, such as fill
  and border color, line width, and symbol shape.
- This also applies to the scale funtions: e.g. in ggplot2, the
  [`scale_fill_continuous()`](https://ggplot2.tidyverse.org/reference/scale_colour_continuous.html)
  is defined for the visual variable fill for the whole plot. In tmap,
  the scale functions are mapped 1 to 1 to the visual variables per
  layer:
  `tm_polygons(fill = "my_var", fill.scale = tm_scale_continuous())`

See other vignette (to do: link) that compares tmap with ggplot2 by a
series of examples

\[1\]

Leland. Wilkinson and G. Wills, *The grammar of graphics*, 2nd ed. in
Statistics and computing. New York: Springer, 2005.

\[2\]

H. Wickham, *ggplot2: Elegant graphics for data analysis*.
Springer-Verlag New York, 2016. Available:
<https://ggplot2.tidyverse.org>
