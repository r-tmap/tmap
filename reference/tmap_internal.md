# Internal methods for tmap extensions

Internal methods for tmap extensions

## Usage

``` r
toTitleCase(x)

tm_element(..., subclass = NULL)

tm_element_list(...)

tmapChartBinned(chart, bin_colors, breaks_def, na.show, x1)

tmapChartBinned2d(chart, chart1, chart2)

tmapChartBinned2d_numcat(chart, chart1, chart2)

tmapChartBinned2d_numnum(chart, chart1, chart2)

tmapChartBinned2d_catcat(chart, chart1, chart2)

tmapChartRaw(chart, na.show, x1, ...)

tmapChartRaw_nna(chart, na.show, x1, ...)

tmapChartNone(chart, na.show, x1, ...)

tmapChartPass(chart, na.show, x1, ...)

tmapChartBinned_categorical(chart, bin_colors, breaks_def, na.show, x1)

tmapChartBinned_numeric(chart, bin_colors, breaks_def, na.show, x1)

tmapGpar(
  fill = NULL,
  col = NULL,
  shape = NULL,
  size = NULL,
  fill_alpha = NULL,
  col_alpha = NULL,
  lty = NULL,
  lwd = NULL,
  linejoin = NULL,
  lineend = NULL,
  ...
)

tmapTpar(...)

tmapGetCompGroupArgs(comp)

lwd_to_mm(value, unit = "bigpts")

.TMAP

.TMAP_LEAFLET

.TMAP_GRID

tmapScaleAsIs(
  x1,
  scale,
  legend,
  chart,
  o,
  aes,
  layer,
  layer_args,
  sortRev,
  bypass_ord,
  submit_legend = TRUE
)

tmapScaleBivariate(
  x1,
  x2,
  scale,
  legend,
  chart,
  o,
  aes,
  layer,
  layer_args,
  sortRev,
  bypass_ord,
  submit_legend = TRUE
)

tmapScaleCategorical(
  x1,
  scale,
  legend,
  chart,
  o,
  aes,
  layer,
  layer_args,
  sortRev,
  bypass_ord,
  submit_legend = TRUE
)

tmapScaleIntervals(
  x1,
  scale,
  legend,
  chart,
  o,
  aes,
  layer,
  layer_args,
  sortRev,
  bypass_ord,
  submit_legend = TRUE
)

tmapScaleRank(
  x1,
  scale,
  legend,
  chart,
  o,
  aes,
  layer,
  layer_args,
  sortRev,
  bypass_ord,
  submit_legend = TRUE
)

tmapUsrCls(x)

format_aes_results(values, ord = NULL, legend, chart)

chart_save(legend)

data_type(x)

data_class(x, check_for_color_class = FALSE, midpoint_enabled = FALSE)

tmapScale(aes, value, scale, legend, chart, free)

tmapScaleAuto(
  x1,
  scale,
  legend,
  chart,
  o,
  aes,
  layer,
  layer_args,
  sortRev,
  bypass_ord,
  submit_legend = TRUE,
  ...
)

tmapValuesCheck_col(x, is_var = TRUE)

tmapValuesCheck_fill(x, is_var = TRUE)

tmapValuesCheck_bgcol(x, is_var = TRUE)

tmapValuesCheck_shape(x, is_var = TRUE)

tmapValuesCheck_size(x, is_var = TRUE)

tmapValuesCheck_area(x, is_var = TRUE)

tmapValuesCheck_lwd(x, is_var = TRUE)

tmapValuesCheck_lty(x, is_var = TRUE)

tmapValuesCheck_xmod(x, is_var = TRUE)

tmapValuesCheck_angle(x, is_var = TRUE)

tmapValuesCheck_col_alpha(x, is_var = TRUE)

tmapValuesCheck_fill_alpha(x, is_var = TRUE)

tmapValuesCheck_bgcol_alpha(x, is_var = TRUE)

tmapValuesCheck_area(x, is_var = TRUE)

tmapValuesCheck_text(x, is_var = TRUE)

tmapValuesCheck_fontface(x, is_var = TRUE)

tmapValuesIsDiv_fill(x)

tmapValuesIsDiv_col(x)

tmapValuesIsDiv_bgcol(x)

tmapValuesIsDiv_size(x)

tmapValuesIsDiv_area(x)

tmapValuesIsDiv_lwd(x)

tmapValuesIsDiv_lty(x)

tmapValuesIsDiv_col_alpha(x)

tmapValuesIsDiv_fill_alpha(x)

tmapValuesIsDiv_bgcol_alpha(x)

tmapValuesIsDiv_area(x)

tmapValuesIsDiv_xmod(x)

tmapValuesIsDiv_angle(x)

tmapValuesIsDiv_shape(x)

tmapValuesIsDiv_text(x)

tmapValuesIsDiv_fontface(x)

tmapValuesRange_fill(x, n, isdiv)

tmapValuesRange_col(x, n, isdiv)

tmapValuesRange_bgcol(x, n, isdiv)

tmapValuesRange_shape(x, n, isdiv)

tmapValuesRange_lty(x, n, isdiv)

tmapValuesRange_size(x, n, isdiv)

tmapValuesRange_area(x, n, isdiv)

tmapValuesRange_lwd(x, n, isdiv)

tmapValuesRange_col_alpha(x, n, isdiv)

tmapValuesRange_fill_alpha(x, n, isdiv)

tmapValuesRange_bgcol_alpha(x, n, isdiv)

tmapValuesRange_area(x, n, isdiv)

tmapValuesRange_xmod(x, n, isdiv)

tmapValuesRange_angle(x, n, isdiv)

tmapValuesRange_text(x, n, isdiv)

tmapValuesRange_fontface(x, n, isdiv)

tmapValuesVV_fill(
  x,
  value.na,
  isdiv,
  n,
  dvalues,
  are_breaks,
  midpoint,
  range,
  scale,
  rep,
  o,
  aes = "fill"
)

tmapValuesVV_col(...)

tmapValuesVV_bgcol(...)

tmapValuesVV_shape(
  x,
  value.na,
  isdiv,
  n,
  dvalues,
  are_breaks,
  midpoint,
  range,
  scale,
  rep,
  o
)

tmapValuesVV_lty(
  x,
  value.na,
  isdiv,
  n,
  dvalues,
  are_breaks,
  midpoint,
  range,
  scale,
  rep,
  o
)

tmapValuesVV_size(
  x,
  value.na,
  isdiv,
  n,
  dvalues,
  are_breaks,
  midpoint,
  range,
  scale,
  rep,
  o
)

tmapValuesVV_area(...)

tmapValuesVV_lwd(...)

tmapValuesVV_col_alpha(...)

tmapValuesVV_fill_alpha(...)

tmapValuesVV_bgcol_alpha(...)

tmapValuesVV_area(...)

tmapValuesVV_xmod(...)

tmapValuesVV_angle(...)

tmapValuesVV_text(
  x,
  value.na,
  isdiv,
  n,
  dvalues,
  are_breaks,
  midpoint,
  range,
  scale,
  rep,
  o
)

tmapValuesVV_fontface(
  x,
  value.na,
  isdiv,
  n,
  dvalues,
  are_breaks,
  midpoint,
  range,
  scale,
  rep,
  o
)

tmapValuesSubmit_col(x, args)

tmapValuesSubmit_fill(x, args)

tmapValuesSubmit_bgcol(x, args)

tmapValuesSubmit_size(x, args)

tmapValuesSubmit_area(x, args)

tmapValuesSubmit_xmod(x, args)

tmapValuesSubmit_ymod(x, args)

tmapValuesSubmit_angle(x, args)

tmapValuesSubmit_lwd(x, args)

tmapValuesSubmit_lty(x, args)

tmapValuesSubmit_shape(x, args)

tmapValuesSubmit_col_alpha(x, args)

tmapValuesSubmit_fill_alpha(x, args)

tmapValuesSubmit_bgcol_alpha(x, args)

tmapValuesSubmit_text(x, args)

tmapValuesSubmit_fontface(x, args)

tmapValuesScale_col(x, scale)

tmapValuesScale_fill(x, scale)

tmapValuesScale_bgcol(x, scale)

tmapValuesScale_size(x, scale)

tmapValuesScale_area(x, scale)

tmapValuesScale_lwd(x, scale)

tmapValuesScale_lty(x, scale)

tmapValuesScale_shape(x, scale)

tmapValuesScale_col_alpha(x, scale)

tmapValuesScale_fill_alpha(x, scale)

tmapValuesScale_bgcol_alpha(x, scale)

tmapValuesScale_text(x, scale)

tmapValuesScale_fontface(x, scale)

tmapValuesScale_xmod(x, scale)

tmapValuesScale_angle(x, scale)

tmapValuesColorize_col(x, pc)

tmapValuesColorize_fill(x, pc)

tmapValuesColorize_bgcol(x, pc)

tmapValuesColorize_size(x, pc)

tmapValuesColorize_area(x, pc)

tmapValuesColorize_lwd(x, pc)

tmapValuesColorize_lty(x, pc)

tmapValuesColorize_shape(x, pc)

tmapValuesColorize_col_alpha(x, pc)

tmapValuesColorize_fill_alpha(x, pc)

tmapValuesColorize_bgcol_alpha(x, pc)

tmapValuesColorize_text(x, pc)

tmapValuesColorize_fontface(x, pc)

tmapValuesColorize_xmod(x, pc)

tmapValuesColorize_angle(x, pc)

tmapSeq(s, n = NULL)

transform_values(x, lim, rng, power, scale, include.neutral = TRUE)

tmapValuesCVV_fill(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_col(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_bgcol(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_size(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_area(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_lwd(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_col_alpha(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_fill_alpha(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_bgcol_alpha(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_area(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_xmod(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_ymod(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_angle(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_shape(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_lty(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_text(x, value.na, n, range, scale, rep, o)

tmapValuesCVV_fontface(x, value.na, n, range, scale, rep, o)

tmapValuesBVV_fill(x, value.na, m, n, scale, rep, o)

tmapValuesBVV_col(x, value.na, m, n, scale, rep, o)

tmapValuesBVV_bgcol(x, value.na, m, n, scale, rep, o)

tmapValuesCheck_num(x, is_var = TRUE)

tmapValuesIsDiv_num(x)

tmapValuesRange_num(x, n, isdiv)

tmapValuesVV_num(
  x,
  value.na,
  isdiv,
  n,
  dvalues,
  are_breaks,
  midpoint,
  range,
  scale,
  rep,
  o
)

tmapValuesSubmit_num(x, args)

tmapValuesScale_num(x, scale)

tmapValuesColorize_num(x, pc)

tmapValuesCVV_num(x, value.na, n, range, scale, rep, o)

tmapValuesCheck_skip(x, is_var)

tmapValuesIsDiv_skip(x)

tmapValuesRange_skip(x, n, isdiv)

tmapValuesVV_skip(
  x,
  value.na,
  isdiv,
  n,
  dvalues,
  are_breaks,
  midpoint,
  range,
  scale,
  rep,
  o
)

tmapValuesSubmit_skip(x, args)

tmapValuesScale_skip(x, scale)

tmapValuesColorize_skip(x, pc)

tmapValuesCVV_skip(x, value.na, n, range, scale, rep, o)

tmapTransCentroid(
  shpTM,
  xmod = NULL,
  ymod = NULL,
  ord__,
  plot.order,
  args,
  scale
)

tmapTransRaster(shpTM, ord__, plot.order, args)

tmapTransPolygons(shpTM, ord__, plot.order, args, scale)

tmapTransLines(shpTM, ord__, plot.order, args, scale)
```

## Format

An object of class `environment` of length 10.

An object of class `environment` of length 0.

An object of class `environment` of length 1.

## Arguments

- ...:

  args

- subclass:

  subclass

- fill, col, shape, size, fill_alpha, col_alpha, lty, lwd, linejoin,
  lineend:

  visual variables

- scale:

  scale

- args:

  args

- shpTM:

  shpTM

- xmod, ymod:

  xmod and ymod

- ord\_\_:

  ord

- plot.order:

  plot.order
