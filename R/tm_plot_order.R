#' Determine plotting order of features
#' 
#' Determine plotting order of features.
#' 
#' @param aes Visual variable for which the values determine the plotting order.
#'   Example: bubble map where the `"size"` aesthetic is used.
#'   A data variable (say population) is mapped via a continuous scale ([tm_scale_continuous()])
#'   to bubble sizes. The bubbles are plotted in order of size.
#'   How is determined by the other arguments. Use `"DATA"` to keep the same
#'   order as in the data. Another special value are `"AREA"` and `"LENGTH"`
#'   which are preserved for polygons and lines respectively: rather than a data
#'   variable the polygon area / line lengths determines the plotting order.
#' @param reverse Logical that determines whether the visual values are plotted
#'   in reversed order. The visual values (specified with tmap option `"values.var"`)
#'   are by default reversed, so plotted starting from the last value.
#'   In the bubble map example, this means that large bubbles are plotted first,
#'   hence at the bottom.
#' @param na.order Where should features be plotted that have an `NA` value for
#'   (at least) one other aesthetic variable? In the (order) `"mix"`, at the `"bottom"`,
#'   or on `"top"`? In the bubble map example: if fill color is missing for some bubble,
#'   where should those bubbles be plotted?
#' @param null.order Where should non-selected (aka null) features be plotted?
#' @param null.below.na Should null features be plotted below NA features?
#' @export
tm_plot_order = function(aes, reverse = TRUE, na.order = c("mix", "bottom", "top"), null.order = c("bottom", "mix", "top"), null.below.na = TRUE) {
	na.order = match.arg(na.order)
	null.order = match.arg(null.order)
	structure(list(aes = aes, reverse = reverse, na.order = na.order, null.order = null.order, null.below.na = null.below.na), class = "tm_plot_order")
}
