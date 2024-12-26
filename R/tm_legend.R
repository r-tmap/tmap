#' Legend
#'
#' Legend specification
#'
#' @param title Legend title
#' @param show Show legend?
#' @param orientation Orientation of the legend: `"portrait"` or `"landscape"`
#' @param design Legend design `"standard"`. No other designs implemented yet.
#' @param reverse Should the legend be reversed?
#' @param na.show Show `NA` values in legend?
#' @param width Width of the legend. Units are 'text line heights'. In case a negative number is specified, the units are (approximate) pixels. The relation between these two is configured via the option `absolute_fontsize`.
#' @param height Height of the legend. Units are 'text line heights'. In case a negative number is specified, the units are (approximate) pixels. The relation between these two is configured via the option `absolute_fontsize`.
#' @inheritParams tm_title
#' @param title.color `r .doc_opt("legend.title.color")`
#' @param title.size `r .doc_opt("legend.title.size")`
#' @param title.fontface `r .doc_opt("legend.title.fontface")`
#' @param title.fontfamily `r .doc_opt("legend.title.fontfamily")`
#' @param title.padding `r .doc_opt("legend.title.padding")`
#' @param title.align `r .doc_opt("legend.title.align")`
#' @param text.color `r .doc_opt("legend.text.color")`
#' @param text.size `r .doc_opt("legend.text.size")`
#' @param text.fontface `r .doc_opt("legend.text.fontface")`
#' @param text.fontfamily `r .doc_opt("legend.text.fontfamily")`
#' @param format Not used anymore: use the format argument of the `tm_scale_*()` functions instead.
#' @param bg.color `r .doc_opt("legend.bg.color")`
#' @param bg.alpha `r .doc_opt("legend.bg.alpha")`
#' @param absolute_fontsize `r .doc_opt("legend.absolute_fontsize")`
#' @param item.height `r .doc_opt("legend.item.height")`
#' @param item.width `r .doc_opt("legend.item.width")`
#' @param item.space `r .doc_opt("legend.item.space")`
#' @param item.na.height `r .doc_opt("legend.na item.height")`
#' @param item.na.width `r .doc_opt("legend.na item.width")`
#' @param item.na.space `r .doc_opt("legend.na item.space")`
#' @param item.shape `r .doc_opt("legend.item.shape")`
#' @param ticks List of vectors of size 2 that determines the horizontal tick mark lines (for portrait legends). The values are the y-values of begin and endpoint of each tick mark.
#' @param ticks.disable.na Remove ticks for NA values
#' @param ticks.col `r .doc_opt("legend.ticks.col")`
#' @param ticks.lwd `r .doc_opt("legend.ticks.lwd")`
#' @param margins `r .doc_opt("legend.margins")`
#' @param item_text.margin `r .doc_opt("legend.item_text.margin")`
#' @param ... visual values, e.g. `col`, `fill`, `lwd`, can be specified. If so, they overrule the default visual values, which are determined by the drawn map objects (e.g. polygons)
#' @param variable visual (or transformation) variable to combine the legend with: e.g. `"fill"` or `"size"`
#' @return A tm_legend component
#' @export
#' @example ./examples/tm_legend.R
tm_legend = function(title,
					 show,
					 orientation,
					 design,
					 reverse,
					 na.show,
					 position,
					 width,
					 height,
					 stack,
					 z,
					 group.frame,
					 resize_as_group,
					 title.color,
					 title.size,
					 title.fontface,
					 title.fontfamily,
					 title.padding,
					 title.align,
					 text.color,
					 text.size,
					 text.fontface,
					 text.fontfamily,
					 format,
					 frame,
					 frame.lwd,
					 frame.r,
					 bg.color,
					 bg.alpha,
					 absolute_fontsize,
					 item.height,
					 item.width,
					 item.space,
					 item.na.height,
					 item.na.width,
					 item.na.space,
					 item.shape,
					 ticks,
					 ticks.disable.na,
					 ticks.col,
					 ticks.lwd,
					 margins,
					 item_text.margin,
					 ...) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	if (!("title" %in% (names(args)))) args$title = NA
	if (!("xlab" %in% (names(args)))) args$xlab = NA
	if (!("ylab" %in% (names(args)))) args$ylab = NA
	if (!("z" %in% (names(args)))) args$z = NA_integer_
	structure(args, class = c("tm_legend", "tm_component", "list"))
}

#' @rdname tm_legend
#' @export
tm_legend_hide = function() {
	tm_legend(show = FALSE)
}

#' @rdname tm_legend
#' @export
tm_legend_combine = function(variable) {
	structure(list(FUN = "tmapLegend", title = NA, reverse = FALSE, show = FALSE, aes = variable), class = c("tm_legend", "tm_component", "list"))
}

tm_legend_bivariate = function(xlab,
							   ylab,
							   xlab.color,
							   xlab.size,
							   xlab.fontface,
							   xlab.fontfamily,
							   xlab.padding,
							   xlab.align,
							   ylab.color,
							   ylab.size,
							   ylab.fontface,
							   ylab.fontfamily,
							   ylab.padding,
							   ylab.align,
							   ...) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	if (!("title" %in% (names(args)))) args$title = NA
	if (!("xlab" %in% (names(args)))) args$xlab = NA
	if (!("ylab" %in% (names(args)))) args$ylab = NA
	if (!("z" %in% (names(args)))) args$z = NA_integer_
	args$orientation = "portrait"
	structure(args, class = c("tm_legend", "tm_component", "list"))
}
