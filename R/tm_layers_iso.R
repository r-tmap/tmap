#' Map layer: iso (contour)
#'
#' Map layer that draws iso (contour) lines. Stack of [tm_lines()] and [tm_labels_highlighted].
#'
#' @param col `r .doc_vv("col")`
#' @param text `r .doc_vv("text")`
#' @param ... passed on to [tm_lines()] and [tm_labels_highlighted()]. For the text color and alpha transparency of the text labels, please use `text_col` and `text_alpha` instead of `col` and `col_alpha`.
#' @param options_lines The options for [tm_lines()]
#' @param options_labels The options for [tm_labels_highlighted()]
#' @export
tm_iso = function(col = tm_const(),
				  text = tm_vars(x = 1),
				  ...,
				  options_lines = opt_tm_lines(),
				  options_labels = opt_tm_labels()) {
	args = list(...)
	args$options = NULL
	argsL = args[intersect(names(formals("tm_lines")), names(args))]

	nms_col = c("col", "col.scale", "col.legend", "col.chart", "col.free", "col_alpha", "col_alpha.scale", "col_alpha.legend", "col_alpha.chart", "col_alpha.free")

	# update args for labels
	args[nms_col] = NULL
	rename_id = names(args) %in% paste0("text_", nms_col)

	names(args)[rename_id] = substr(names(args)[rename_id], 6, nchar(names(args)[rename_id]))
	argsT = args[intersect(names(formals("tm_labels_highlighted")), names(args))]

	do.call("tm_lines", c(list(col=col, options = options_lines), argsL)) +
	do.call("tm_labels_highlighted", c(list(text=text, options = options_labels), argsT))
}
