process_label_format = function(lf, mlf) {
	if (is.function(lf)) lf = list(fun = lf)

	to_be_assigned = setdiff(names(mlf), names(lf))
	big.num.abbr.set = "big.num.abbr" %in% names(lf)
	lf[to_be_assigned] = mlf[to_be_assigned]
	attr(lf, "big.num.abbr.set") = big.num.abbr.set
	lf
}

# process_popup_format = function(gpf, gtlf, vars, show.warnings) {
# 	# check if g$legend.format is list of lists or functions
# 	islist = is.list(gpf) && length(gpf)>0 && is.list(gpf[[1]])
#
# 	if (!islist) {
# 		process_legend_format(gpf, gtlf, nx=1)
# 	} else {
# 		nms = names(gpf)
# 		if (is.na(vars[1])) {
# 			if (show.warnings) warning("popup.vars not specified whereas popup.format is a list", call. = FALSE)
# 			return(process_legend_format(gpf[[1]], gtlf, nx=1))
# 		}
# 		if (!all(nms %in% vars)) stop("popup.format names do not correspond to popup.vars", call. = FALSE)
# 		lapply(vars, function(v) {
# 			if (v %in% nms) {
# 				process_legend_format(gpf[[v]], gtlf, nx=1)
# 			} else {
# 				process_legend_format(list(), gtlf, nx=1)
# 			}
# 		})
# 	}
# }
