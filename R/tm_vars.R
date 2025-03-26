#' tmap function to specify variables
#'
#' tmap function to specify all variables in the shape object
#'
#' @param x variable names, variable indices, or a dimension name
#' @param dimvalues dimension values
#' @param n if specified the first `n` variables are taken (or the first `n` dimension values)
#' @param multivariate in case multiple variables are specified, should they serve as facets (FALSE) or as a multivariate visual variable?
#' @param animate should the variable(s) be animated? (experimental)
#' @export
tm_vars = function(x = NA, dimvalues = NULL, n = NA, multivariate = FALSE, animate = FALSE) {
	structure(list(x = x, dimvalues = dimvalues, n = n, multivariate = multivariate, animate = animate), class = c("tmapVars", "list"))
}


# process visual variable specification. Can either be tmapVars (output of tm_vars) or a list of values.
tmapVV = function(x) {
	if (is.null(x)) {
		x = structure(list("value.blank"), class = "tmapOption")

	} else if (!is.list(x) && length(x) == 1 && is.na(x)) {
		x = structure(list("value.const"), class = "tmapOption")
	}

	if (inherits(x, c("tmapOption", "tmapVars"))) return(x)

	# if (inherits(x, "tm_shape_vars")) return(structure(list(ids = x$ids, n = x$n), class = "tmapShpVars"))
	# if (inherits(x, "tm_mv_shape_vars")) return(structure(list(ids = x$ids, n = x$n), class = "tmapMVShpVars"))
	# if (inherits(x, "tmapDimVars")) return(x)

	cls = if (inherits(x, "AsIs")) "tmapAsIs" else if (inherits(x, "tmapUsrCls")) "tmapUsrCls" else "tbd"


	isL = is.list(x)

	if (cls == "tbd" && !isL) {
		if (is.character(x) && all(substr(x, 1, 1) == "*")) {
			x = substr(x, 2, nchar(x))
			return(tm_vars(x, animate = TRUE))
		}
	}

	isNestedL = isL && any(vapply(x, is.list, FUN.VALUE = logical(1)))
	isSpecialL = isL && !setequal(class(x), "list")
	isSpecialNestedL = isL && is.list(x[[1]]) &&  !setequal(class(x[[1]]), "list")
	if (!isL) {
		x = as.list(x)
	} else if (isSpecialL) {
		x = list(x)
	}

	if (cls == "tbd") cls = if (isSpecialL || isSpecialNestedL || isNestedL) "tmapSpecial" else "tmapStandard"

	if (cls == "tmapSpecial") {
		nms = seq_along(x)
	} else {
		nms = x
	}

	structure(x, names = nms, class = cls)
}
