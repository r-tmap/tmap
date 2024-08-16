#' tmap function to specify variables
#' 
#' tmap function to specify all variables in the shape object
#' 
#' @param x variable names, variable indices, or a dimension name
#' @param dimvalues dimension values
#' @param n if specified the first `n` variables are taken (or the first `n` dimension values) 
#' @param multivariate in case multiple variables are specified, should they serve as facets (FALSE) or as a multivariate visual variable?
#' @export
tm_vars = function(x = NA, dimvalues = NULL, n = NA, multivariate = FALSE) {
	structure(list(x = x, dimvalues = dimvalues, n = n, multivariate = multivariate), class = c("tmapVars", "list"))
}


# process visual variable specification. Can either be tmapVars (output of tm_vars) or a list of values.
tmapVV = function(x) {
	if (inherits(x, c("tmapOption", "tmapVars"))) return(x)
	
	# if (inherits(x, "tm_shape_vars")) return(structure(list(ids = x$ids, n = x$n), class = "tmapShpVars"))
	# if (inherits(x, "tm_mv_shape_vars")) return(structure(list(ids = x$ids, n = x$n), class = "tmapMVShpVars"))
	# if (inherits(x, "tmapDimVars")) return(x)
	
	cls = if (inherits(x, "AsIs")) "tmapAsIs" else if (inherits(x, "tmapUsrCls")) "tmapUsrCls" else "tbd"
	
	isL = is.list(x)
	isSpecialL = isL && !setequal(class(x), "list")
	isSpecialNestedL = isL && is.list(x[[1]]) &&  !setequal(class(x[[1]]), "list")
	if (!isL) {
		x = as.list(x)
	} else if (isSpecialL) {
		x = list(x)
	}
	
	if (cls == "tbd") cls = if (isSpecialL) "tmapSpecial" else if (isSpecialNestedL) "tmapSpecial" else "tmapStandard"

	structure(x, names = x, class = cls)
}