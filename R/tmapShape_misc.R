#' Internal tmap function to create by variables (used for faceting)
#'
#' Internal tmap function to create by variables (used for faceting)
#'
#' @param dt data.table
#' @param tmf tmf object
#' @param smeta smeta object
#' @export
#' @keywords internal
make_by_vars = function(dt, tmf, smeta) {
	by123 = paste0("by", 1L:3L)
	by123__ = paste0("by", 1L:3L, "__")
	with(tmf, {
		if (length(b)) {
			for (w in b) {
				byvar = by123[w]
				byname = by123__[w]
				var = tmf[[byvar]]

				if (var == "FRAME__") {
					dt[, (byname):= NA_integer_] # dummy
				} else if (var %in% smeta$vars) {
					levs = smeta$vars_levs[[var]]
					if (is.null(levs)) {
						cli::cli_abort("Variable {.val {var}} is used as facetting variable. Therefore, it should be a factor.")
					}
					if (attr(levs, "showNA")) levs[length(levs)] = NA

					dt[, (byname):= match(get(get(..byvar)), levs)]
				} else if (tmf[[byvar]] %in% smeta$dims) {
					dt[, (byname):= match(get(get(..byvar)), smeta$dims_vals[[var]])]
				}
			}
		}
	})
	invisible(NULL)
}



#' Internal tmap function that gets factor levels with NA's
#'
#' Internal tmap function that gets factor levels with NA's
#'
#' @param x vector
#' @param o options
#' @export
#' @keywords internal
get_fact_levels_na = function(x, o) {
	if (inherits(x, "sfc") || is.list(x)) {
		levs = NULL
	} else if (is.factor(x)) {
		if (o$drop.empty.facets) {
			tab = tabulate(x, nbins = nlevels(x))
			anyna = (sum(tab) != length(x)) # note that NA can already be included in the levels (in that case anyna = FALSE)
			levs = levels(x)[tab != 0]
		} else {
			anyna = anyNA(x)
			levs = levels(x)
		}

		if (!o$drop.NA.facets && anyna) {
			showNA = TRUE
			levs = c(levs, o$na.text)
		} else if (!o$drop.NA.facets && any(is.na(levs))) {
			showNA = TRUE
			levs[is.na(levs)] = o$na.text
		} else if (o$drop.NA.facets && any(is.na(levs))) {
			showNA = FALSE
			levs = levs[!is.na(levs)]
		} else {
			showNA = FALSE
		}
	} else {
		u = unique(as.vector(x))
		if (length(u) > o$facet.max) {
			levs = NULL
		} else {
			levs = as.character(sort(u))
			if (!o$drop.NA.facets && any(is.na(u))) {
				showNA = TRUE
				levs = c(levs, o$na.text)
			} else {
				showNA = FALSE
			}
		}
	}
	if (!is.null(levs)) attr(levs, "showNA") = showNA
	levs
}
