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

					# `levs` are always character labels (built via as.character()
					# or levels() in get_fact_levels_na). match() does coerce the
					# left-hand side to character, but for classed columns it relies
					# on as.character() S3 dispatch: Date has an as.character method
					# and formats correctly, whereas POSIXct (and other date/time
					# classes lacking such a method) fall back to the raw underlying
					# double (e.g. "1672574400") and never match the formatted labels.
					# Coercing here with as.character() uses the same representation
					# that produced `levs`, so the join is correct for every class.
					dt[, (byname):= match(as.character(get(get(..byvar))), levs)]
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
	# POSIXlt is stored as a list; normalise to POSIXct so it is treated as a
	# date/time vector below instead of being caught by the is.list() guard
	if (inherits(x, "POSIXlt")) x = as.POSIXct(x)

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
	} else if (inherits(x, c("POSIXct", "Date", "difftime"))) {
		# Date/time classes: keep the class. The plain branch below uses
		# as.vector(), which would strip the class and turn dates into raw
		# day/second counts. Order chronologically and format the labels with
		# the class's own as.character() method; make_by_vars() coerces the
		# column with the same as.character(), so the facet join lines up.
		u = unique(x)
		if (length(u) > o$facet_levels.max) {
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
	} else {
 		u = unique(as.vector(x))
		if (length(u) > o$facet_levels.max) {
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
