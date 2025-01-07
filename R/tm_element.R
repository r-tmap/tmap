#' Internal methods for tmap extensions
#'
#' Internal methods for tmap extensions
#'
#' @param ... arguments
#' @param subclass subclass
#' @export
#' @name tmap_internal
#' @keywords internal
tm_element = function(..., subclass = NULL) {
	structure(list(...), class = c(subclass, "tm_element", "list"))
}

#' @export
#' @rdname tmap_internal
tm_element_list = function(...) {
	structure(list(...), class = "tmap")
}

tm_element_list_subclass = function(tml) {
	vapply(tml, function(x) {
		setdiff(class(x), "tm_element")[1]
	}, FUN.VALUE = character(1))
}

tm_element_list_sel = function(tml, subclass) {
	ids = which(tm_element_list_subclass(tml) %in% subclass)
	x = tml[ids]
	attr(x, "ids") = ids
	x
}

#' Stacking of tmap elements
#'
#' The plus operator allows you to stack tmap elements (functions with a prefix `tm_`)
#'
#' @param e1 first tmap element
#' @param e2 second tmap element
#' @name tmap-element
#' @export
"+.tmap" = function(e1, e2) {
	assign("last_map_new", rlang::call_match(), envir = .TMAP)
	if (inherits(e2, "tm_legend")) {
		names(e2) = paste0("legend.", names(e2))

		e2 = tm_element_list(do.call(tm_element, c(e2, list(calls = "v3_tm_legend", subclass = c("tm_legend_v3", "tm_options")))))
	}
	structure(c(e1, e2), class = "tmap")
}

#' Retrieve the last map to be modified or created
#'
#' Retrieve the last map to be modified or created. Works in the same way
#' as `ggplot2::last_plot()`, although there is a difference:
#' `tmap_last()` returns the last call instead of the stacked [`tmap-element`]s.
#'
#' @return call
#' @export
#' @seealso [tmap_save()]
tmap_last = function() {
	.x = get("last_map", envir = .TMAP)
	if (is.null(.x) && get("tmapOptions", envir = .TMAP)$show.warnings) {
		warning("A map has not been created yet.")
	}
	eval(.x)
}

save_last_map = function() {
	lt = get("last_map", envir = .TMAP)
	ltnew = get("last_map_new", envir = .TMAP)
	if (!is.null(ltnew)) lt = replace_last_tmap_by_correct_call(ltnew, lt)
	assign("last_map", lt, envir = .TMAP)
	assign("last_map_new", NULL, envir = .TMAP)
}

replace_last_tmap_by_correct_call = function(mc, lt) {
	if (is.symbol(mc)) {
		mc
	} else if (as.character(mc[1])=="last_map") {
		lt
	} else {
		if (as.character(mc[1]) %in% c("+.tmap", "+")) {
			if (!is.null(mc[[2]])) mc[2] = list(replace_last_tmap_by_correct_call(mc[[2]], lt))
			if (!is.null(mc[[3]])) mc[3] = list(replace_last_tmap_by_correct_call(mc[[3]], lt))
		}
		mc
	}
}

#' Print tm_element
#'
#' @param x x
#' @param ... passed on
#' @export
#' @name print.tm_element
#' @keywords internal
print.tm_element = function(x, ...) {
	cat("tm_element object\n")
	str(x, 2)
}

#' @rdname print.tm_element
#' @export
print.tm_shape = function(x, ...) {
	NextMethod()
}
