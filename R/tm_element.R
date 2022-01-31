tm_element = function(..., subclass = NULL) {
	structure(list(...), class = c(subclass, "tm_element", "list"))
}

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
#' The plus operator allows you to stack tmap elements (functions with a prefix \code{tm_})
#' 
#' @param e1 first tmap element
#' @param e2 second tmap element
#' @export
"+.tmap" = function(e1, e2) {
	structure(c(e1, e2), class = "tmap")
}


#' Print tm_element
#' 
#' Print tm_element
#' 
#' @param x x
#' @param ... passed on 
#' @export
#' @method print tm_element
#' @rdname print.tm_element
#' @name print.tm_element
print.tm_element = function(x, ...) {
	cat("tm_element object\n")
	str(x, 2)
}

#' @rdname print.tm_element
#' @name print.tm_shape
#' @method print tm_shape
#' @export
print.tm_shape = function(x, ...) {
	NextMethod()
}
