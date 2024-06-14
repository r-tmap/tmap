#' @param fill,col,shape,size,fill_alpha,col_alpha,lty,lwd,linejoin,lineend visual variables
#' @param ... args
#' @export
#' @name tmapGpar
#' @rdname tmap_internal
#' @keywords internal
tmapGpar = function(fill = NULL,
					col = NULL,
					shape = NULL,
					size = NULL,
					fill_alpha = NULL,
					col_alpha = NULL,
					#pattern = NULL,
					lty = NULL,
					lwd = NULL,
					linejoin = NULL,
					lineend = NULL,
					...) {
	args = c(as.list(environment()), list(...))
	structure(args, class = "tmapGpar")
}

#' @export
#' @name tmapTpar
#' @rdname tmap_internal
#' @keywords internal
tmapTpar = function(...) {
	args = c(as.list(environment()), list(...))
	structure(args, class = "tmapTpar")
}
