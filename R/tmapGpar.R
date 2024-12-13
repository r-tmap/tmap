#' @param fill,col,shape,size,fill_alpha,col_alpha,lty,lwd,linejoin,lineend visual variables
#' @param ... args
#' @export
#' @rdname tmap_internal
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
#' @rdname tmap_internal
tmapTpar = function(...) {
	args = c(as.list(environment()), list(...))
	structure(args, class = "tmapTpar")
}
