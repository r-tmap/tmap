valid_colors <- function(x) {
	sapply(x, function(X) {
		tryCatch(is.matrix(col2rgb(X)), 
				 error = function(e) FALSE)
	})
}