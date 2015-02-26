get_alpha_col <- function(colour, alpha=NA) {
	col <- col2rgb(colour, TRUE)/255
	if (is.na(alpha)) alpha <- col[4,] 
	new_col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
	new_col
}
