process_color <- function(col, alpha=NA, sepia.intensity=0, saturation=1, ...) {
	res <- t(col2rgb(col, alpha=TRUE))
	
	# set alpha values
	if (!is.na(alpha)) res[,4] <- alpha * 255

	# convert to sepia
	if (sepia.intensity!=0) {
		conv_matrix <- matrix(c(.393, .769, .189,
								.349, .686, .168,
								.272, .534, .131), ncol=3, byrow=FALSE)
		res[,1:3] <-  (res[,1:3] %*% conv_matrix) * sepia.intensity + res[,1:3] * (1-sepia.intensity)
		res[res>255] <- 255
		res[res<0] <- 0
	}	
	
	# convert to black&white
	if (saturation!=1) {
		res[,1:3] <- (res[,1:3] %*% matrix(c(.299, .587, .114), nrow=3, ncol=3))  * (1-saturation) + res[,1:3] * saturation
		res[res>255] <- 255
		res[res<0] <- 0
	}
	do.call("rgb", c(unname(as.data.frame(res)), list(maxColorValue=255)))
}
# get_alpha_col <- function(colour, alpha=NA) {
# 	col <- col2rgb(colour, TRUE)/255
# 	if (is.na(alpha)) alpha <- col[4,] 
# 	new_col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
# 	new_col
# }
# 
# get_sepia_col <- function(col, intensity=1) {
# 	conv_matrix <- matrix(c(.393, .769, .189,
# 							.349, .686, .168,
# 							.272, .534, .131), ncol=3, byrow=FALSE)
# 	colM <- t(col2rgb(col))
# 	res <-  (colM %*% conv_matrix) * intensity + colM * (1-intensity)
# 	res[res>255] <- 255
# 	do.call("rgb", c(unname(as.data.frame(res)), list(maxColorValue=255)))
# }
# 