get_sepia_col <- function(col, intensity=1) {
	conv_matrix <- matrix(c(.393, .769, .189,
							.349, .686, .168,
							.272, .534, .131), ncol=3, byrow=FALSE)
	colM <- t(col2rgb(col))
	res <-  (colM %*% conv_matrix) * intensity + colM * (1-intensity)
	res[res>255] <- 255
	do.call("rgb", c(unname(as.data.frame(res)), list(maxColorValue=255)))
}
