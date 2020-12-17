tm_aes_color_discrete = function(n = 5,
								 style = "pretty",
								 palette = "Purples") {
	structure(as.list(environment()), class = c("tm_aes_color_discrete", "tm_aes"))
}

tm_aes_2d_size = function(max = NA,
						  perceptual = FALSE) {
	structure(as.list(environment()), class = c("tm_aes_2d_size", "tm_aes"))
}

tm_aes_shape = function(shapes = c(16:21)) {
	structure(as.list(environment()), class = c("tm_aes_shape", "tm_aes"))
}

