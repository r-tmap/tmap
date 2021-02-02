tm_aes_color_discrete = function(n = 5,
								 style = "pretty",
								 palette = "Purples") {
	structure(c(list(FUN = "tmapAesColorDiscrete"), as.list(environment())), class = c("tm_aes_color_discrete", "tm_aes"))
}

tm_aes_color_rgb = function(maxValue = 255) {
	structure(c(list(FUN = "tmapAesColorRGB"), as.list(environment())), class = c("tm_aes_color_rgb", "tm_aes"))
}

tm_aes_2d_size = function(max = NA,
						  perceptual = FALSE) {
	structure(c(list(FUN = "tmapAes2dSize"), as.list(environment())), class = c("tm_aes_2d_size", "tm_aes"))
}

tm_aes_shape = function(shapes = c(16:21)) {
	structure(c(list(FUN = "tmapAes2dSize"), as.list(environment())), class = c("tm_aes_shape", "tm_aes"))
}

tmapAesColorDiscrete = function(x1, setup) {
	ci = classInt::classIntervals(x1, n = setup$n, style = setup$style)
	pals::brewer.blues(setup$n)[classInt::findCols(ci)]
}

tmapAesColorRGB = function(x1, x2, x3, setup) {
	grDevices::rgb(x1, x2, x3, maxColorValue = setup$maxValue)
}

tmapAes2dSize = function(x1, setup) {
	max = if (is.na(setup$max)) max(x1, na.rm = TRUE) else setup$max
	x1 / max
}

tmapAesShape = function(x1, setup) {
	if (is.numeric(x1)) x1 = as.integer(x1)
	if (is.character(x1)) x1 = as.factor(x1)
	if (is.factor(x1)) x1 = as.integer(x1)
	#n = length(shapes)
	shapes[x1]
}
