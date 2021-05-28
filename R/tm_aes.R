tm_aes_color_discrete = function(palette = NULL,
								 n = 5,
								 style = ifelse(is.null(breaks), "pretty", "fixed"),
								 style.args = list(),
								 as.count = NA,
								 breaks = NULL,
								 interval.closure = "left",
								 labels = NULL,
								 drop.levels = FALSE,
								 midpoint = NULL,
								 stretch.palette = TRUE,
								 contrast = NA,
								 colorNA = NULL,
								 textNA = "Missing",
								 showNA = NA,
								 colorNULL = NULL,
								 legend = tm_legend()) {
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

format_aes_results = function(values, legend) {
	lst = vector(mode = "list", length = length(values))
	lst[[1]] = legend
	list(values = values,
		 legend = lst)
}



tm_legend = function(title  = NA,
					 show = TRUE,
					 format = list(),
					 is.portrait = TRUE,
					 reverse = FALSE,
					 z = NA) {
	structure(c(list(FUN = "tmapLegend"), as.list(environment())), class = "tm_legend")
}




tmapAesColorRGB = function(x1, x2, x3, setup, opt) {
	values = grDevices::rgb(x1, x2, x3, maxColorValue = setup$maxValue)

	format_aes_results(values, list())
}

tmapAes2dSize = function(x1, setup, opt) {
	max = if (is.na(setup$max)) max(x1, na.rm = TRUE) else setup$max
	values = x1 / max
	legend = list(max = max)
	
	format_aes_results(values, legend)
}

tmapAesShape = function(x1, setup) {
	if (is.numeric(x1)) x1 = as.integer(x1)
	if (is.character(x1)) x1 = as.factor(x1)
	if (is.factor(x1)) x1 = as.integer(x1)
	#n = length(shapes)
	values = setup$shapes[x1]
	legend = list(shapes = setup$shapes)
	
	format_aes_results(values, legend)
}
