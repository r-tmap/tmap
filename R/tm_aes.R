tm_aes_color = function(palette = tmap_option("aes.palette"),
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
								 contrast = 1,
								 colorNA = tmap_option("aes.color")["na"],
								 textNA = "Missing",
								 showNA = NA,
								 colorNULL = tmap_option("aes.color")["null"]) {
	structure(c(list(FUN = "tmapAesColor"), as.list(environment())), class = c("tm_aes_color", "tm_aes"))
}

tm_aes_color_rgb = function(maxValue = 255) {
	structure(c(list(FUN = "tmapAesColorRGB"), as.list(environment())), class = c("tm_aes_color_rgb", "tm_aes"))
}

tm_aes_2d_size = function(max = NA,
						  lim=NA,
						  sizes.legend = NULL,
						  sizes.legend.labels = NULL,
						  perceptual = FALSE,
						  scale = 1) {
	structure(c(list(FUN = "tmapAes2dSize"), as.list(environment())), class = c("tm_aes_2d_size", "tm_aes"))
}

tm_aes_shape = function(shapes = 21:25,
						shapes.legend = NULL,
						shapes.legend.fill = NA,
						shapes.labels = NULL,
						shapes.drop.levels = FALSE,
						shapeNA = 4,
						shape.textNA = "Missing",
						shape.showNA = NA,
						shapes.n = 5, shapes.style = ifelse(is.null(shapes.breaks), "pretty", "fixed"),
						shapes.style.args = list(),
						shapes.as.count = NA,
						shapes.breaks = NULL,
						shapes.interval.closure = "left") {
	structure(c(list(FUN = "tmapAes2dShape"), as.list(environment())), class = c("tm_aes_shape", "tm_aes"))
}

format_aes_results = function(values, legend) {
	lst = vector(mode = "list", length = length(values))
	lst[[1]] = legend
	list(values = values,
		 legend = lst)
}







