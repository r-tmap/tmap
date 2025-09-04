#' Create a style catalogue
#'
#' Create a style catalogue for each predefined tmap style. The result is a set of png images, one for each style.
#'
#' @param path path where the png images are stored
#' @param styles vector of styles function names (see [tmap_style()]) for which a preview is generated. By default, a preview is generated for all loaded styles.
#' @import grid
#' @rdname tmap_style_catalogue
#' @export
tmap_style_catalogue <- function(path="./tmap_style_previews", styles=NA) {
	# get all styles

	styles <- get_style_names()

	ns <- length(styles)

	if (!file.exists(path)) dir.create(path, recursive = TRUE)


	# load and process data
	pb <- txtProgressBar()

	tm1 = tm_shape(NLD_prov) +
		tm_polygons() +
		tm_title_out("Constant polygon fill")

	NLD_muni$letter = factor(substr(NLD_muni$name, 1, 1))
	tm2 = tm_shape(NLD_muni) +
		tm_polygons(lwd = .5, fill = "letter", fill.scale = tm_scale_categorical(n.max = 7)) +
		tm_shape(NLD_prov) +
		tm_borders(lwd = 2) +
		tm_title_out("Categorical color scale") +
		tm_compass() +
		tm_scalebar()


	tm3 = tm_shape(NLD_muni) +
		tm_polygons(lwd = 0.5, fill = "income_high") +
		tm_shape(NLD_prov) +
		tm_borders(lwd = 2) +
		tm_title_out("Sequential color scale")

	tm4 = tm_shape(NLD_muni) +
		tm_polygons(lwd = 0.5, fill = "urbanity", fill.scale = tm_scale_categorical(values = "div")) +
		tm_shape(NLD_prov) +
		tm_borders(lwd = 2) +
		tm_title_out("Diverging color scale")


	print_style <- function(style, i) {
		on.exit(dev.off())
		png(file.path(path, paste0(style, ".png")), width=1920, height=1920)
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(2,2)))

		# set progress bar init
		pbi <- (i-1)/ns
		pbii <- function(i) pbi + (i/4) * (1/ns)

		print(tm1 + tm_style(style) + tm_layout(scale = 2),
			  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
		setTxtProgressBar(pb, pbii(1))
		print(tm2 + tm_style(style) + tm_layout(scale = 2),
			  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
		setTxtProgressBar(pb, pbii(2))
		print(tm3 + tm_style(style) + tm_layout(scale = 2),
			  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
		setTxtProgressBar(pb, pbii(3))
		print(tm4 + tm_style(style) + tm_layout(scale = 2),
			  vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
		setTxtProgressBar(pb, pbii(4))
		upViewport()
	}

	mapply(print_style, styles, 1:ns)
	if (get("tmapOptions", envir = .TMAP)$show.messages) message("\nCatalogue created in ", normalizePath(path))
	invisible()
}

#' @export
#' @rdname tmap_style_catalogue
tmap_style_catalog <- tmap_style_catalogue


