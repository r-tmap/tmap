#' Create a style catalogue
#' 
#' Create a style catalogue for each predefined tmap style. The result is a set of png images, one for each style.
#' 
#' @param path path where the png images are stored
#' @param styles vector of styles function names (see \code{\link{tmap_style}}) for which a preview is generated. By default, a preview is generated for all loaded styles.
#' @import grid
#' @rdname tmap_style_catalogue
#' @export
tmap_style_catalogue <- function(path="./tmap_style_previews", styles=NA) {
	# get all styles
	
	styles <- get_style_names()

	ns <- length(styles)
	
	if (!file.exists(path)) dir.create(path, recursive=TRUE)

	# load and process data
	World <- metro <- rivers <- land <- NULL
	data(World, metro, rivers, land, envir = environment())
	metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

	rivers <- sf::st_transform(rivers, st_crs(World))
	
	pb <- txtProgressBar()
	
	print_style <- function(style, i) {
		on.exit(dev.off())
		png(file.path(path, paste0(style, ".png")), width=1920, height=1080)
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(3,3)))

		# set progress bar init
		pbi <- (i-1)/ns
		pbii <- function(i) pbi + (i/9) * (1/ns)
		
		# first column
		print(tm_shape(World) +
			  	tm_polygons() +
			  	tm_text("iso_a3", size="AREA") +
			  	tm_symbols() +
			  	tm_shape(rivers) +
			  	tm_lines() + 
			  	tm_compass() +
			  	tm_scale_bar() +
			  	tm_style(style) + tm_format("World", title="Fixed aesthetics"),
			  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
		setTxtProgressBar(pb, pbii(1))
		print(tm_shape(World) +
			  	tm_borders() + 
			  	tm_text("name", size="AREA") +
			  	tm_style(style) + tm_format("World", title="Polygon borders with text"),
			  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
		setTxtProgressBar(pb, pbii(2))
		print(tm_shape(rivers, bbox = bb(World)) +
			  	tm_lines() + 
			  	tm_style(style) + tm_format("World", title="Lines only"),
			  vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
		setTxtProgressBar(pb, pbii(3))
		
		# second column
		print(tm_shape(World) +
			  	tm_polygons("MAP_COLORS", ncols=5) + 
			  	tm_style(style) + tm_format("World", title="Map coloring"),
			  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
		setTxtProgressBar(pb, pbii(4))
		print(tm_shape(World) +
			  	tm_polygons() + 
			  	tm_shape(metro) +
			  	tm_dots() + 
			  	tm_grid(projection = 4326) +
			  	tm_style(style) + tm_format("World", title="Dot map"),
			  vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
		setTxtProgressBar(pb, pbii(5))
		print(tm_shape(World) +
			  	tm_polygons() +
			  	tm_shape(metro) +
			  	tm_symbols(size = "pop2010", col = "growth", breaks = c(-Inf, -2, -1, -.5, .5, 1, 2, Inf), midpoint = 0) + tm_style(style) + tm_format("World", title="symbol map"),
			  vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
		setTxtProgressBar(pb, pbii(6))
		
				
		# third column
		print(tm_shape(World) +
			  	tm_polygons("gdp_cap_est", breaks=c(0, 10000, 20000, 30000, 40000, 50000, Inf)) + 
			  	tm_text("iso_a3", size="AREA") + tm_style(style) + tm_format("World", title="Choropleth"),
			  vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
		setTxtProgressBar(pb, pbii(7))
		print(tm_shape(World) +
			  	tm_polygons("HPI", palette="div", n=9) +
			  	tm_text("iso_a3", size="AREA") + tm_style(style) + tm_format("World", title="Choropleth (diverging)"),
			  vp = viewport(layout.pos.row = 2, layout.pos.col = 3))
		setTxtProgressBar(pb, pbii(8))
		print(tm_shape(World) +
			  	tm_polygons("economy") + 
			  	tm_text("iso_a3", size="AREA") + tm_style(style) + tm_format("World", title="Categorical map"),
			  vp = viewport(layout.pos.row = 3, layout.pos.col = 3))
		setTxtProgressBar(pb, pbii(9))
		upViewport()
	}
	
	mapply(print_style, styles, 1:ns)
	if (get("tmapOptions", envir = .TMAP_CACHE)$show.messages) message("\nCatalogue created in ", normalizePath(path))
	invisible()
}

#' @export
#' @rdname tmap_style_catalogue
tmap_style_catalog <- tmap_style_catalogue


