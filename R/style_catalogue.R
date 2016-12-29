#' Create a style catalogue
#' 
#' Create a style catalogue for each predefined tmap style. The result is a set of png images, one for each style.
#' 
#' @param path path where the png images are stored
#' @param styles vector of \code{tm_style_XXX} function names. By default, it contains all styles that are included in tmap, and, if \code{include.global.styles=TRUE}, also the user defined \code{tm_style_XXX} function names.
#' @param include.global.styles See above
#' @import grid
#' @rdname style_catalogue
#' @export
style_catalogue <- function(path="./tmap_style_previews", styles=NA, include.global.styles=TRUE) {
	# get all styles
	if (is.na(styles[1])) {
		lst <- ls("package:tmap")
		if (include.global.styles) lst <- c(lst, ls())
		styles <- grep("tm_style", lst, value=TRUE, fixed=TRUE)
	}
	
	ns <- length(styles)
	
	if (!file.exists(path)) dir.create(path, recursive=TRUE)

	# load and process data
	Europe <- metro <- rivers <- land <- NULL
	data(Europe, metro, rivers, land, envir = environment())
	metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100
	EUriv <- set_projection(rivers, get_projection(Europe))
	
	
	pb <- txtProgressBar()
	
	print_style <- function(style, i) {
		on.exit(dev.off())
		png(file.path(path, paste0(style, ".png")), width=1920, height=1080)
		tml <- do.call(style, args=list())
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(3,3)))

		# set progress bar init
		pbi <- (i-1)/ns
		pbii <- function(i) pbi + (i/9) * (1/ns)
		
		# first column
		print(tm_shape(Europe) +
			  	tm_polygons() +
			  	tm_text("iso_a3", size="AREA") +
			  	tm_symbols() +
			  	tm_shape(rivers) +
			  	tm_lines() + 
			  	tm_compass() +
			  	tm_scale_bar() +
			  	tml + tm_format_Europe(title="Fixed aesthetics"),
			  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
		setTxtProgressBar(pb, pbii(1))
		print(tm_shape(Europe) +
			  	tm_borders() + 
			  	tm_text("name", size="AREA") +
			  	tml + tm_format_Europe(title="Polygon borders with text"),
			  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
		setTxtProgressBar(pb, pbii(2))
		print(tm_shape(EUriv, bbox = bb(Europe)) +
			  	tm_lines() + 
			  	tml + tm_format_Europe(title="Lines only"),
			  vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
		setTxtProgressBar(pb, pbii(3))
		
		# second column
		print(tm_shape(Europe) +
			  	tm_polygons("MAP_COLORS", ncols=5) + 
			  	tml + tm_format_Europe(title="Map coloring"),
			  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
		setTxtProgressBar(pb, pbii(4))
		print(tm_shape(Europe) +
			  	tm_polygons() + 
			  	tm_shape(metro) +
			  	tm_dots() + 
			  	tm_grid(projection = "longlat") +
			  	tml + tm_format_Europe(title="Dot map"),
			  vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
		setTxtProgressBar(pb, pbii(5))
		print(tm_shape(Europe) +
			  	tm_polygons() +
			  	tm_shape(metro) +
			  	tm_symbols(size = "pop2010", col = "growth", breaks = c(-Inf, -2, -1, -.5, .5, 1, 2, Inf)) + tml + tm_format_Europe(title="symbol map"),
			  vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
		setTxtProgressBar(pb, pbii(6))
		
				
		# third column
		print(tm_shape(Europe) +
			  	tm_polygons("gdp_cap_est", breaks=c(0, 10000, 20000, 30000, 40000, 50000, Inf)) + 
			  	tm_text("iso_a3", size="AREA") + tml + tm_format_Europe(title="Choropleth"),
			  vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
		setTxtProgressBar(pb, pbii(7))
		print(tm_shape(Europe) +
			  	tm_polygons("HPI", palette="div", auto.palette.mapping=FALSE, n=9) +
			  	tm_text("iso_a3", size="AREA") + tml + tm_format_Europe(title="Choropleth (diverging)"),
			  vp = viewport(layout.pos.row = 2, layout.pos.col = 3))
		setTxtProgressBar(pb, pbii(8))
		print(tm_shape(Europe) +
			  	tm_polygons("economy") + 
			  	tm_text("iso_a3", size="AREA") + tml + tm_format_Europe(title="Categorical map"),
			  vp = viewport(layout.pos.row = 3, layout.pos.col = 3))
		setTxtProgressBar(pb, pbii(9))
		upViewport()
	}
	
	mapply(print_style, styles, 1:ns)
	message("Catalogue created in ", normalizePath(path))
	invisible()
}

#' @rdname style_catalogue
style_catalog <- style_catalogue


