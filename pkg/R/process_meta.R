process_meta <- function(g, nx, varnames) {
	
	g$geo_grid <- within(g$geo_grid, {
		if (is.null(ncol) && is.null(nrow)) {
			## default setting: place next to each other, or in grid
			if (nx <= 3) {
				ncol <- nx
				nrow <- 1
			} else {
				ncol <- ceiling(sqrt(nx))
				nrow <- ceiling(nx / ncol)
			}
		} else {
			if (is.null(ncol)) ncol <- ceiling(nx / nrow)
			if (is.null(nrow)) nrow <- ceiling(nx / ncol)
		}
	})
	
	g$geo_theme <- within(g$geo_theme, {
		if (is.na(title[1])) {
			id <- which(as.logical(sapply(varnames, function(x)sum(!is.na(x[1])))))[1]
		} else id <- switch(title[1],
							choro.fill=1,
							bubble.size=2,
							bubble.col=3,
							0)
		
		if (is.na(id)) {
			title <- rep("", nx)
		} else if (id!=0) {
			if (is.na(legend.choro.title) && id!=1) legend.choro.title <- varnames[[1]]
			if (is.na(legend.bubble.size.title) && id!=2) legend.bubble.size.title <- varnames[[2]]
			if (is.na(legend.bubble.col.title) && id!=3) legend.bubble.col.title <- varnames[[3]]
			title <- varnames[[id]]
		}
		if (is.na(legend.choro.title[1])) legend.choro.title <- rep("", nx)
		if (is.na(legend.bubble.size.title[1])) legend.bubble.size.title <- rep("", nx)
		if (is.na(legend.bubble.col.title[1])) legend.bubble.col.title <- rep("", nx)
		
		if (length(title) < nx) title <- rep(title, length.out=nx)
		if (length(legend.choro.title) < nx) legend.choro.title <- rep(legend.choro.title, length.out=nx)
		if (length(legend.bubble.size.title) < nx) legend.bubble.size.title <- rep(legend.bubble.size.title, length.out=nx)
		if (length(legend.bubble.col.title) < nx) legend.bubble.col.title <- rep(legend.bubble.col.title, length.out=nx)
		
		if (is.null(bg.color)) bg.color <- ifelse(is.na(varnames$choro[1]), "white", "grey85")
		
		if (identical(title.bg.color, TRUE)) title.bg.color <- bg.color
		if (identical(legend.bg.color, TRUE)) legend.bg.color <- bg.color
	})	
	
	g
}
