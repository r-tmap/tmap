process_choro <- function(g, free.scales) {
	shp.name <- g$shp
	x <- g$col
	n <- g$n
	convert2density <- g$convert2density
	total.area.km2 <- g$total.area.km2
	style <- g$style
	breaks <- g$breaks
	palette <- g$palette
	auto.palette.mapping <- g$auto.palette.mapping
	contrast <- g$contrast
	labels <- g$labels
	colorNA <- g$colorNA
	
	nx <- length(x)
	X <- get(shp.name)@data[, x, drop=FALSE]
	if (convert2density) X <- densities(get(shp.name), var=x, total.area.km2=ifelse(is.na(total.area.km2), 1, total.area.km2), drop=FALSE)
	
	if (free.scales && nx > 1) {
		fill <- matrix("", ncol=nx, nrow=nrow(X))
		choro.legend.labels <- list()
		choro.legend.palette <- list()
		choro.breaks <- list()
		
		for (i in 1:nx) {
            if (is.factor(X[[i]])) {
            	if (is.null(palette)) palette <- "Dark2"
                colsLeg <- cat2pal(X[[i]],
                                   palette = palette,
                                   colorNA = colorNA)
                choro.breaks[[i]] <- NA
            } else {
            	if (is.null(palette)) palette <- "RdYlBu"
            	colsLeg <- num2pal(X[[i]], n, style=style, breaks=breaks, 
                                   palette = palette,
                                   auto.palette.mapping = auto.palette.mapping,
                                   contrast = contrast, legend.labels=labels,
                                   colorNA=colorNA)
                
                choro.breaks[[i]] <- colsLeg[[4]]
            }
			fill[,i] <- colsLeg[[1]]
			choro.legend.labels[[i]] <- colsLeg[[2]]
			choro.legend.palette[[i]] <- colsLeg[[3]]
		}
	} else {
        if (is.factor(X[[1]])) {
        	if (is.null(palette)) palette <- "Dark2"
        	colsLeg <- cat2pal(unlist(X),
                               palette = palette,
                               colorNA = colorNA)
            choro.breaks <- NA
        } else {
        	if (is.null(palette)) palette <- "RdYlBu"
        	colsLeg <- num2pal(unlist(X), n, style=style, breaks=breaks, 
    						   palette = palette,
    						   auto.palette.mapping = auto.palette.mapping,
    						   contrast = contrast, legend.labels=labels,
    						   colorNA=colorNA)
    		choro.breaks <- colsLeg[[4]]
        }
		fill <- matrix(unlist(split(colsLeg[[1]], 
									rep(1:nx, each=length(colsLeg[[1]])/nx))), ncol=nx)
		choro.legend.labels <- colsLeg[[2]]
		choro.legend.palette <- colsLeg[[3]]
	}
	choro.values <- X
	
	list(fill=fill,
		 choro.values=choro.values,
		 choro.legend.labels=choro.legend.labels,
		 choro.legend.palette=choro.legend.palette,
		 choro.breaks=choro.breaks,
		 xfill=x,
		 shp.name=shp.name)
}
