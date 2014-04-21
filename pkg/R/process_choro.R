process_choro <- function(shp, g, free.scales, legend.digits) {
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
	X <- shp@data[, x, drop=FALSE]
	if (convert2density) X <- calc_densities(shp, var=x, total.area.km2=total.area.km2, drop=FALSE)
	
	tiny <- approx_areas(shp, units="prop") < .001

	
	
	if (free.scales && nx > 1) {
		fill <- matrix("", ncol=nx, nrow=nrow(X))
		choro.legend.labels <- list()
		choro.legend.palette <- list()
		choro.breaks <- list()
		
		for (i in 1:nx) {
			XX <- X[[i]]
            if (is.factor(XX)) {
            	if (is.null(palette)) palette <- "Dark2"
                colsLeg <- cat2pal(XX,
                                   palette = palette,
                                   colorNA = colorNA)
                choro.breaks[[i]] <- NA
            } else {
            	if (is.null(palette)) {
            		anyPos <- any(XX>0, na.rm=TRUE)
            		anyNeg <- any(XX<0, na.rm=TRUE)
            		palette <- ifelse(anyPos && !anyNeg, "Blues",
            						  ifelse(!anyPos && anyNeg, "-Reds", "RdYlBu"))
            	}
            	xmin <- min(XX[!tiny], na.rm=TRUE)
            	xmax <- max(XX[!tiny], na.rm=TRUE)
            	XX[tiny & XX<xmin] <- xmin
            	XX[tiny & XX>xmax] <- xmax
            	colsLeg <- num2pal(XX, n, style=style, breaks=breaks,
                                   palette = palette,
                                   auto.palette.mapping = auto.palette.mapping,
                                   contrast = contrast, legend.labels=labels,
                                   colorNA=colorNA,
            					   legend.digits=legend.digits)
                
                choro.breaks[[i]] <- colsLeg[[4]]
            }
			X[[i]] <- XX
			fill[,i] <- colsLeg[[1]]
			choro.legend.labels[[i]] <- colsLeg[[2]]
			choro.legend.palette[[i]] <- colsLeg[[3]]
		}
	} else {
		XX <- unlist(X)
        if (is.factor(XX)) {
        	if (is.null(palette)) palette <- "Dark2"
        	colsLeg <- cat2pal(XX,
                               palette = palette,
                               colorNA = colorNA)
            choro.breaks <- NA
        } else {
        	if (is.null(palette)) {
        		anyPos <- any(XX>0, na.rm=TRUE)
        		anyNeg <- any(XX<0, na.rm=TRUE)
        		palette <- ifelse(anyPos && !anyNeg, "Blues",
        						  ifelse(!anyPos && anyNeg, "-Reds", "RdYlBu"))
        	}
        	xmin <- min(XX[!tiny], na.rm=TRUE)
        	xmax <- max(XX[!tiny], na.rm=TRUE)
        	XX[tiny & XX<xmin] <- xmin
        	XX[tiny & XX>xmax] <- xmax
        	colsLeg <- num2pal(XX, n, style=style, breaks=breaks, 
    						   palette = palette,
    						   auto.palette.mapping = auto.palette.mapping,
    						   contrast = contrast, legend.labels=labels,
    						   colorNA=colorNA, 
    						   legend.digits=legend.digits)
    		choro.breaks <- colsLeg[[4]]
        }
		fill <- matrix(unlist(split(colsLeg[[1]], 
									rep(1:nx, each=length(colsLeg[[1]])/nx))), ncol=nx)
		choro.legend.labels <- colsLeg[[2]]
		choro.legend.palette <- colsLeg[[3]]
		X[,] <- XX
	}
	choro.values <- X
	list(fill=fill,
		 choro.values=choro.values,
		 choro.legend.labels=choro.legend.labels,
		 choro.legend.palette=choro.legend.palette,
		 choro.breaks=choro.breaks,
		 xfill=x)
}
