process_fill_vector <- function(x, g, gt, tiny) {
	choro.values <- x
	
	x[tiny] <- NA
	
	if (is.factor(x)) {
		palette <- if (is.null(g$palette)) ifelse(nlevels(x)>8, "Set3", "Dark2") else g$palette
		colsLeg <- cat2pal(x,
						   palette = palette,
						   colorNA = g$colorNA,
						   legend.NA.text = gt$legend.NA.text,
						   max_levels=g$max.categories)
		choro.breaks <- NA
	} else {
		palette <- if (is.null(palette)) "RdYlGn" else palette
		colsLeg <- num2pal(x, g$n, style=g$style, breaks=g$breaks, 
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = contrast, legend.labels=g$labels,
						   colorNA=g$colorNA, 
						   legend.digits=gt$legend.digits,
						   legend.NA.text = gt$legend.NA.text)
		choro.breaks <- colsLeg[[4]]
	}
	fill <- colsLeg[[1]]
	choro.legend.labels <- colsLeg[[2]]
	choro.legend.palette <- colsLeg[[3]]
	
	## adjust hisogram
	if (!is.na(choro.breaks[1])) {
		tmp_breaks <- choro.breaks
		tmp_breaks[1] <- -Inf
		tmp_breaks[length(tmp_breaks)] <- Inf
		tmp_int <- findInterval(choro.values[tiny], tmp_breaks)
		tmp_int[is.na(tmp_int)] <- length(choro.legend.palette)
		fill[tiny] <- choro.legend.palette[tmp_int]
	}
	return(list(fill=fill, 
				choro.legend.labels=choro.legend.labels,
				choro.legend.palette=choro.legend.palette,
				choro.breaks=choro.breaks))
}




process_fill <- function(data, g, gt, gby) {
	
	x <- g$col
	nx <- length(x)
	
	npol <- nrow(data)

	# check for direct color input
	if (all(valid_colors(x))) return(list(fill=matrix(rep(x, each=npol), nrow=npol), xfill=NA))
	
	areas <- data$SHAPE_AREAS
	by <- data$GROUP_BY
	
	dt <- process_data(data[, x, drop=FALSE], by=by, free.scales=gby$free.scales.fill)
	## output: matrix=colors, list=free.scales, vector=!freescales
	
	# return if data is matrix of color values
	if (is.matrix(dt)) return(list(fill=data, xfill=NA))

	tiny <- areas < g$thres.poly

	#
	choro.values <- dt
	if (is.list(dt)) {
		isNum <- sapply(dt, is.numeric)
		
		if (any(isNum) && convert2density) {
			dt[isNum] <- lapply(data[isNum], function(d) {
				d / (areas * g$total.area.km2)
			})
		}
		res <- lapply(dt, process_fill_vector, g, gt, tiny)
	} else {
		if (is.numeric(dt) && convert2density) {
			dt <- dt / (areas * g$total.area.km2)
		}
		res <- process_fill_vector(dt, g, gt, tiny)
	}
	browser()
	
	
	




fill <- matrix(unlist(split(colsLeg[[1]], rep(1:nx, each=length(colsLeg[[1]])/nx))), ncol=nx)


	if (free.scales && nx > 1) {
		fill <- matrix("", ncol=nx, nrow=nrow(X))
		choro.legend.labels <- list()
		choro.legend.palette <- list()
		choro.breaks <- list()
		
		for (i in 1:nx) {
			XX <- X[[i]]
            if (isCat) {
            	if (is.null(palette)) palette <- ifelse(nlevels(XX)>8, "Set3", "Dark2")
                colsLeg <- cat2pal(XX,
                                   palette = palette,
                                   colorNA = colorNA,
                				   legend.NA.text = legend.NA.text,
                				   max_levels=g$max.categories)
                choro.breaks[[i]] <- NA
            } else {
            	if (is.null(palette)) {
            		#anyPos <- any(XX>0, na.rm=TRUE)
            		#anyNeg <- any(XX<0, na.rm=TRUE)
            		palette <- "RdYlGn"		# ifelse(anyPos && !anyNeg, "YlGn",
            		# ifelse(!anyPos && anyNeg, "-YlOrGn", "RdYlBu"))
            	}
            	colsLeg <- num2pal(XX, n, style=style, breaks=breaks,
                                   palette = palette,
                                   auto.palette.mapping = auto.palette.mapping,
                                   contrast = contrast, legend.labels=labels,
                                   colorNA=colorNA,
            					   legend.digits=legend.digits,
            					   legend.NA.text=legend.NA.text)
                
                choro.breaks[[i]] <- colsLeg[[4]]
            }
			#X[[i]] <- XX
			fill[,i] <- colsLeg[[1]]
			choro.legend.labels[[i]] <- colsLeg[[2]]
			choro.legend.palette[[i]] <- colsLeg[[3]]

			## adjust hisogram
			if (!is.na(choro.breaks[[i]][1])) {
				tmp_breaks <- choro.breaks[[i]]
				tmp_breaks[1] <- -Inf
				tmp_breaks[length(tmp_breaks)] <- Inf
				tmp_int <- findInterval(choro.values[tiny,i], tmp_breaks)
				tmp_int[is.na(tmp_int)] <- length(choro.legend.palette[[i]])
				fill[,i][tiny] <- choro.legend.palette[[i]][tmp_int]
			}
		}
		X[tiny, ] <- NA
	} else {
		XX <- unlist(X)
        if (isCat) {
        	if (is.null(palette)) palette <- ifelse(nlevels(XX)>8, "Set3", "Dark2")
        	colsLeg <- cat2pal(XX,
                               palette = palette,
                               colorNA = colorNA,
        					   legend.NA.text = legend.NA.text,
        					   max_levels=g$max.categories)
            choro.breaks <- NA
        } else {
        	if (is.null(palette)) {
        		#anyPos <- any(XX>0, na.rm=TRUE)
        		#anyNeg <- any(XX<0, na.rm=TRUE)
        		palette <- "RdYlGn"		# ifelse(anyPos && !anyNeg, "YlGn",
        								# ifelse(!anyPos && anyNeg, "-YlOrGn", "RdYlBu"))
        	}
			colsLeg <- num2pal(XX, n, style=style, breaks=breaks, 
    						   palette = palette,
    						   auto.palette.mapping = auto.palette.mapping,
    						   contrast = contrast, legend.labels=labels,
    						   colorNA=colorNA, 
    						   legend.digits=legend.digits,
							   legend.NA.text = legend.NA.text)
    		choro.breaks <- colsLeg[[4]]
        }
		fill <- matrix(unlist(split(colsLeg[[1]], 
									rep(1:nx, each=length(colsLeg[[1]])/nx))), ncol=nx)
		choro.legend.labels <- colsLeg[[2]]
		choro.legend.palette <- colsLeg[[3]]
		
		## adjust hisogram
		if (!is.na(choro.breaks[1])) {
			tmp_breaks <- choro.breaks
			tmp_breaks[1] <- -Inf
			tmp_breaks[length(tmp_breaks)] <- Inf
			tmp_int <- findInterval(unlist(choro.values[tiny,]), tmp_breaks)
			tmp_int[is.na(tmp_int)] <- length(choro.legend.palette)
			fill[tiny,] <- choro.legend.palette[tmp_int]
		}
	}
	choro.values <- X
	
	
# 	if (style=="kmeans") {
# 		choro.values[choro.values>max(choro.breaks)] <- max(choro.breaks)
# 		choro.values[choro.values<min(choro.breaks)] <- min(choro.breaks)
# 	}
	
	list(fill=fill,
		 fill.legend.labels=choro.legend.labels,
		 fill.legend.palette=choro.legend.palette,
		 fill.legend.misc=list(values=choro.values, breaks=choro.breaks),
		 xfill=x)
}












