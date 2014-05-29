process_fill <- function(data, g, free.scales, legend.digits, legend.NA.text, legend.max.categories) {
	x <- g$col
	nx <- length(x)
	if (nx==1 && valid_colors(x)[1]) {
		return(list(fill=x,
					choro.fill.legend.labels=NA,
					choro.fill.legend.palette=NA,
					choro.fill.legend.misc=list(),					
			 		xfill=NA))
	}
	X <- data[, x, drop=FALSE]
	
	isColor <- if (all(sapply(X, function(i) is.character(i)))) all(valid_colors(unlist(X))) else FALSE
	
	if (isColor) { 
		return(list(fill=X, 
					choro.fill.legend.labels=NA,
					choro.fill.legend.palette=NA,
					choro.fill.legend.misc=list(),					
					xfill=NA))
	}
	
	isCat <- any(sapply(X, function(i) !is.numeric(i)))
	if (isCat) X <- as.data.frame(lapply(X, function(i) if (!is.factor(i)) as.factor(i) else i))
	
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
	thres.poly <- g$thres.poly
	
	if (convert2density) X <- X / (data$SHAPE_AREAS * total.area.km2)
	tiny <- data$SHAPE_AREAS < thres.poly
	
	choro.values <- X
	X[tiny, ] <- NA
	
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
                				   max_levels=legend.max.categories)
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
        					   max_levels=legend.max.categories)
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
		 choro.fill.legend.labels=choro.legend.labels,
		 choro.fill.legend.palette=choro.legend.palette,
		 choro.fill.legend.misc=list(choro.values=choro.values, choro.breaks=choro.breaks),
		 xfill=x)
}
