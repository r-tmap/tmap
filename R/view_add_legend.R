add_legend <- function(map, gpl, gt, aes, alpha, group, list.only=FALSE, zindex = NULL) {
	pal_name <- paste(aes, "legend.palette", sep=".")
	val_name <- paste(aes, "legend.values", sep=".")
	lab_name <- paste(aes, "legend.labels", sep=".")
	
	pal <- gpl[[pal_name]]
	val <- gpl[[val_name]]
	lab <- gpl[[lab_name]]
	
	if (!is.null(zindex)) {
		layerId <- legendName(zindex)
	} else {
		layerId <- NULL
	}
	
	if (nchar(pal[1])>10) {
		# check whether style is continuous
		style <- attr(pal, "style")
		is.cont <- TRUE
		incl.na <- nchar(pal[length(pal)]) < 10
		
		orig <- unlist(lapply(pal, function(x) {
			p <- strsplit(x, split = "-", fixed=TRUE)[[1]]
			if (length(p) == 1) NULL else p[p!="NA"]
		}), use.names = FALSE)
		
		
		pal <- vapply(pal, function(x) {
			p <- strsplit(x, split = "-", fixed=TRUE)[[1]]
			if (length(p)==1) p[1] else if (p[6]=="NA") p[5] else p[6]
		}, character(1))
		if (incl.na) {
			colNA <- unname(pal[length(pal)])
			pal <- pal[-length(pal)]
			textNA <- lab[length(lab)]
		} else {
			colNA <- NA
			textNA <- NA
		}
	} else {
		is.cont <- FALSE
		if (length(pal) != length(val)) {
			colNA <- pal[length(pal)]
			textNA <- lab[length(pal)]
			pal <- pal[-length(pal)]
			lab <- lab[-length(lab)]
		} else {
			colNA <- NA
			textNA <- NA
		}
		orig <- pal
	}
	
	allNAs <- (length(pal) == 0)
	
	if (allNAs) {
		col <- character()
		opacity <- alpha
	} else {
		RGBA <- col2rgb(pal, alpha = TRUE)
		col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
		opacity <- unname(RGBA[4,1]/255) * alpha
	}
	
	if (!is.na(colNA)) {
		RGBA_NA <- col2rgb(colNA, alpha = TRUE)
		colNA <- rgb(RGBA_NA[1,], RGBA_NA[2,], RGBA_NA[3,], maxColorValue = 255)
	}
	
	if (list.only) {
		if (!is.na(colNA)) orig <- c(orig, colNA)
		return(list(col=orig, opacity=opacity))
	}
	
	title_name <- paste(aes, "legend.title", sep=".")
	
	title <- if (nonempty_text(gpl[[title_name]])) expr_to_char(gpl[[title_name]]) else NULL
	
	legend.position <-gt$view.legend.position
	
	if (is.cont) {
		legvals <- if (!is.na(colNA)) c(val, NA) else val
		
		if (style=="quantile") {
			addLegend(map, position=legend.position, group = group,
					  pal=colorQuantile(col, val, na.color=colNA, alpha = FALSE), values=legvals, na.label = textNA, title=title, opacity=opacity, layerId = layerId)
		} else {
			addLegend(map, position=legend.position, group = group,
					  pal=colorNumeric(col, val, na.color=colNA, alpha = FALSE), values=legvals, na.label = textNA, title=title, opacity=opacity, layerId = layerId)
		}
	} else {
		if (allNAs) {
			addLegend(map, position=legend.position, group = group, colors=colNA, labels=textNA, title=title, opacity=opacity, layerId = layerId)
		} else {
			if (!is.na(colNA)) {
				legvals <- c(lab, textNA)
				col <- c(col, colNA)
			} else {
				legvals <- lab
			}
			addLegend(map, position=legend.position,
					  group = group,
					  colors = col,
					  labels = legvals,
					  title=title,
					  opacity=opacity,
					  layerId = layerId)
			
		}
	}
}
