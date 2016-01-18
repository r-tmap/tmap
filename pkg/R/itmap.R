view_tmap <- function(gps, shps) {
	# take first small multiple
	gp <- gps[[1]]
	
	lf <- leaflet() %>% addProviderTiles("CartoDB.Positron", group = "Light gray") %>% addProviderTiles("CartoDB.DarkMatter", group = "Dark gray") %>% addProviderTiles("OpenTopoMap", group = "Topo")
	
	gt <- gp$tm_layout
	gp$tm_layout <- NULL
	
	e <- environment()
	id <- 1
	alpha <- gt$alpha
	popup.all.data <- gt$popup.all.data
	
	mapply(function(shp, gpl, shp_name) {
		bbx <- attr(shp, "bbox")
		
		if (inherits(shp, "Spatial")) {
			co <- get_sp_coordinates(shp, gpl, gt, bbx=bbox(shp))
		}
		
		plot_tm_fill <- function() {
			col <- do.call("process_color", c(list(gpl$col, alpha=gpl$alpha), gt$pc))
			
			if(!is.null(gpl$fill)) {
				fillRGBA <- col2rgb(gpl$fill, alpha = TRUE)
				fillColor <- rgb(fillRGBA[1,], fillRGBA[2,], fillRGBA[3,], maxColorValue = 255)
				fillOpacity <- unname(fillRGBA[4,]/255 * alpha)
				#fillColor[gpl$fill=="#00000000"] <- "#00000000"
			} else {
				fillColor <- NULL
				fillOpacity <- 0
			}

			dt <- gpl$data
			if (popup.all.data) {
				popups <- format_popups(gpl$fill.names, names(dt), dt)
			} else {
				popups <- format_popups(gpl$fill.names, gpl$xfill, list(gpl$fill.values))
			}

			if (length(popups)==1 && popups[1]=="") popups <- NULL

			lf <- lf %>% addPolygons(data=shp, stroke=gpl$lwd>0 && !is.na(col), weight=gpl$lwd, color=col, fillColor = fillColor, fillOpacity = fillOpacity, popup = popups, options = pathOptions(clickable=!is.null(popups)), group=shp_name, layerId = id)
			if (!is.null(gpl$fill.legend.show)) {
				legendRGBA <- col2rgb(gpl$fill.legend.palette, alpha = TRUE)
				legendColor <- rgb(legendRGBA[1,], legendRGBA[2,], legendRGBA[3,], maxColorValue = 255)
				title <- if (gpl$fill.legend.title=="") NULL else gpl$fill.legend.title
				lf <- lf %>% addLegend(colors=legendColor, labels = gpl$fill.legend.labels, opacity=max(fillOpacity), title=title)
			}
			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			NULL
		}
		
		plot_tm_lines <- function() {
			stop("Lines not implemented yet")
# 			col <- do.call("process_color", c(list(gpl$line.col, alpha=gpl$line.alpha), gt$pc))
# 			grid.shplines(shp, gp=gpar(col=col, lwd=gpl$line.lwd, lty=gpl$line.lty,
# 									   lineend="butt"), i, k)
		}
		
		plot_tm_bubbles <- function() {
			npol <- nrow(co)
			
			fill <- rep(gpl$bubble.col, length.out=npol)
			fillRGBA <- col2rgb(fill, alpha = TRUE)
			fillColor <- rgb(fillRGBA[1,], fillRGBA[2,], fillRGBA[3,], maxColorValue = 255)
			fillOpacity <- unname(fillRGBA[4,1]/255) * alpha
			
			
			bubble.size <- gpl$bubble.size

			dt <- gpl$data
			if (popup.all.data) {
				popups <- format_popups(gpl$bubble.names, names(dt), dt)
			} else {
				popups <- format_popups(gpl$bubble.names, c(gpl$xsize, gpl$xcol), c(list(gpl$bubble.size.values, gpl$bubble.col.values)))	
			}
			
			# sort bubbles
			if (length(bubble.size)!=1) {
				decreasing <- order(-bubble.size)
				co2 <- co[decreasing,]
				bubble.size2 <- bubble.size[decreasing]
				fillColor2 <- if (length(fillColor)==1) fillColor else fillColor[decreasing]
				popups2 <- popups[decreasing]
			} else {
				co2 <- co
				bubble.size2 <- bubble.size
				fillColor2 <- fillColor
				popups2 <- popups
			}
			
			max_lines <- par("din")[2]*10

			# calculate top-center to bottom-center
			vdist <- distGeo(p1=c(mean(bbx[1,]), bbx[2,1]),
							 p2=c(mean(bbx[1,]), bbx[2,2]))
			
			rad <- bubble.size2 * vdist/max_lines
			
			lf <- lf %>% addCircles(lng=co2[,1], lat=co2[,2], fill = any(!is.na(fillColor2)), fillColor = fillColor2, fillOpacity=fillOpacity, color = gpl$bubble.border.col, radius=rad, weight =1, popup=popups2, group=shp_name, layerId = id)
			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			NULL
			
		}
		plot_tm_text <- function() {
			#stop("Text not implemented yet")
			NULL
		}
		plot_tm_raster <- function() {
			shp@data@values <- match(gpl$raster, gpl$raster.legend.palette)
			legendRGBA <- col2rgb(gpl$raster.legend.palette, alpha = TRUE)
			legendColor <- rgb(legendRGBA[1,], legendRGBA[2,], legendRGBA[3,], maxColorValue = 255)
			legendOpacity <- unname(legendRGBA[4,1]/255) * alpha
			
# 			transNA <- which(gpl$raster.legend.palette=="#00000000")
# 			
# 			legendColor[transNA] <- "#00000000"
# 			
			lf <- lf %>% addRasterImage(x=shp, colors=legendColor, opacity = legendOpacity, group=shp_name, project = FALSE)
			if (!is.null(gpl$raster.legend.show)) {
# 				if (transNA) {
# 					legendColor <- legendColor[-transNA]
# 					legendLabels <- gpl$raster.legend.labels[-transNA]
# 				} else {
# 					
# 				}
				legendLabels <- gpl$raster.legend.labels
				title <- if (gpl$raster.legend.title=="") NULL else gpl$raster.legend.title
				lf <- lf %>% addLegend(colors=legendColor, labels = legendLabels, opacity=legendOpacity, title=title)
			}
			assign("lf", lf, envir = e)
			NULL
		}
		plot_tm_grid <- function() {
		}
		
		e2 <- environment()
		
		fnames <- paste("plot", gpl$plot.order, sep="_")
		lf_layers <- lapply(fnames, do.call, args=list(), envir=e2)

	}, shps, gp, gt$shp_name)
	lf %>% addLayersControl(baseGroups=c("Light gray", "Dark gray", "Topo"), overlayGroups = gt$shp_name, position=c("topleft"), options = layersControlOptions(autoZIndex = TRUE))
}


format_popups <- function(id=NULL, titles, values) {
	isnull <- sapply(values, is.null)
	
	titles <- titles[!isnull]
	values <- values[!isnull]
	
	if (!is.null(id)) {
		labels <- paste("<b>", htmlEscape(id), "</b>", sep="")
	} else {
		labels <- ""
	}
	
	titles_format <- sapply(titles, htmlEscape)
	values_format <- lapply(values, function(v) {
		htmlEscape(if (is.numeric(v)) fancy_breaks(v) else v)
	})
	

	labels2 <- mapply(function(l, v) {
		paste0("<tr><td style=\"color: #888888;\">", l, "</td><td>", v, "</td>")
	}, titles_format, values_format, SIMPLIFY=FALSE)
		
	labels3 <- paste0(do.call("paste", c(labels2, list(sep="</tr>"))), "</tr>")
	x <- paste("<div style=\"max-height:10em;overflow:auto;\"><table>
			   <thead><tr><th colspan=\"2\">", labels, "</th></thead></tr>", labels3, "</table></div>", sep="")
	x
}
