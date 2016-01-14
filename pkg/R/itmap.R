#' Interactive thematic map
#' 
#' Create an interactive map
#' 
#' @param tm tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @importFrom geosphere distGeo
#' @import leaflet
#' @importFrom htmltools htmlEscape
#' @example ../examples/itmap.R
#' @export
itmap <- function(tm) {
	x <- print(tm, plot=FALSE, interactive=TRUE)
	
	
	# take first small multiple
	gp <- x$gps[[1]]
	
	require(leaflet)
	lf <- leaflet() %>% addTiles()
	
	shps <- x$shps
	

	gt <- gp$tm_layout
	gp$tm_layout <- NULL
	

	e <- environment()
	
	mapply(function(shp, gpl) {
		bbx <- attr(shp, "bbox")
		
		if (inherits(shp, "Spatial")) {
			co <- get_sp_coordinates(shp, gpl, gt, bbx=bbox(shp))
		}
		
		plot_tm_fill <- function() {
			col <- do.call("process_color", c(list(gpl$col, alpha=gpl$alpha), gt$pc))
			
			fill <- if (!is.null(gpl$fill)) {
				fillRGBA <- col2rgb(gpl$fill, alpha = TRUE)
				fillColor <- rgb(fillRGBA[1,], fillRGBA[2,], fillRGBA[3,], maxColorValue = 255)
				fillOpacity <- fillRGBA[4,]/255
			} else {
				fillColor <- NULL
				fillOpacity <- 0
			}
			
			popups <- format_popups(gpl$fill.names, gpl$xfill, list(gpl$fill.values))
			if (length(popups)==1 && popups[1]=="") popups <- NULL

			lf <- lf %>% addPolygons(data=shp, stroke=gpl$lwd>0 && !is.na(col), weight=gpl$lwd, color=col, fillColor = fillColor, fillOpacity = fillOpacity, popup = popups, options = pathOptions(clickable=!is.null(popups)))
			if (!is.null(gpl$fill.legend.show) && gpl$fill.legend.show) {
				legendRGBA <- col2rgb(gpl$fill.legend.palette, alpha = TRUE)
				legendColor <- rgb(legendRGBA[1,], legendRGBA[2,], legendRGBA[3,], maxColorValue = 255)
				title <- if (gpl$fill.legend.title=="") NULL else gpl$fill.legend.title
				lf <- lf %>% addLegend(colors=legendColor, labels = gpl$fill.legend.labels, opacity=fillOpacity[1], title=title)
			}
			assign("lf", lf, envir = e)
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
			fillOpacity <- fillRGBA[4,1]/255
			
			bubble.size <- gpl$bubble.size

			popups <- format_popups(gpl$bubble.names, c(gpl$xsize, gpl$xcol), list(gpl$bubble.size.values, gpl$bubble.col.values))

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
			require(geosphere)
			
			# calculate top-center to bottom-center
			vdist <- distGeo(p1=c(mean(bbx[1,]), bbx[2,1]),
							 p2=c(mean(bbx[1,]), bbx[2,2]))
			
			rad <- bubble.size2 * vdist/max_lines
			
			lf <- lf %>% addCircles(lng=co2[,1], lat=co2[,2], fill = any(!is.na(fillColor2)), fillColor = fillColor2, fillOpacity=fillOpacity, color = gpl$bubble.border.col, radius=rad, weight =1, popup=popups2)
			assign("lf", lf, envir = e)
			NULL
			
		}
		plot_tm_text <- function() {
			stop("Text not implemented yet")
		}
		plot_tm_raster <- function() {
			stop("Raster not implemented yet")
		}
		
		e2 <- environment()
		
		fnames <- paste("plot", gpl$plot.order, sep="_")
		lf_layers <- lapply(fnames, do.call, args=list(), envir=e2)

	}, shps, gp)
	lf
}


format_popups <- function(id=NULL, titles, values) {
	isnull <- sapply(values, is.null)
	
	titles <- titles[!isnull]
	values <- values[!isnull]
	
	if (!is.null(id)) {
		labels <- paste("<b>", htmlEscape(id), "</b><br>", sep="")
	} else {
		labels <- ""
	}
	labels2 <- mapply(function(l, v) {
		htmlEscape(paste(l, v, sep=" = "))
	}, titles, values, SIMPLIFY=FALSE)
	
	labels3 <- do.call("paste", c(labels2, list(sep="<br>")))
	paste(labels, labels3, sep="")
}
