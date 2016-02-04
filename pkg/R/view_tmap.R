view_tmap <- function(gps, shps) {
	# take first small multiple
	gp <- gps[[1]]
	gt <- gp$tm_layout
	gp$tm_layout <- NULL
	
	#lf <- leaflet() %>% addProviderTiles("CartoDB.Positron", group = "Light gray") %>% addProviderTiles("CartoDB.DarkMatter", group = "Dark gray") %>% addProviderTiles("OpenTopoMap", group = "Topo")

	lf <- leaflet()
	basemaps <- gt$basemaps

	# add base layer(s)
	if (length(basemaps)) {
		for (bm in basemaps) {
			lf <- lf %>% addProviderTiles(bm, group=bm)
		}
	}
	
	# add background overlay
	if (gt$bg.overlay.alpha!=0) {
		if (any(sapply(gp, function(gpl)!is.null(gpl$raster)))) {
			warning("Background overlays do not work yet with raster images. Background disabled.", call. = FALSE)
		} else {
			lf <- lf %>%  addRectangles(-540,-90,540,90, stroke=FALSE, fillColor=gt$bg.overlay, fillOpacity = gt$bg.overlay.alpha, layerId=0) 
			lf$x$limits <- NULL
		}
	}
	
		
	if (!length(gp)) {
		if (length(basemaps)>1) lf <- lf %>% addLayersControl(baseGroups=basemaps, options = layersControlOptions(autoZIndex = TRUE))
		
		if (!is.null(gt$bbx)) {
			lf <- lf %>% 
				fitBounds(gt$bbx[1], gt$bbx[2], gt$bbx[3], gt$bbx[4]) %>% 
				addMarkers(gt$center[1], gt$center[2])
		}
		return(lf)
	}
	
	e <- environment()
	id <- 1
	alpha <- gt$alpha
	popup.all.data <- gt$popup.all.data

	group_selection <- mapply(function(shp, gpl, shp_name) {
		bbx <- attr(shp, "bbox")
		upl <- units_per_line(bbx)
		bpl <- bbx_per_line(bbx)
		if (inherits(shp, "Spatial")) {
			res <- get_sp_coordinates(shp, gpl, gt, bbx)
			co <- res$co
			if (gt$line.center.type[1]=="segment") {
				gpl <- res$gpl
				shp <- res$shp
			}	
		}
		
		plot_tm_fill <- function() {
			bres <- split_alpha_channel(gpl$col, alpha=alpha)
			bcol <- bres$col
			bopacity <- bres$opacity
			
			fres <- split_alpha_channel(gpl$fill, alpha=alpha)
			fcol <- fres$col
			fopacity <- fres$opacity
			
			if (!is.null(gpl$fill)) {
				popups <- get_popups(gpl, type="fill", popup.all.data=popup.all.data)
			} else {
				popups <- NULL
			}
			stroke <- gpl$lwd>0 && !is.na(bcol) && bopacity!=0
			
			lf <- lf %>% addPolygons(data=shp, stroke=stroke, weight=gpl$lwd, color=bcol, fillColor = fcol, fillOpacity = fopacity, popup = popups, options = pathOptions(clickable=!is.null(popups)), group=shp_name, layerId = id)
			
			
			if (!is.na(gpl$xfill)) {
				if (gpl$fill.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="fill", alpha=alpha)
			}
			
			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			TRUE
		}
		
		plot_tm_lines <- function() {
			lres <- split_alpha_channel(gpl$line.col, alpha=alpha)
			lcol <- lres$col
			lopacity <- lres$opacity

			popups <- get_popups(gpl, type="line", popup.all.data=popup.all.data)
			dashArray <- lty2dashArray(gpl$line.lty)
			
			lf <- lf %>% addPolylines(data=shp, stroke=TRUE, weight=gpl$line.lwd, color=lcol, opacity = lopacity, popup = popups, options = pathOptions(clickable=!is.null(popups)), dashArray=dashArray, group=shp_name, layerId = id)

			
			if (!is.na(gpl$xline)) {
				if (gpl$line.col.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="line.col", alpha=alpha)
			}
			
			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			
			TRUE
			# 			col <- do.call("process_color", c(list(gpl$line.col, alpha=gpl$line.alpha), gt$pc))
			# 			grid.shplines(shp, gp=gpar(col=col, lwd=gpl$line.lwd, lty=gpl$line.lty,
			# 									   lineend="butt"), i, k)
		}
		
		plot_tm_bubbles <- function() {
			npol <- nrow(co)
			
			
			co[, 1] <- co[, 1] + gpl$bubble.xmod * bpl
			co[, 2] <- co[, 2] + gpl$bubble.ymod * bpl
			
			
			bres <- split_alpha_channel(gpl$bubble.border.col, alpha=alpha)
			bcol <- bres$col
			bopacity <- bres$opacity
			
			fres <- split_alpha_channel(rep(gpl$bubble.col, length.out=npol), alpha=alpha)
			fcol <- fres$col
			fopacity <- fres$opacity
			
			bubble.size <- gpl$bubble.size
			
			popups <- get_popups(gpl, type="bubble", popup.all.data=popup.all.data)

			# sort bubbles
			if (length(bubble.size)!=1) {
				decreasing <- order(-bubble.size)
				co2 <- co[decreasing,]
				bubble.size2 <- bubble.size[decreasing]
				fcol2 <- if (length(fcol)==1) fcol else fcol[decreasing]
				popups2 <- popups[decreasing]
			} else {
				co2 <- co
				bubble.size2 <- bubble.size
				fcol2 <- fcol
				popups2 <- popups
			}
			
			
			rad <- bubble.size2 * upl
			
			fixed <- ifelse(gpl$bubble.misc$bubble.are.dots, gt$dot.size.fixed, gt$bubble.size.fixed)
			if (fixed) {
				lf <- lf %>% addCircleMarkers(lng=co2[,1], lat=co2[,2], fill = any(!is.na(fcol2)), fillColor = fcol2, fillOpacity=fopacity, color = bcol, stroke = !is.na(bcol) && bopacity!=0, radius = 20*bubble.size2, weight = 1, popup=popups2, group=shp_name, layerId = id)
			} else {
				lf <- lf %>% addCircles(lng=co2[,1], lat=co2[,2], fill = any(!is.na(fcol2)), fillColor = fcol2, fillOpacity=fopacity, color = bcol, stroke = !is.na(bcol) && bopacity!=0, radius=rad, weight =1, popup=popups2, group=shp_name, layerId = id)
			}
			
			
			if (!is.na(gpl$xcol)) {
				if (gpl$bubble.col.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="bubble.col", alpha=alpha)
			}
			

			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			TRUE
			
		}
		plot_tm_text <- function() {
 			FALSE
		}
		plot_tm_raster <- function() {
			if (gpl$raster.misc$is.OpenStreetMap) return(FALSE)	
			if (is.na(gpl$xraster)) {
				gpl$raster.legend.palette <- unique(gpl$raster)
			}
			
			shp@data@values <- match(gpl$raster, gpl$raster.legend.palette)

			res_leg <- add_legend(map=NULL, gpl, gt, aes="raster", alpha = alpha, list.only=TRUE)
			
			lf <- lf %>% addRasterImage(x=shp, colors=res_leg$col, opacity = res_leg$opacity, group=shp_name, project = FALSE, layerId = id)
			
			if (!is.na(gpl$xraster)) {
				if (gpl$raster.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="raster", alpha=alpha)
			}

			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			TRUE
		}
		plot_tm_grid <- function() {
			FALSE
		}
		
		e2 <- environment()
		
		fnames <- paste("plot", gpl$plot.order, sep="_")
		layer_selection <- sapply(fnames, do.call, args=list(), envir=e2)
		any(layer_selection)
	}, shps, gp, gt$shp_name, SIMPLIFY = TRUE)
	
	groups <- gt$shp_name[group_selection]
	
	#center <- c(mean(lims[c(1,3)]), mean(lims[c(2,4)]))
	
	if (is.na(gt$control.position[1])) {
		control.position <- c("lefttop")
	} else if (!is.character(gt$control.position) || (!length(gt$control.position)==2)) {
		stop("Invalid control.position", call.=FALSE)
	} else if (gt$control.position[1] %in% c("left", "right") &&
		gt$control.position[2] %in% c("top", "bottom")) {
		control.position <- paste(gt$control.position[c(2,1)], collapse="")
	} else {
		stop("Invalid control.position", call.=FALSE)
	}

	
	
	lf <- lf %>% addLayersControl(baseGroups=basemaps, overlayGroups = groups, options = layersControlOptions(autoZIndex = TRUE), position=control.position)  
	if (!(identical(gt$set.bounds, FALSE))) {
		if (identical(gt$set.bounds, TRUE)) {
			lims <- unname(unlist(lf$x$limits)[c(3,1,4,2)])
		} else {
			lims <- gt$set.bounds
		}
		lf <- lf %>% setMaxBounds(lims[1], lims[2], lims[3],lims[4])
	}
	lf
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

split_alpha_channel <- function(x, alpha) {
	if (is.null(x)) {
		list(col=NULL, opacity=0)
	} else {
		RGBA <- col2rgb(x, alpha = TRUE)
		col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
		opacity <- unname(RGBA[4,]/255 * alpha)
		list(col=col, opacity=opacity)
	}
}
get_x_name <- function(type) {
	if (type=="fill") {
		"xfill"
	} else if (type=="bubble") {
		c("xsize", "xcol")
	} else if (type=="raster") {
		"xraster"
	} else if (type=="line") {
		c("xline", "xlwd")
	}
}

get_aes_name <- function(type) {
	if (type=="fill") {
		"fill"
	} else if (type=="bubble") {
		c("bubble.size", "bubble.col")
	} else if (type=="raster") {
		"raster"
	} else if (type=="line") {
		c("line.col", "line.lwd")
	}
}

get_popups <- function(gpl, type, popup.all.data) {
	var_names <- paste(type, "names", sep=".")
	var_x <- get_x_name(type)
	var_aes <- get_aes_name(type)
	
	var_values <- paste(var_aes, "values", sep=".")
	
	if (all(is.na(unlist(gpl[var_x])))) popup.all.data <- TRUE

	dt <- gpl$data
	if (popup.all.data) {
		popups <- format_popups(gpl[[var_names]], names(dt), dt)
	} else {
		if (type=="fill" && !is.null(gpl$fill.densities)) {
			gpl$xfill_dens <- paste(gpl[[paste(var_x)]], "density", sep="_")
			var_x <- c(var_x, "xfill_dens")
			var_values <- c(var_values, "fill.densities")
		}
		popups <- format_popups(gpl[[var_names]], unlist(gpl[var_x]), gpl[var_values])
	}
	if (length(popups)==1 && popups[1]=="") popups <- NULL
	popups
}

add_legend <- function(map, gpl, gt, aes, alpha, list.only=FALSE) {
	pal_name <- paste(aes, "legend.palette", sep=".")
	pal <- gpl[[pal_name]]
	
	if (nchar(pal[1])>10) {
		style <- attr(pal, "style")
		is.cont <- TRUE
		incl.na <- nchar(pal[length(pal)]) < 10
		pal <- sapply(pal, function(x) {
			p <- strsplit(x, split = "-", fixed=TRUE)[[1]]
			if (length(p)==1) p[1] else if (p[6]=="NA") p[5] else p[6]
		})
		if (incl.na) {
			colNA <- unname(pal[length(pal)])
			pal <- pal[-length(pal)]
			val <- as.numeric(gsub(",", "", gpl$fill.legend.labels, fixed = TRUE))
		} else {
			val <- as.numeric(gsub(",", "", gpl$fill.legend.labels, fixed = TRUE))
			colNA <- NA
		}
	} else {
		is.cont <- FALSE
	}
	RGBA <- col2rgb(pal, alpha = TRUE)
	col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
	opacity <- unname(RGBA[4,1]/255) * alpha
	
	if (list.only) {
		return(list(col=col, opacity=opacity))
	}
	
	title_name <- paste(aes, "legend.title", sep=".")
	lab_name <- paste(aes, "legend.labels", sep=".")
	
	title <- if (gpl[[title_name]]=="") NULL else gpl[[title_name]]

	legend.position <- gt$view.legend.position

	if (is.cont) {
		if (style=="quantile") {
			addLegend(map, position=legend.position,
					  pal=colorQuantile(pal, val, na.color=colNA), values=val)
		} else {
			addLegend(map, position=legend.position,
					  pal=colorNumeric(pal, val, na.color=colNA), values=val)
		}
	} else {
		addLegend(map, position=legend.position,
				  colors=col, labels = gpl[[lab_name]], opacity=opacity, title=title)
	}
}

working_internet <- function(url = "http://www.google.com") {
		
	# test the http capabilities of the current R build
	if (!capabilities(what = "http/ftp")) return(FALSE)
	
	# test connection by trying to read first line of url
	test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
	
	# return FALSE if test inherits 'try-error' class
	!inherits(test, "try-error")
}

bbx_per_line <- function(bbx) {
	max_lines <- par("din")[2]*10
	(bbx[2,2] - bbx[2,1]) / max_lines
}

units_per_line <- function(bbx) {
	max_lines <- par("din")[2]*10
	
	# calculate top-center to bottom-center
	vdist <- distGeo(p1=c(mean(bbx[1,]), bbx[2,1]),
					 p2=c(mean(bbx[1,]), bbx[2,2]))
	vdist/max_lines
}

lty2dashArray <- function(lty) {
	numlty <- switch(lty,
					 solid=0,
					 # These numbers taken from ?par
					 dashed=c(4, 4),
					 dotted=c(1, 3),
					 dotdash=c(1, 3, 4, 3),
					 longdash=c(7, 3),
					 twodash=c(2, 2, 6, 2),
					 # Otherwise we're a hex string
					 as.numeric(as.hexmode(strsplit(lty, "")[[1]])))
	paste(ifelse(numlty == 0,
				 "none",
				 numlty),
		  collapse=",")
}
