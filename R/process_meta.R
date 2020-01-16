process_meta <- function(gt, gf, gg, gc, gl, gsb, gcomp, glab, gmm, nx, nxa, panel.names, along.names, layer_vary, gm, any.legend, interactive) {
	attr.color <- aes.colors <- aes.color <- pc <- NULL
	xlab.rotation <- xlab.text <- ylab.rotation <- ylab.text <- NULL
	fontface <- fontfamily <- NULL
	compass.text.size <- NULL
	
	credit.show <- !is.null(gc)
	logo.show <- !is.null(gl)
	scale.show <- !is.null(gsb)
	compass.show <- !is.null(gcomp)
	
	gf <- within(gf, {
		by <- NULL
		if (is.na(ncol) && is.na(nrow)) {
			nrowcol <- get_arrangement(nx = nxa, asp_ratio = gm$shape.asp_ratio)
			nrow <- nrowcol[1]
			ncol <- nrowcol[2]
		} else {
			if (is.na(ncol)) ncol <- ceiling(nxa / nrow)
			if (is.na(nrow)) nrow <- ceiling(nxa / ncol)
		}
	})
	
	m <- gf$ncol * gf$nrow
	
	legend.only <- legend.frame <- legend.bg.alpha <- legend.hist.bg.alpha <- title.bg.alpha <- NULL
	freescales <- names(gf)[substr(names(gf), 1, 11) == "free.scales"]
	
	gt <- within(gt, {
		#nxa <- nxa
		
		## number of pages (np) and number of plots (small multiples) per page (pp)
		if (length(along.names)==1) {
			# wrap of plots
			np <- ceiling(nx / (gf$nrow * gf$ncol))
			pp <- min(gf$nrow * gf$ncol, nx)
			along <- FALSE
		} else {
			# plots along
			np <- nx / nxa
			pp <- nxa
			along <- TRUE
		}
		layer_vary <- layer_vary

		if (!any.legend || !legend.show) {
			if (legend.only) stop("No legend to show.", call.=FALSE)
			legend.show <- FALSE
			legend.outside <- FALSE
		} else {
			if (is.na(legend.outside)) legend.outside <- (pp > 1) && !any(vapply(gf[freescales], "[", logical(1), 1))
		}
		
		if (legend.outside) {
			title.snap.to.legend <- TRUE
		} else if (is.na(title.snap.to.legend)) {
			title.snap.to.legend <- FALSE
		}

		if (is.na(panel.show)) panel.show <- !is.na(panel.names[1]) || !is.ena(panel.labels[1])
		if (legend.only) {
			title <- rep("", nx)
			legend.width <- .9
			legend.height <- .9
			main.title <- rep("", np)
		} else {
			if (nx>1) {
				title <- rep(nonna_text(title), length.out=nx)
				
				if (panel.show) {
					if (is.ena(panel.labels[1])) {
						if (!is.na(panel.names[1])) {
							panel.labels <- panel.names
						} else panel.labels <- rep("", nx)
					} else if (is.list(panel.names)) {
						if (!is.list(panel.labels) || length(panel.labels)!=2) stop("for cross table facets, panel.labels should be a list containing the row names in the first, and column names in the second item.", call. = FALSE)
						if (length(panel.labels[[1]])!=length(panel.names[[1]])) stop("number of row names incorrect", call.=FALSE)
						if (length(panel.labels[[2]])!=length(panel.names[[2]])) stop("number of column names incorrect", call.=FALSE)
					} else if (is.list(panel.labels)) stop("unable to use row and column names unless panel.show in tm_layout is TRUE", call.=FALSE)
				}
				
				#if (title.snap.to.legend) title <- title[1]
			} else {
				if (is.ena(panel.labels[1])) {
					if (!is.na(panel.names[1])) {
						panel.labels <- panel.names[1]
					} else panel.labels <- ""
				}
				title <- nonna_text(title[1])
			}
			if (panel.show) {
				panel.names <- panel.labels
				if (nxa > 1 && !is.list(panel.names)) {
					panel.names <- rep(panel.names, np)
				}
			}
			
			if (!is.na(main.title[1]) || is.na(along.names[1])) {
				main.title <- rep(nonna_text(main.title), length.out=np)
			} else {
				main.title <- along.names	
			}
		}

		if (gm$shape.asp_ratio>1) {
			asp_w <- 1
			asp_h <- 1/gm$shape.asp_ratio
		} else {
			asp_w <- gm$shape.asp_ratio
			asp_h <- 1
		}
		scale.extra <- (min(1/ (asp_w * gf$ncol), 1 / (asp_h * gf$nrow))) ^ (1/gf$scale.factor)
		asp_w <- asp_h <- NULL
		scale <- scale * scale.extra

		title.size <- title.size * scale
		legend.title.size <- legend.title.size * scale
		legend.text.size <- legend.text.size * scale
		legend.hist.size <- legend.hist.size * scale
		
		panel.label.size <- panel.label.size * scale
				
		#if (is.null(bg.color)) bg.color <- "white"
		space.color <- ifelse(is.null(space.color), bg.color, space.color[1])
		earth.boundary.color <- ifelse(is.null(earth.boundary.color), attr.color, earth.boundary.color[1])
		legend.text.color <-  ifelse(is.null(legend.text.color), attr.color, legend.text.color[1])
		legend.title.color <- ifelse(is.null(legend.title.color), attr.color, legend.title.color[1])
		title.color <- ifelse(is.null(title.color), attr.color, title.color[1])

		if (is.null(legend.hist.width)) legend.hist.width <- legend.width
		
		
		legend.inside.box <- if (!is.logical(legend.frame)) TRUE else legend.frame
		if (identical(title.bg.color, TRUE)) title.bg.color <- bg.color
		
		if (identical(frame, TRUE)) frame <- attr.color else if (identical(frame, FALSE)) frame <- NA 

		if (is.logical(legend.frame)) if (identical(legend.frame, TRUE)) legend.frame <- attr.color else legend.frame <- NA 
# 		
# 		between.margin.in <- convertHeight(unit(between.margin, "lines") * scale, "inch", valueOnly=TRUE)
# 		
# 		between.margin.y <-convertHeight(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE) * gf$nrow
# 		between.margin.x <-convertWidth(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE) * gf$ncol
# 		
		
		outer.margins <- rep(outer.margins, length.out=4)
		#outer.margins <- rep(0, length.out=4)
		# 		if (m>1) {
# 			outer.margins <- c(between.margin.y, between.margin.x, between.margin.y, between.margin.x)
# 		}
		
		inner.margins <- if (is.na(inner.margins[1])) {
			if (gm$shape.is_raster_master) rep(0, 4) else rep(0.02, 4)
		} else rep(inner.margins, length.out=4)
		
 		attr.color.light <- is_light(attr.color)
 		aes.color.light <- is_light(aes.colors)

		title.color <- do.call("process_color", c(list(col=title.color), pc))
		main.title.color <- do.call("process_color", c(list(col=main.title.color), pc))
		legend.text.color <- do.call("process_color", c(list(col=legend.text.color), pc))
		legend.title.color <- do.call("process_color", c(list(col=legend.title.color), pc))
		if (!is.na(frame)) frame <- do.call("process_color", c(list(col=frame), pc))
		if (!is.na(legend.frame)) legend.frame <- do.call("process_color", c(list(col=legend.frame), pc))
		
		panel.label.color <- do.call("process_color", c(list(col=panel.label.color), pc))
		panel.label.bg.color <- do.call("process_color", c(list(col=panel.label.bg.color), pc))
		
		#if (is.na(earth.boundary.color)) earth.boundary.color <- attr.color
		earth.boundary.color <- do.call("process_color", c(list(col=earth.boundary.color), pc))
		
		#attr.color <- do.call("process_color", c(list(col=attr.color), pc))
		bg.color <- do.call("process_color", c(list(col=bg.color), pc))

		if (!is.null(outer.bg.color)) outer.bg.color <- do.call("process_color", c(list(col=outer.bg.color), pc))
		
		if (is.na(legend.bg.color)) legend.bg.color <- !is.na(legend.frame)
		if (!is.na(legend.bg.color)) {
			legend.bg.color <- if (identical(legend.bg.color, FALSE)) {
				NA
			} else if (identical(legend.bg.color, TRUE)) {
				bg.color
			} else {
				do.call("process_color", c(list(col=legend.bg.color, alpha=legend.bg.alpha), pc))				}
		} 
		if (!is.na(legend.hist.bg.color)) legend.hist.bg.color <- do.call("process_color", c(list(col=legend.hist.bg.color, alpha=legend.hist.bg.alpha), pc))
		if (!is.na(title.bg.color)) title.bg.color <- do.call("process_color", c(list(col=title.bg.color, alpha=title.bg.alpha), pc))
		if (!is.na(earth.boundary.color)) earth.boundary.color <- do.call("process_color", c(list(col=earth.boundary.color), pc))
		space.color <- do.call("process_color", c(list(col=space.color), pc))
		
		earth.bounds <- if (is.logical(earth.boundary)) {
			c(-180, -90, 180, 90)
		} else {
			as.vector(bb(earth.boundary))
		}
		earth.boundary <- !identical(earth.boundary, FALSE)
		
		earth.boundary.lwd <- earth.boundary.lwd * scale
		frame.lwd <- frame.lwd * scale
		
		if (is.na(attr.outside.size)) attr.outside.size <- (credit.show*.1 + logo.show*.15 + scale.show*.1 + compass.show * .15) * scale
		
		# set font face and family
		
		if (is.null(legend.title.fontface)) legend.title.fontface <- fontface
		if (is.null(legend.title.fontfamily)) legend.title.fontfamily <- fontfamily

		if (is.null(legend.text.fontface)) legend.text.fontface <- fontface
		if (is.null(legend.text.fontfamily)) legend.text.fontfamily <- fontfamily
		
		if (is.null(title.fontface)) title.fontface <- fontface
		if (is.null(title.fontfamily)) title.fontfamily <- fontfamily
		
		if (is.null(main.title.fontface)) main.title.fontface <- fontface
		if (is.null(main.title.fontfamily)) main.title.fontfamily <- fontfamily
		
		if (is.null(panel.label.fontface)) panel.label.fontface <- fontface
		if (is.null(panel.label.fontfamily)) panel.label.fontfamily <- fontfamily
		
		
		## overrule margins if interactive
		if (interactive) {
			inner.margins <- rep(0, 4)
			outer.margins <- rep(0, 4)
			asp <- NA
			if (title[1]=="" && !is.ena(panel.names[1]) && panel.names[1]!="") {
				if (is.list(panel.names)) {
					title <- unlist(lapply(panel.names[[1]], function(n1) {
						lapply(panel.names[[2]], function(n2) {
							paste(n1, n2, sep = " / ")
						})
					}))
				} else {
					title <- panel.names
				}
				
			}
		}
		
	})	

	gg <- process_meta_grid(gg, gt, interactive)

	if (credit.show) {
		gc <- within(gc, {
		 	credits.col[is.na(credits.col)] <- gt$attr.color
			credits.col <- do.call("process_color", c(list(col=credits.col), gt$pc))
			credits.size <- credits.size * gt$scale
			credits.fontface[is.na(credits.fontface)] <-gt$fontface
			credits.fontfamily[is.na(credits.fontfamily)] <-gt$fontfamily
			credits.text <- lapply(credits.text, rep, length.out=nx)
			credits.show <- lapply(credits.text, nonempty_text)
		})
	} else {
		gc <- list(credits.show=list(rep(FALSE, nx)))
	}
	
	if (logo.show) {
		gl <- within(gl, {
			# get local file names
			logo.file <- lapply(gl$logo.file, function(lf){
				# loop over different tm_logos
				lapply(lf, function(lf2) {
					lf3 <- lf2
					lf3[lf2!=""] <- tmap_icons(lf2[lf2!=""])$iconUrl
					lf3
				})
				# loop over different multiples
			})
			
			
			# one for each small multiple
			logo.height <- lapply(logo.height, function(lh) rep(lh, length.out=nx))
			logo.file <- lapply(logo.file, rep, length.out=nx)
			
			
			# make heights consistent with files
			logo.height <- mapply(function(lf, lh) {
				mapply(function(lf2, lh2) {
					hts <- rep(lh2, length.out=length(lf2))
					hts[lf2==""] <- 0
					hts * gt$scale
				}, lf, lh, SIMPLIFY=FALSE)
			}, logo.file, logo.height, SIMPLIFY=FALSE)
			
			# which ones to show
			logo.show <- lapply(logo.file, function(lf) vapply(lf, function(lf2) any(nonempty_text(lf2)), logical(1)))
			
			# calculate widths per icon
			logo.width <- mapply(function(lf, lh) {
				# loop over different tm_logos
				mapply(function(lf2, lh2) {
					# loop over different multiples
					mapply(function(lf3, lh3) {
						# loop over different icons in a row
						if (lf3=="") return(0)
						ti <- tmap_icons(lf3, height = 1e3 * lh3, width = 1e6 * lh3)
						ti$iconWidth / 1e3
					}, lf2, lh2, USE.NAMES = FALSE)
				}, lf, lh, SIMPLIFY=FALSE)
			}, logo.file, logo.height, SIMPLIFY=FALSE)
		})
	} else {
		gl <- list(logo.show=list(rep(FALSE, nx)))
	}	
	
	gsb <- process_meta_scale_bar(gsb, interactive, gt)
	
	compass.show.labels <- NULL
	if (compass.show) {
		gcomp <- within(gcomp, {
			if (is.na(compass.text.color)) compass.text.color <- gt$attr.color
			compass.text.color <- do.call("process_color", c(list(col=compass.text.color), gt$pc))
			
			if (is.na(compass.color.dark)) compass.color.dark <- ifelse(gt$attr.color.light, gt$attr.color, gt$attr.color)
			if (is.na(compass.color.light)) compass.color.light <- ifelse(gt$attr.color.light, "black", "white")
			compass.color.dark <- do.call("process_color", c(list(col=compass.color.dark), gt$pc))
			compass.color.light <- do.call("process_color", c(list(col=compass.color.light), gt$pc))
			
			compass.fontsize <- compass.text.size * gt$scale
			compass.lwd <- compass.lwd * gt$scale
			
			compass.show <- TRUE
			if (is.na(compass.type)) compass.type <- gt$compass.type
			if (is.na(compass.size)) compass.size <- switch(compass.type, arrow=2, radar=6, rose=6, 4)
			compass.nlines <- compass.size + ifelse(compass.show.labels==0, 0, ifelse(compass.show.labels==1, 1, 2))
		})
	} else {
		gcomp <- list(compass.show=FALSE)
	}
	
	if (!is.null(glab)) {
		glab <- within(glab, {
			if (!is.null(xlab.text)) {
				xlab.nlines <- if (xlab.rotation %in% c(90, 270)) {
					convertHeight(stringWidth(xlab.text), "lines", valueOnly = TRUE)	
				} else number_text_lines(xlab.text)
				#if (is.na(xlab.space)) xlab.space <- ifelse(gg$grid.show && !gg$grid.labels.inside.frame, gg$grid.labels.size / xlab.size, 0)
				xlab.size <- xlab.size * gt$scale
				xlab.show <- TRUE
			} else {
				xlab.show <- FALSE
			}
			if (!is.null(ylab.text)) {
				ylab.nlines <- if (ylab.rotation %in% c(0, 180)) {
					convertWidth(stringWidth(ylab.text), "lines", valueOnly = TRUE)	
				} else number_text_lines(ylab.text)
				#if (is.na(ylab.space)) ylab.space <- ifelse(gg$grid.show && !gg$grid.labels.inside.frame, gg$grid.labels.size / ylab.size, 0)
				ylab.size <- ylab.size * gt$scale
				ylab.show <- TRUE
			} else {
				ylab.show <- FALSE
			}
		})
	} else {
		glab <- list(xlab.show=FALSE, ylab.show=FALSE)
	}
	

	gt[c("compass.type", "compass.size")] <- NULL
	
	gmm <- process_meta_minimap(gmm, interactive, gt)
	
	c(gt, gf, gg, gc, gl, gsb, gcomp, glab, gmm, gm)
}

process_meta_minimap <- function(gmm, interactive, gt) {
	if (!is.null(gmm) && interactive) {
		if (is.na(gmm$minimap.position[1])) gmm$minimap.position <- gt$attr.position
		gmm$minimap.position <- find_leaflet_position(gmm$minimap.position)
		gmm$minimap.show <- TRUE
	} else {
		gmm <- list(minimap.show = FALSE)
	}
	gmm
}


find_leaflet_position <- function(position) {
	if (is_num_string(position[1])) {
		paste(ifelse(as.numeric(position[2])<.5, "bottom", "top"),
								ifelse(as.numeric(position[1])<.5, "left", "right"), sep="")
	} else {
		paste(ifelse(position[2] %in% c("BOTTOM", "bottom"), "bottom", "top"),
								ifelse(position[1] %in% c("LEFT", "left"), "left", "right"), sep="")
	}	
}


process_meta_scale_bar <- function(gsb, interactive, gt) {
	show.messages <- get("tmapOptions", envir = .TMAP_CACHE)$show.messages
	
	if (!is.null(gsb)) {
		gsb <- within(gsb, {
			if (!exists("scale.call")) scale.call <- ""
			if (interactive) {
				if ("breaks" %in% scale.call) warnings("In view mode, scale bar breaks are ignored.", call. = FALSE)
				
				if (is.na(scale.width))
					scale.width <- 100
				else if (scale.width < 1) {
					if (show.messages) message("Scale bar width set to 100 pixels")
					scale.width <- 100
				}
				
				if (is.na(scale.position[1])) scale.position <- gt$attr.position
				scale.position <- find_leaflet_position(scale.position)
			} else {
				if (all(c("breaks", "width") %in% scale.call)) {
					warning("For tm_scale_bar, breaks and width cannot be used together. The width is being ignored.", call. = FALSE)	
				}
				if ("breaks" %in% scale.call) {
					if (scale.breaks[1] != 0) {
						warning("First scale_bar breaks value should be 0.", call. = FALSE)
						scale.breaks <- c(0, scale.breaks)
					}
				}
				
				if (is.na(scale.width))
					scale.width <- .25
				else if (scale.width > 1) {
					if (show.messages) message("Scale bar width set to 0.25 of the map width")
					scale.width <- .25
				}
			}
			if (is.na(scale.text.color)) scale.text.color <- gt$attr.color
			scale.text.size <- scale.text.size * gt$scale
			scale.lwd <- scale.lwd * gt$scale
			scale.show <- TRUE
		})
	} else {
		gsb <- list(scale.show=FALSE)
	}
}

process_meta_grid <- function(gg, gt, interactive) {
	grid.alpha <- grid.labels.inside.frame <- grid.labels.rot <- NULL
	if (!is.null(gg)) {
		gg <- within(gg, {
			grid.show <- TRUE
			if (is.na(grid.col)) grid.col <- ifelse(gt$attr.color.light, darker(gt$attr.color, .5), lighter(gt$attr.color, .5))
			if (is.na(grid.labels.col)) grid.labels.col <- ifelse(gt$attr.color.light, darker(gt$attr.color, .2), lighter(gt$attr.color, .2))
			if (!is.numeric(grid.labels.rot) || length(grid.labels.rot) != 2) stop("labels.rot should be a numeric vector of length two")
			grid.col <- do.call("process_color", c(list(col=grid.col, alpha=grid.alpha), gt$pc))
			grid.labels.col <- do.call("process_color", c(list(col=grid.labels.col), gt$pc))
			grid.lwd <- grid.lwd * gt$scale
			grid.is.projected <- !(grid.projection=="longlat" || tryCatch(sf::st_is_longlat(grid.projection), error = function(e) TRUE))
			
			grid.projection <- get_proj4(grid.projection, output = "crs")
			
			if (!interactive && !grid.labels.inside.frame && any(gt$outer.margins[1:2]==0)) stop("When grid labels are plotted outside the frame, outer.margins (the bottom and the left) should be greater than 0. When using tmap_save, notice that outer.margins are set to 0 by default, unless set to NA.")
			if (!"scientific" %in% names(grid.labels.format)) grid.labels.format$scientific <- FALSE
			if (!"digits" %in% names(grid.labels.format)) grid.labels.format$digits <- NA
			
		})
	} else {
		gg <- list(grid.show=FALSE)
	}
}
