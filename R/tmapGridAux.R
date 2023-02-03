findZoom = function(b) {
	## calculate zoom level	
	# borrowed from https://github.com/dkahle/ggmap/blob/master/R/calc_zoom.r
	lon_diff = b[3] - b[1]
	lat_diff = b[4] - b[2]
	
	zoomlon = ceiling(log2(360 * 2/lon_diff))
	zoomlat = ceiling(log2(180 * 2/lat_diff))
	zoom = as.integer(min(zoomlon, zoomlat))
}

tmapGridTilesPrep = function(a, bs, o) {
	g = get("g", envir = .TMAP_GRID)
	
	if (!requireNamespace("maptiles")) stop("maptiles package is required", call. = FALSE)
	
	
	crs = sf::st_crs(bs[[1]])
	
	isproj = !sf::st_is_longlat(crs)
	
	if (isproj) {
		bs_orig = bs
		bs = lapply(bs, function(b) {
			sf::st_bbox(sf::st_transform(sf::st_as_sfc(b), crs = "EPSG:4326"))
		})
	}
	
	bs = lapply(bs, function(b) {
		bb_ll_valid(bb_asp(b, g$fasp))
	})
	
	
	if (is.na(a$zoom)) {
		zs = vapply(bs, findZoom, FUN.VALUE = integer(1))
	} else {
		zs = rep(a$zoom, length(bs))
	}

	xs = mapply(function(b, z) {

		m = tryCatch({
			maptiles::get_tiles(x = b, provider = a$server, zoom = z, crop = FALSE, )	
		}, error = function(e) {
			tryCatch({
				maptiles::get_tiles(x = b, provider = a$server, zoom = z - 1, crop = FALSE)	
			}, error = function(e) {
				NULL
			})
		})
		names(m)[1:3] = c("red", "green", "blue")
		m
	}, bs, zs, SIMPLIFY = FALSE)

	if (isproj) xs = mapply(function(x,b) {
		ex = terra::ext(as.vector(b[c(1,3,2,4)]))
		asp = (ex[2] - ex[1]) / (ex[4] - ex[3])
		
		tot = terra::ncell(x) * 2
		
		nc = round(sqrt(tot * asp))
		nr = round(tot / nc)

		r = terra::rast(ex, nrows = nr, ncols = nc, crs = crs$wkt)
		terra::project(x, r, method = "near")
	}, xs, bs_orig, SIMPLIFY = FALSE)
	
	
	ss = lapply(xs, function(x) {
		if (is.null(x)) NULL else do.call(tmapShape, list(shp = x, is.main = FALSE, crs = crs, bbox = NULL, unit=NULL, filter=NULL, shp_name = "x", smeta = list(), o = o))
	})
	
	srgb = tm_scale_rgb(maxValue = 255, value.na = "#FFFFFF")
	
	
	ds = lapply(ss, function(s) {
		if (is.null(s)) return(NULL)
		d = s$dt
		d[, c("col", "legnr") := do.call(srgb$FUN, list(x1 = red, x2 = green, x3 = blue, scale = srgb, legend = list(), o = o, aes = "col", layer = "raster", sortRev = NA, bypass_ord = TRUE))]
		d[, col_alpha:=1L]
		d
	})

	shpTMs = lapply(ss, function(s) {
		if (is.null(s)) NULL else s$shpTM	
	})


	
	g$bmaps_shpTHs = shpTMs
	g$bmaps_dts = ds

	assign("g", g, envir = .TMAP_GRID)
	paste0(a$server, collapse = "__")
}


grid_nonoverlap <- function(x, s) {
	n = length(x)
	x = c(-x[1], x, 1 + (1 - x[n]))
	
	m_left = x[2:(n+1)] - x[1:n]
	m_right = x[3:(n+2)] - x[2:(n+1)]
	m = pmin(m_left, m_right)
	
	m > s
}

pretty30 = function(x, n, longlat) {
	p = pretty(x, n)
	if (!longlat) return(p)
	step = p[2] - p[1]
	if (step > 50) {
		x = p %/% 60 * 60
		seq(min(x), max(x), by = 60)
	} else if (step > 10) {
		x = p %/% 30 * 30
		seq(min(x), max(x), by = 30)
	} else {
		p
	}
}


tmapGridGridPrep = function(a, bs, o) {
	g = get("g", envir = .TMAP_GRID)
	
	
	#alpha <- labels.inside.frame <- labels.rot <- NULL
	a2 = within(a, {
		show <- TRUE
		if (is.na(col)) col <- ifelse(o$attr.color.light, darker(o$attr.color, .5), lighter(o$attr.color, .5))
		if (is.na(labels.col)) labels.col <- ifelse(o$attr.color.light, darker(o$attr.color, .2), lighter(o$attr.color, .2))
		if (!is.numeric(labels.rot) || length(labels.rot) != 2) stop("labels.rot should be a numeric vector of length two")
		col <- do.call("process_color", c(list(col=col, alpha=alpha), o$pc))
		labels.col <- do.call("process_color", c(list(col=labels.col), o$pc))
		lwd <- lwd * o$scale
		is.projected <- !(projection=="longlat" || tryCatch(sf::st_is_longlat(projection), error = function(e) TRUE))
		
		projection <- sf::st_crs(projection)
		
		if (!labels.inside.frame && any(o$outer.margins[1:2]==0)) stop("When grid labels are plotted outside the frame, outer.margins (the bottom and the left) should be greater than 0. When using tmap_save, notice that outer.margins are set to 0 by default, unless set to NA.")
		if (!"scientific" %in% names(labels.format)) labels.format$scientific <- FALSE
		if (!"digits" %in% names(labels.format)) labels.format$digits <- NA
		
		labels.show <- rep(labels.show, length.out = 2)
		ticks <- rep(ticks, length.out = 2)
		
	})

	o$scale.extra = 1

	#grid.n.x <- grid.n.y <- projection <- grid.is.projected <- grid.ndiscr <- NULL
	
	a3s = lapply(bs, function(bbx) {
		proj = sf::st_crs(bbx)
		within(a2, { 
			if (!is.na(projection)) {
				bbx_orig <- bbx
				bbx <- suppressWarnings(bb(bbx, current.projection = proj, projection = projection))
			}
			
			## automatically determine number of grid lines
			if (is.na(n.x) && !is.na(n.y)) {
				n.x <- n.y * o$sasp
			} else if (!is.na(n.x) && is.na(n.y)) {
				n.y <- n.x / o$sasp
			} else if (is.na(n.x) && is.na(n.y)) {
				n.lines <- 15 / (o$scale / o$scale.extra)
				n.x <- round(o$sasp * (n.lines/(1+o$sasp)))
				n.y <- round(n.lines / (1+o$sasp))
			}
			
			## find natural breaks
			custom.x <- !is.na(x[1])
			custom.y <- !is.na(y[1])
			
			if (!custom.x) x <- pretty30(bbx[c(1,3)], n=n.x, longlat = !is.na(projection) || sf::st_is_longlat(proj))
			if (!custom.y) y <- pretty30(bbx[c(2,4)], n=n.y, longlat = !is.na(projection) || sf::st_is_longlat(proj))
			
			## copy x and y
			x.orig <- x
			y.orig <- y
			
			## crop
			x <- x[x>bbx[1] & x<bbx[3]]
			y <- y[y>bbx[2] & y<bbx[4]]
			
			## project grid lines
			if (!is.na(projection)) {
				## add extra grid lines to make sure the warped grid is full
				if (custom.x) {
					x2 <- x.orig
				} else {
					gnx2 <- floor(length(x))
					if (gnx2==1) {
						x2 <- x
					} else if (gnx2>1) {
						x2 <- c(rev(seq(x[1], by=-diff(x[1:2]), length.out = gnx2)),
									 x[-c(1, length(x))],
									 seq(x[length(x)], by=diff(x[1:2]), length.out = gnx2))
					} else x2 <- NA
				}
				if (custom.y) {
					y2 <- y.orig
				} else {
					gny2 <- floor(length(y))
					if (gny2==1) {
						y2 <- y
					} else if (gny2>1) {
						y2 <- c(rev(seq(y[1], by=-diff(y[1:2]), length.out = gny2)),
									 y[-c(1, length(y))],
									 seq(y[length(y)], by=diff(y[1:2]), length.out = gny2))
					} else y2 <- NA
				}
				if (!is.projected) {
					# x2[abs(x2-180)<1e-9] <- 180
					# x2[abs(x2- -180)<1e-9] <- -180
					# y2[abs(y2-90)<1e-9] <- 90
					# y2[abs(y2- -90)<1e-9] <- -90
					x2 <- x2[x2>=-180 & x2<=180]	
					y2 <- y2[y2>=-90 & y2<=90]	
				}
				gnx2 <- gny2 <- NULL
				
				## determine limits
				x2.min <- min(min(x2), bbx[1], na.rm=TRUE)
				x2.max <- max(max(x2), bbx[3], na.rm=TRUE)
				y2.min <- min(min(y2), bbx[2], na.rm=TRUE)
				y2.max <- max(max(y2), bbx[4], na.rm=TRUE)
				
				lnsSel <- c(length(x2) && !is.na(x2[1]),
							length(y2) && !is.na(y2[1]))
				
				if (lnsSel[1]) {
					lnsX = sf::st_sfc(lapply(x2, function(x) {
						sf::st_linestring(matrix(c(rep(x,ndiscr), seq(y2.min, y2.max, length.out=ndiscr)), ncol=2))
					}), crs = projection)
					lnsX_proj <- sf::st_transform(lnsX, crs = proj)
					lnsX_emp <- sf::st_is_empty(lnsX_proj)
					
					x2 <- x2[!lnsX_emp]
					lnsX_proj <- lnsX_proj[!lnsX_emp]
					xco <- st_coordinates(lnsX_proj)
					co.x.lns <- lapply(unique(xco[,3]), function(i) {
						lco <- xco[xco[,3]==i, 1:2]
						lco[, 1] <- (lco[, 1]-bbx_orig[1]) / (bbx_orig[3] - bbx_orig[1])
						lco[, 2] <- (lco[, 2]-bbx_orig[2]) / (bbx_orig[4] - bbx_orig[2])
						lco
					})
					lnsX <- NULL
					lnsX_proj <- NULL
					lnsX_emp <- NULL
					
					sel.x <- which(x2 %in% x)
				} else {
					co.x.lns <- numeric(0)
				}
				
				if (lnsSel[2]) {
					lnsY = sf::st_sfc(lapply(y2, function(y) {
						st_linestring(matrix(c(seq(x2.min, x2.max, length.out=ndiscr), rep(y,ndiscr)), ncol=2))
					}), crs = projection)
					lnsY_proj <- sf::st_transform(lnsY, crs = proj)
					lnsY_emp <- sf::st_is_empty(lnsY_proj)
					
					y2 <- y2[!lnsY_emp]
					lnsY_proj <- lnsY_proj[!lnsY_emp]
					yco <- st_coordinates(lnsY_proj)
					co.y.lns <- lapply(unique(yco[,3]), function(i) {
						lco <- yco[yco[,3]==i, 1:2]
						lco[, 1] <- (lco[, 1]-bbx_orig[1]) / (bbx_orig[3] - bbx_orig[1])
						lco[, 2] <- (lco[, 2]-bbx_orig[2]) / (bbx_orig[4] - bbx_orig[2])
						lco
					})
					lnsY <- NULL
					lnsY_proj <- NULL
					lnsY_emp <- NULL
					
					sel.y <- which(y2 %in% y)
				}	else {
					co.y.lns <- numeric(0)
				}
				
				
				labels.show <- labels.show & lnsSel # update needed for plot_n
			} else {
				# normalize coordinates
				co.x <- (x-bbx[1]) / (bbx[3] - bbx[1])
				co.y <- (y-bbx[2]) / (bbx[4] - bbx[2])
			}
			
			## format grid labels
			
			
			labels.x <- local({
				if (labels.cardinal) {
					xneg <- x < 0
					xpos <- x > 0
					
					xlab <- do.call("fancy_breaks", c(list(vec=abs(x), intervals=FALSE), o$labels.format)) #format(x, big.mark = ",")	
					
					xlab[xpos] <- paste0(xlab[xpos], "E")
					xlab[xneg] <- paste0(xlab[xneg], "W")
					xlab
				} else {
					do.call("fancy_breaks", c(list(vec=x, intervals=FALSE), o$labels.format)) #format(x, big.mark = ",")	
				}
			})
			
			
			labels.y <- local({
				if (labels.cardinal) {
					yneg <- y < 0
					ypos <- y > 0
					
					ylab <- do.call("fancy_breaks", c(list(vec=abs(y), intervals=FALSE), o$labels.format))
					
					ylab[ypos] <- paste0(ylab[ypos], "N")
					ylab[yneg] <- paste0(ylab[yneg], "S")
					ylab
				} else {
					do.call("fancy_breaks", c(list(vec=y, intervals=FALSE), o$labels.format))
				}
			})
			
		})
		
	})
	
	g$grid_comp_per_bbx = a3s
	
	assign("g", g, envir = .TMAP_GRID)
	
	return("grid")
}


tmapGridTiles = function(bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	g = get("g", envir = .TMAP_GRID)

	dt = g$bmaps_dts[[bi]]
	shpTM = g$bmaps_shpTHs[[bi]]
	gp = list()
	
	if (!is.null(dt)) tmapGridRaster(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o)	
}

tmapGridGrid = function(bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	g = get("g", envir = .TMAP_GRID)
	
	a3s = g$grid_comp_per_bbx[[bi]]
	gp = list()
	
	if (!is.null(dt)) tmapGridRaster(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o)	
	
}
