islistof = function(x, class) {
	is.list(x) && all(vapply(x, inherits, logical(1), what = class))
}

select_sf = function(shpTM, dt) {
	shp = shpTM$shp
	stid = shpTM$tmapID

	dtid = dt$tmapID__

	tid = intersect(stid, dtid)

	d = data.table(sord = seq_along(tid), ord = dt$ord__[match(tid, dtid)], tid = tid)
	if ("ord" %in% names(d)) {
		setkeyv(d, cols = c("ord", "sord"))
	} else {
		setkeyv(d, cols = "sord")
	}
	sid = match(d$tid, stid)

	shpSel = shp[sid] #sf::st_cast(shp[match(tid, tmapID)], "MULTIPOLYGON")

	# assign prop_ vectors to data dt (to be used in plotting) e.g. prop_angle is determined in tmapTransCentroid when along_lines = TRUE
	prop_vars = names(shpTM)[substr(names(shpTM), 1, 5) == "prop_"]
	if (length(prop_vars)) {
		for (p in prop_vars) {
			pname = substr(p, 6, nchar(p))
			dt[[pname]] = shpTM[[p]][sid]
		}
	}

	dt = dt[match(d$tid, dtid), ]
	list(shp = shpSel, dt = dt)
}

get_nby = function(fl) {
	vapply(fl, function(f) {
		if (is.integer(f)) f else length(f)
	}, integer(1))
}

get_row = function(i, nrows) {
	as.integer((i - 1) %% nrows + 1)
}

get_col = function(i, nrows, ncols) {
	as.integer((((i - 1) %/% nrows + 1) - 1) %% ncols + 1)
}

get_page = function(i, nrows, ncols) {
	as.integer(i - 1) %/% (nrows * ncols) + 1
}

get_i = function(ir, ic, ip, nby) {
	ir + (ic - 1) * nby[1] + (ip - 1) * prod(nby[1:2])
}

completeDT = function(DT, cols, defs = NULL){
	mDT = do.call(CJ, c(DT[, ..cols], list(unique=TRUE)))
	res = DT[mDT, on=names(mDT)]
	if (length(defs))
		res[, names(defs) := Map(replace, .SD, lapply(.SD, is.na), defs), .SDcols=names(defs)]
	res[]
}

completeDT2 = function(DT, cols, defs = NULL){
	mDT = do.call(CJ, cols)
	res = DT[mDT, on=names(mDT)]
	if (length(defs))
		res[, names(defs) := Map(replace, .SD, lapply(.SD, is.na), defs), .SDcols=names(defs)]
	res[]
}

cont_breaks = function(breaks, n=101) {
	x = round(seq(1, n, length.out=length(breaks)))

	unlist(lapply(1L:(length(breaks)-1L), function(i) {
		y = seq(breaks[i], breaks[i+1], length.out=x[i+1]-x[i]+1)
		if (i!=1) y[-1] else y
	}), use.names = FALSE)
}

prettyCount = function(x, n, ...) {
	x = na.omit(x)
	if (!length(x)) return(x)

	if (!is.integer(x)) x = as.integer(x)

	mn = min(x)
	mx = max(x)

	any0 = any(x==0)

	if (mn < 0) {
		n = floor(n / 2)
		pneg = -rev(prettyCount(-x[x<0], n = n, ...)) + 1L
		pneg = pneg[pneg != 0L]
		x = x[x>0]
		any0 = TRUE
	} else {
		pneg = integer()
	}

	if (any0) x = x[x!=0L]

	p = pretty(x - 1L, n = n, ...) + 1L

	p = p[(p %% 1) == 0]
	p = p[p!=0L]

	if (length(x) < 2) {
		if (any0) return(c(0L, p)) else return(p)
	}

	step = p[2] - p[1]
	if (p[length(p)] == mx) p = c(p, mx+step)

	if (any0) {
		c(pneg, 0L, p)
	} else {
		c(pneg, p)
	}
}

valid_colors = function(x) {
	is.na(x) | (x %in% colors()) |	(vapply(gregexpr("^#(([[:xdigit:]]){6}|([[:xdigit:]]){8})$", x), "[[", integer(1), 1) == 1L)
}

col2hex = function(x) {
	y = apply(col2rgb(x), MARGIN=2, FUN=function(y)do.call(rgb, c(as.list(y), list(maxColorValue=255))))
	y[is.na(x)] = NA
	y
}

# get aspect ratio of a shape
get_asp_ratio = function (x, width = 700, height = 700, res = 100)  {
	asp = if (inherits(x, "tmap")) {
		tmp = tempfile(fileext = ".png")
		png(tmp, width = width, height = height, res = res)
		mode = options(tmap.mode = "plot")
		on.exit({
			do.call(options, mode)
		})
		asp = print(x, return.asp = TRUE)
		dev.off()
		asp
	} else {
		bbx = sf::st_bbox(x)
		crs = sf::st_crs(x)

		ll = sf::st_is_longlat(crs)

		xlim = bbx[c(1, 3)]
		ylim = bbx[c(2, 4)]
		if (diff(xlim) == 0 || diff(ylim) == 0) {
			1
		} else unname((diff(xlim)/diff(ylim)) * ifelse(identical(ll, TRUE),cos((mean(ylim) * pi)/180), 1))
	}
	asp
}



# get aspect ratios of a list of bounding boxes
get_asp = function(bbxl) {
	vapply(bbxl, function(bbxi) {
		if (is.na(bbxi)) NA_real_ else get_asp_ratio(bbxi)
	}, FUN.VALUE = numeric(1))
}

leaflet2crs = function(x) {
	epsg = get_epsg(x)
	if (!is.na(epsg)) {
		sf::st_crs(epsg)
	} else if (!is.null(x$proj4def)) {
		sf::st_crs(x$proj4def)
	} else {
		stop("Unable to extract crs from leafletCRS object")
	}
}

leafletSimple  = structure(list(crsClass = "L.CRS.Simple", code = NULL, proj4def = NULL,
								projectedBounds = NULL, options = structure(list(), .Names = character(0))), class = "leaflet_crs")

crs2leaflet = function(x) {
	epsg = get_epsg(x)
	if (!is.na(epsg) && (epsg %in% c(3857, 4326, 3395))) {
		epsg2 = 3857 # somehow L.CRS.EPSG4326 doesn't work well (?)
		structure(list(crsClass = paste0("L.CRS.EPSG", epsg2), code = NULL, proj4def = NULL,
					   projectedBounds = NULL, options = structure(list(), .Names = character(0))), class = "leaflet_crs")
	} else {
		leafletSimple
		#stop("Unable to extract leaflet crs from sf crs object")
	}
}

get_epsg = function (x) {
	if (is.numeric(x)) {
		x
	} else if (inherits(x, "leaflet_crs")) {
		if (grepl("EPSG", x$crsClass, fixed = TRUE)) {
			substr(x$crsClass, 11, 14) # one of L.CRS.EPSG3857, L.CRS.EPSG4326, L.CRS.EPSG3395
		} else {
			NA_integer_
		}
	} else {
		x = sf::st_crs(x)
		# from sf
		if (is.na(x))
			NA_integer_
		else if (grepl("^EPSG:", x[["input"]]))
			as.integer(gsub("^EPSG:(\\d+)\\b.*$", "\\1", x[["input"]]))
		else {
			# not exported by sf:
			# crs_parameters(x, with_units = FALSE)[["epsg"]]
			NA_integer_
		}
	}
}

number_text_lines = function(txt) {
	if (is.character(txt)) {
		length(strsplit(txt, "\n")[[1]])
	} else 1
}

nonempty_text = function(txt) {
	if (is.character(txt)) {
		txt!=""
	} else rep(TRUE, length(txt))
}

cont_split = function(x) strsplit(x, split = "_", fixed=TRUE)
cont_collapse = function(x) sapply(x, paste, collapse="_")


without_units = function(x) {
	if (inherits(x, "units")) units::drop_units(x) else x
}


get_midpoint = function (coords) {
	dist = sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
	dist_mid = sum(dist)/2
	dist_cum = c(0, cumsum(dist))
	end_index = which(dist_cum > dist_mid)[1]
	start_index = end_index - 1
	start = coords[start_index, ]
	end = coords[end_index, ]
	dist_remaining = dist_mid - dist_cum[start_index]
	start + (end - start) * (dist_remaining/dist[start_index])

}

# copied from tmap3, may need updating
process_just = function(just, interactive) {
	show.messages = get("tmapOptions", envir = .TMAP)$show.messages
	show.warnings = get("tmapOptions", envir = .TMAP)$show.warnings

	n = length(just)
	isnum = is_num_string(just)

	if (!all(isnum | (just %in% c("left", "right", "top", "bottom", "center", "centre"))) && show.warnings) {
		warning("wrong specification of argument just", call. = FALSE)
	}

	just[just == "centre"] = "center"

	if (interactive) {
		just = just[1]
		if (n > 1 && show.messages) message("In interactive mode, the just argument should be one element")

		if (isnum[1]) {
			justnum = as.numeric(just)
			just = ifelse(justnum < .25, "left",
						  ifelse(justnum > .75, "right", "center"))
			if (show.messages) message("In interactive mode, just cannot be a numeric value. Therefore, ", justnum, " has been cenverted to \"", just, "\".")
		}
	} else {
		if (n > 2 && show.warnings) warning("The just argument should be a single value or a vector of 2 values.", call. = FALSE)
		if (n == 1) {
			if (just %in% c("top", "bottom")) {
				just = c("center", just)
				isnum = c(FALSE, isnum)
			} else {
				just = c(just, "center")
				isnum = c(isnum, FALSE)
			}
		}

		x = ifelse(isnum[1], as.numeric(just[1]),
					ifelse(just[1] == "left", 0,
						   ifelse(just[1] == "right", 1,
						   	   ifelse(just[1] == "center", .5, NA))))
		if (is.na(x)) {
			if (show.warnings) warning("wrong specification of argument just", call. = FALSE)
			x = 0.5
		}

		y = ifelse(isnum[2], as.numeric(just[2]),
				   ifelse(just[2] == "bottom", 0,
						  ifelse(just[2] == "top", 1,
						  	   ifelse(just[2] == "center", .5, NA))))
		if (is.na(y)) {
			if (show.warnings) warning("wrong specification of argument just", call. = FALSE)
			y = 0.5
		}
		just = c(x, y)
	}
	just
}






################!!!!! Functions below needed for Advanced text options !!!!####################

.grob2Poly = function(g) {
	x = convertX(g$x, unitTo = "native", valueOnly = TRUE)
	y = convertY(g$y, unitTo = "native", valueOnly = TRUE)
	if (inherits(g, "rect")) {
		w = convertWidth(g$width, unitTo = "native", valueOnly = TRUE)
		h = convertHeight(g$height, unitTo = "native", valueOnly = TRUE)
		x1 = x - .5*w
		x2 = x + .5*w
		y1 = y - .5*h
		y2 = y + .5*h
		polys = mapply(function(X1, X2, Y1, Y2) {
			sf::st_polygon(list(cbind(c(X1, X2, X2, X1, X1),
								  c(Y2, Y2, Y1, Y1, Y2))))
		}, x1, x2, y1, y2, SIMPLIFY = FALSE)
		sf::st_union(sf::st_sfc(polys))
	} else if (inherits(g, "polygon")) {
		xs = split(x, g$id)
		ys = split(y, g$id)

		polys = mapply(function(xi, yi) {
			co = cbind(xi, yi)
			sf::st_polygon(list(rbind(co, co[1,])))
		}, xs, ys, SIMPLIFY = FALSE)
		sf::st_union(sf::st_sfc(polys))
	} # else return(NULL)

}

polylineGrob2sfLines <- function(gL) {
	if (is.null(gL$id)) {
		ids = unlist(mapply(rep, 1L:length(gL$id.lengths), gL$id.lengths, SIMPLIFY = FALSE, USE.NAMES = FALSE))
	} else {
		ids = gL$id
	}
	coords = mapply(cbind, split(as.numeric(gL$x), ids), split(as.numeric(gL$y), ids), SIMPLIFY = FALSE)

	sf::st_sf(geometry = sf::st_sfc(sf::st_multilinestring(coords)))
}

npc_to_native <- function(x, scale) {
	x * (scale[2] - scale[1])# + scale[1]
}

native_to_npc_to_native <- function(x, scale) {
	#(x - scale[1]) / (scale[2] - scale[1])
	(x) / (scale[2] - scale[1])
}

.rectGrob2pathGrob <- function(rg, angles, bbx) {
	x = as.numeric(rg$x)
	y = as.numeric(rg$y)
	w = as.numeric(rg$width)
	h = as.numeric(rg$height)


	####################################
	### borrowed from pointLabel2
	####################################


	asp = tmaptools::get_asp_ratio(bbx)# * 1.25

	#xyAspect <- diff(boundary[c(1,2)]) / diff(boundary[c(3,4)])

	toUnityCoords <- function(xy) {
		if (asp > 1) {
			list(x = (xy$x - bbx[1])/(bbx[3] - bbx[1]) * asp,
				 y = (xy$y - bbx[2])/(bbx[4] - bbx[2]))
		} else {
			list(x = (xy$x - bbx[1])/(bbx[3] - bbx[1]),
				 y = (xy$y - bbx[2])/(bbx[4] - bbx[2])/asp)
		}


	}
	toUserCoords <- function(xy) {
		if (asp > 1) {
			list(x = bbx[1] + xy$x/asp * (bbx[3] - bbx[1]),
				 y = bbx[2] + xy$y * (bbx[4] - bbx[2]))
		} else {
			list(x = bbx[1] + xy$x * (bbx[3] - bbx[1]),
				 y = bbx[2] + xy$y * asp * (bbx[4] - bbx[2]))
		}

	}
	xy = grDevices::xy.coords(x, y, recycle = TRUE)
	z  = toUnityCoords(xy)
	x2 = z$x
	y2 = z$y

	# CHANGED: width and height are specified by user
	if (asp > 1) {
		w2 <- ((w) / (bbx[3] - bbx[1])) * asp
		h2 <- ((h) / (bbx[4] - bbx[2]))
	} else {
		w2 <- ((w) / (bbx[3] - bbx[1]))
		h2 <- ((h) / (bbx[4] - bbx[2])) / asp
	}


	####################################
	####################################
	####################################


	xs = c(x2 - w2/2, x2 + w2 / 2, x2 + w2 / 2, x2 - w2 / 2, x2 - w2/2)
	ys = c(y2 - h2/2, y2 - h2 / 2, y2 + h2 / 2, y2 + h2 / 2, y2 - h2/2)

	a <- atan2(h2, w2)
	#as <- as.vector(vapply(a, function(a)c(a,pi-a, pi+a,-a), numeric(4)))
	as <- as.vector(vapply(a, function(a)c(a,pi-a, pi+a,-a), numeric(4)))

	as2 <- as + rep(angles * pi / 180, each=4)

	dst <- rep(sqrt((w2/2)^2+(h2/2)^2), each=4)

	xs2 <- rep(x2, each=4) + cos(as2) * dst
	ys2 <- rep(y2, each=4) + sin(as2) * dst

	id <- rep(1:length(x), each=4)

	#w2 <- w + (h-w) * abs(cos(angles*pi/180))
	#h2 <- h + (w-h) * abs(sin(angles*pi/180))

	z2 <- grDevices::xy.coords(xs2, ys2, recycle = TRUE)
	xy2 <- toUserCoords(z2)

	list(poly=polygonGrob(unit(xy2$x, "native"), grid::unit(xy2$y, "native"), id=id, gp=rg$gp))
	#list(poly=rectGrob(unit(x, "native"), unit(y, "native"), width = unit(w, "native"), height=unit(h, "native"), gp = rg$gp))
}

.get_direction_angle <- function(co) {
	p1 <- co[1,]
	p2 <- co[nrow(co),]

	a <- atan2(p2[2] - p1[2], p2[1] - p1[1]) * 180 / pi
	if (a < 0) a <- a + 360
	a
}


.editGrob <- function(tg, sel, shiftX, shiftY, angles) {
	nt <- length(sel)
	angles <- rep(angles, length.out=nt)
	if (any(angles != 0)) {
		if (inherits(tg, "rect")) {
			tg <- .rectGrob2pathGrob(tg, angles)$poly
		}
	}
	tgx <- convertX(tg$x, "native", valueOnly = TRUE)
	tgy <- convertY(tg$y, "native", valueOnly = TRUE)

	if (inherits(tg, "polygon")) {
		sel4 <- rep(sel, each=4)
		tg$x <- unit(tgx + rep(shiftX, each=4), "native")[sel4]
		tg$y <- unit(tgy + rep(shiftY, each=4), "native")[sel4]
		tg$id <- rep(1:sum(sel), each=4)
	} else {
		tg$x <- unit(tgx + shiftX, "native")[sel]
		tg$y <- unit(tgy + shiftY, "native")[sel]
		if (inherits(tg, "rect")) {
			tg$height <- tg$height[sel]
			tg$width <- tg$width[sel]
		} else if (inherits(tg, "text")) {
			tg$label <- tg$label[sel]
			tg$rot <- angles[sel]
		}
	}
	tg$gp <- do.call("gpar", lapply(unclass(tg$gp)[names(tg$gp)!="font"], function(g) {
		if (length(g)==nt) g[sel] else g
	}))
	tg
}

# TODO remove function when we depend on R >= 4.4
`%||%` = function(x, y) {
	if (is.null(x)) y else x
}
