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
	
	shpSel = shp[match(d$tid, stid)] #st_cast(shp[match(tid, tmapID)], "MULTIPOLYGON")
	
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
	x = round(seq(1, 101, length.out=length(breaks)))
	
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
get_asp_ratio = function (x)  {
	bbx = sf::st_bbox(x)
	crs = sf::st_crs(x)
	
	ll = sf::st_is_longlat(crs)
	
	xlim = bbx[c(1, 3)]
	ylim = bbx[c(2, 4)]
	asp = if (diff(xlim) == 0 || diff(ylim) == 0) {
		1
	} else unname((diff(xlim)/diff(ylim)) * ifelse(identical(ll, TRUE),cos((mean(ylim) * pi)/180), 1))
	asp
}

# get aspect ratios of a list of bounding boxes
get_asp = function(bbxl) {
	vapply(bbxl, function(bbxi) {
		if (is.na(bbxi)) as.numeric(NA) else get_asp_ratio(bbxi)
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
	if (epsg %in% c(3857, 4326, 3395)) {
		structure(list(crsClass = paste0("L.CRS.EPSG", epsg), code = NULL, proj4def = NULL, 
					   projectedBounds = NULL, options = structure(list(), .Names = character(0))), class = "leaflet_crs")
	} else {
		stop("Unable to extract leaflet crs from sf crs object")
	}
}

get_epsg = function (x) {
	if (is.numeric(x)) {
		x
	} else if (inherits(x, "crs")) {
		# from sf
		if (is.na(x)) 
			NA_integer_
		else if (grepl("^EPSG:", x[["input"]])) 
			as.integer(gsub("^EPSG:(\\d+)\\b.*$", "\\1", x[["input"]]))
		else crs_parameters(x, with_units = FALSE)[["epsg"]]
	} else if (inherits(x, "leaflet_crs")) {
		if (grepl("EPSG", x$crsClass, fixed = TRUE)) {
			substr(x$crsClass, 11, 14) # one of L.CRS.EPSG3857, L.CRS.EPSG4326, L.CRS.EPSG3395
		} else {
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

