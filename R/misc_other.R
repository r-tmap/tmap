islistof = function(x, class) {
	is.list(x) && all(vapply(x, inherits, logical(1), what = class))
}

select_sf = function(shpTM, dt) {
	shp = shpTM$shp
	tmapID = shpTM$tmapID
	
	tmapIDdt = dt$tmapID__
	
	tid = intersect(tmapID, tmapIDdt)
	
	shpSel = shp[match(tid, tmapID)] #st_cast(shp[match(tid, tmapID)], "MULTIPOLYGON")
	
	
	dt = dt[match(tid, tmapIDdt), ]
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
