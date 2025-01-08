# function to update the levels fl per facet dimension
update_fl = function(k, lev = NULL, m = NULL) {
	fl = get("fl", envir = .TMAP)
	# print("test")
	# print(fl)
	fk = fl[[k]]
	n =  if (is.character(fk)) length(fk) else fk

	if (!missing(lev)) {
		m = length(lev)
		if (m == 1L && n == 1L) {
			fk = lev
		} else if (m > 1L && n > 1L && m != n) {
			stop("Inconsistent number of facets in the ", k, " dimension.", call. = FALSE)
		} else if (m > n) {
			fk = lev
		}
	} else if (!missing(m)) {
		if (m > 1L && n > 1L && m != n) {
			stop("Inconsistent number of facets in the ", k, " dimension.", call. = FALSE)
		} else if (m > n) {
			fk = m
		}
	}
	fl[[k]] = fk
	assign("fl", fl, envir = .TMAP)
}



get_tmf = function(tmfs) {
	# Get tmf object: start with the one that is called, and add 'calls'

	nf = length(tmfs)

	gns = vapply(tmfs, "[[", FUN.VALUE = integer(3), "gn", USE.NAMES = FALSE)

	grpid = apply(gns, which.max, MARGIN = 1)

	gn = apply(gns, max, MARGIN = 1)

	for (i in seq_len(3L)) if (any(gns[i, ] > 1L & gns[i, ] != gn[i])) stop("Number of facets inconsistent over groups", call. = FALSE)

	gls = lapply(tmfs, "[[", "gl")

	gl = lapply(seq_len(3L), function(i) gls[[grpid[i]]][[i]])


	#gnl = lapply(tmfs, "[[", FUN.VALUE = integer(3), "gn", USE.NAMES = FALSE)

	# find first tmf that has been called
	fid = which(vapply(tmfs, function(tmf){
		"calls" %in% names(tmf)
	}, FUN.VALUE = logical(1)))[1]

	if (is.na(fid)) fid = 1L

	tmf = tmfs[[fid]]

	if (fid < nf) {
		for (i in (fid+1):nf) {
			args = tmfs[[i]]$calls
			tmf[args] = tmfs[[i]][args]
		}
	}
	tmf$fl = gl
	tmf$fn = gn
	tmf$n = prod(gn)

	if (tmf$type == "wrapstack") {
		tmf$type = if ((!is.na(tmf$ncols) && tmf$ncols %in% c(tmf$n, 1)) ||
			(!is.na(tmf$nrows) && tmf$nrows %in% c(tmf$n, 1))) {
			"stack"
		} else if (!is.na(tmf$ncols) || !is.na(tmf$nrows)) {
			"wrap"
		} else if (tmf$n > 3) "wrap" else "stack"
	}

	if (!is.na(tmf$ncols) && !is.na(tmf$nrows)) {
		if ((tmf$ncols * tmf$nrows) < tmf$n) tmf$type = "page"
	}

	tmf
}

cbind_dts = function(dts, plot.order) {
	if (!length(dts)) return(list())

	bypass_ord = plot.order$aes == "DATA"

	max_values <- tryCatch(
		vapply(dts, FUN =  ncol, FUN.VALUE = integer(1)),
		error = function(e) {
			warning("The data.table with the most group-by columns (others are joined). This is an internal error.")
			NULL
		})
	if (is.null(max_values)) {
		return(NULL)
	}
	id = which.max(max_values) # data.table with the most group-by columns (others are joined)

	dt = dts[[id]]

	dev = getOption("tmap.devel.mode")

	if (length(dts) > 1L) {
		for (i in setdiff(seq_along(dts), id)) {
			dti = dts[[i]]

			id_cols = ncol(dti) - 1L - as.integer(!bypass_ord) #minus one aes and one ord
			id_nams = names(dti)[seq.int(id_cols)]

			#dt = dt[dti, on = names(dti)[1L:(ncol(dti)-2L)]]
			dt = dt[dti, on = id_nams]
		}
	}



	ord_cols = which(subStr(names(dt), -5) == "__ord")
	#m = as.matrix(dt[, ord_cols, with = FALSE])


	#dt[, ord_cols, with = FALSE][, ]

	if (!bypass_ord) {
		dt_rep = function(old, new) {
			dt[, (ord_cols) := replace(.SD, .SD == old, new), .SDcols = ord_cols]
		}


		if (plot.order$na.order == "mix") dt_rep(0L, 1L)
		if (plot.order$null.order == "mix") dt_rep(-1L, 1L)

		if (!plot.order$null.below.na) {
			if (plot.order$na.order == "top") dt_rep(0L, 2147483646L) else dt_rep(0L, -2L)
			if (plot.order$null.order == "top") dt_rep(-1L, 2147483647L)
		} else {
			if (plot.order$na.order == "top") dt_rep(0L, 2147483647L)
			if (plot.order$null.order == "top") dt_rep(-1L, 2147483646L)
		}

		dt[, ord__ := do.call(pmin, .SD), .SDcols = ord_cols]
		dt[ord__ > 0L, ord__ := do.call(pmax, .SD), .SDcols = ord_cols]
		dt[, (ord_cols) := NULL]
	}


	dt
}


subStr = function(x, k) {
	if (k > 0L) {
		substr(x, 1L, k)
	} else {
		n = nchar(x)
		substr(x, n + k + 1L, n)
	}
}



limit_nx = function(nx) {
	# bound number of facets
	tmapOptions <- tmap::tmap_options_mode(mode.specific = FALSE)
	mode = getOption("tmap.mode")

	ox = tmapOptions$facet.max

	if (ox < nx) warning("Number of facets for mode \"", mode, "\" is limited to ", ox, " Change the option facet.max (with tmap_options) to allow more facets.", call. = FALSE)
	min(ox, nx)
}


