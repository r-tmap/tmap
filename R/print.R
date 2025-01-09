#' Draw thematic map
#'
#' @param x tmap object.
#' @param return.asp should the aspect ratio be returned?
#' @param show show the map
#' @param vp viewport (for `"plot"` mode)
#' @param knit A logical, should knit?
#' @param in.shiny A logical, is the map drawn in **shiny**?
#' @param proxy A logical, if `in.shiny`, is [tmapProxy()] used?
#' @param options A vector of options
#' @param ... passed on internally (for developers: in `"view"` mode, the proxy leaflet object is passed to `tmapLeafletInit`).
#' @export
print.tmap = function(x, return.asp = FALSE, show = TRUE, vp = NULL, knit = FALSE, options = NULL, in.shiny = FALSE, proxy = FALSE, ...) {
	args = list(...)

	.TMAP$in.shiny = in.shiny
	.TMAP$proxy = proxy
	.TMAP$set_s2 = NA

	# view mode will use panes, in principle one for each layer. They start at 400, unless shiny proxy is used

	dev = getOption("tmap.devel.mode")
	if (dev) timing_init()
	x2 = step1_rearrange(x)
	if (dev) timing_add("step 1")
	x3 = step2_data(x2)
	if (dev) timing_add("step 2")
	x4 = step3_trans(x3)
	if (dev) timing_add("step 3")
	res = step4_plot(x4, vp = vp, return.asp = return.asp, show = show, in.shiny = in.shiny, knit = knit, args)
	if (dev) timing_add("step 4")
	if (dev) timing_eval()

	v3_reset_flag()
	if (!is.na(.TMAP$set_s2)) suppressMessages(sf::sf_use_s2(.TMAP$set_s2))

	#if (return.asp) return(asp) else invisible(NULL)
	if (knit && tmap_graphics_name() != "Grid") {
		kp = get("knit_print", asNamespace("knitr"))
		return(do.call(kp, c(list(x=res), args, list(options=options))))
	} else {
		invisible(res)
	}
}

#' @rdname print.tmap
#' @exportS3Method knitr::knit_print
knit_print.tmap = function(x, ..., options=NULL) {
	print.tmap(x, knit=TRUE, options=options, ...)
}


timing_init = function() {
	ts = data.table(s1 = "---------", s2 = "---------", s3 = "---------", s4 = "---------", t = Sys.time())
	assign("timings", ts, envir = .TMAP)
}

timing_add = function(s1 = "", s2 = "", s3 = "", s4 = "") {
	tsx = data.table(s1 = s1, s2 = s2, s3 = s3, s4 = s4, t = Sys.time())
	ts = data.table::rbindlist(list(get("timings", envir = .TMAP), tsx))
	assign("timings", ts, envir = .TMAP)
}

timing_eval = function() {
	ts = get("timings", envir = .TMAP)

	ts[, total := round(as.numeric(difftime(ts$t, ts$t[1], units = "secs")), 3)]

	i1 = ts$s1 != ""
	i2 = ts$s2 != "" | i1
	i3 = ts$s3 != "" | i2
	i4 = ts$s4 != "" | i3

	ts[, ':='(t1=0,t2=0,t3=0,t4=0)]
	ts[i1, t1:= c(0, ts$total[i1][-1] - head(ts$total[i1], -1))]
	ts[i2, t2:= c(0, ts$total[i2][-1] - head(ts$total[i2], -1))]
	ts[i3, t3:= c(0, ts$total[i3][-1] - head(ts$total[i3], -1))]
	ts[i4, t4:= c(0, ts$total[i4][-1] - head(ts$total[i4], -1))]

	ts[s2 == "", t2 := 0]
	ts[s3 == "", t3 := 0]
	ts[s4 == "", t4 := 0]

	form = function(l, x) {
		zero = (x==0)
		y = sprintf("%.3f", x)
		z = paste0(l, " (", y, ")")
		z[zero] = ""
		z
	}

	ts[, ':='(s1=form(s1, t1),
			  s2=form(s2, t2),
			  s3=form(s3, t3),
			  s4=form(s4, t4))]

	print(ts[, c("s1", "s2", "s3", "s4", "total")])
}
