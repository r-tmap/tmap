#' Draw thematic map
#' 
#' Draw thematic map.
#' 
#' @param x tmap object. 
#' @param ... not used
#' @export
#' @method print tmap
print.tmap = function(x, ...) {
	dev = getOption("tmap.devel.mode")
	if (dev) timing_init()
	x2 = step1_rearrange(x)
	if (dev) timing_add("step 1")
	x3 = step2_data(x2)
	if (dev) timing_add("step 2")
	x4 = step3_trans(x3)
	if (dev) timing_add("step 3")
	step4_plot(x4)
	if (dev) timing_add("step 4")
	if (dev) timing_eval()
}


timing_init = function() {
	ts = data.table(s1 = "---------", s2 = "---------", s3 = "---------", s4 = "---------", t = Sys.time())
	assign("timings", ts, envir = .TMAP)
}

timing_add  = function(s1 = "", s2 = "", s3 = "", s4 = "") {
	tsx = data.table(s1 = s1, s2 = s2, s3 = s3, s4 = s4, t = Sys.time())
	ts = data.table::rbindlist(list(get("timings", envir = .TMAP), tsx))
	assign("timings", ts, envir = .TMAP)
}

timing_eval = function() {
	ts = get("timings", envir = .TMAP)
	
	secs = round(as.numeric(difftime(ts$t, ts$t[1], units = "secs")), 3)
	secs2 = c(0, secs[-1] - head(secs, -1))
	
	ts[, ':='(time = secs2, total = secs, t = NULL)]
	
	print(ts)
}
