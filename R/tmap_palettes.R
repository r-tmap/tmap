getPal = function(name, n = NA, rep = TRUE, range = NA) {
	if (name %in% c("cat", "seq", "div")) {
		name = cols4all::c4a_options("defaults")$defaults[[name]]
	}
	cols4all::c4a(name, n = n, nm_invalid = {if (rep) "repeat" else "interpolate"}, range = range)
}

getPalBiv = function(name, m = NA, n = NA, rep = TRUE) {
	cols4all::c4a(name, m = m, n = n, nm_invalid = {if (rep) "repeat" else "interpolate"})
}

getPalMeta = function(name) {
	if (name %in% c("cat", "seq", "div")) {
		name = cols4all::c4a_options("defaults")$defaults[[name]]
	}
	cols4all::c4a_info(name, no.match = "null", verbose = FALSE)
}
