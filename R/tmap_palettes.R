getPal = function(name, n = NA, rep = TRUE, range = NA) {
	cols4all::c4a(name, n = n, nm_invalid = {if (rep) "repeat" else "interpolate"}, range = range)
}

getPalMeta = function(name) {
	cols4all::c4a_info(name, no.match = "null", verbose = FALSE)
}
