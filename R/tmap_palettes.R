getPal = function(name, n = NA, rep = TRUE, range = NA) {
	cols4all::c4a(name, n = n, n_too_large = {if (rep) "repeat" else "interpolate"}, range = range)
}

getPalMeta = function(name) {
	cols4all::c4a_meta(name, no.match = "null")
}
