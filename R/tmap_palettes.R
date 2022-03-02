getPal = function(name, n = NA, rep = TRUE, contrast = NA) {
	cols4all::c4a(name, n = n, n_too_large = {if (rep) "repeat" else "interpolate"}, contrast = contrast)
}

getPalMeta = function(name) {
	cols4all::c4a_meta(name, no.match = "null")
}

get_default_contrast <- function(type, m) {
	if (type=="seq") {
		default_contrast_seq(m)
	} else {
		default_contrast_div(m)
	}
}
