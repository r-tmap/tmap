tmapAes = function(value, setup, legend, free) {
	structure(list(value = tmapVars(value), setup = setup, legend = legend, free = free), class = "tmapAes")
}

MV = function(...) {
	list(c(...))
}

tmapVars = function(x) {
	isL = is.list(x)
	if (!isL) x = as.list(x)
	structure(x, class = "tmapVars")
}
