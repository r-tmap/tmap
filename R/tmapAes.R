tmapAes = function(fun, value, setup, free) {
	structure(list(fun = fun, value = tmapVars(value), setup = setup, free = free), class = "tmapAes")
}

MV = function(...) {
	list(c(...))
}

tmapVars = function(x) {
	isL = is.list(x)
	if (!isL) x = as.list(x)
	structure(x, class = "tmapVars")
}
