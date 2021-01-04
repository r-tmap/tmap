tmapAes = function(fun, value, setup) {
	structure(list(fun = fun, value = value, setup = setup), class = "tmapAes")
}

MV = function(...) {
	structure(list(c(...)), class = "tmapVars")
}
