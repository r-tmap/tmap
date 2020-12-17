tmapAes = function(name, value, setup) {
	structure(name = name, value = value, setup = setup, class = "tmapAes")
}

MV = function(...) {
	structure(list(c(...)), class = "tmapVars")
}
