tmapLayer = function(tml, dt) {
	structure(within(tml, {
		trans.dim = max(1, vapply(trans.aes, function(aes) {
			length(aes$value)	
		}, FUN.VALUE = integer(1)))
		mapping.dim = max(1, vapply(mapping.aes, function(aes) {
			length(aes$value)	
		}, FUN.VALUE = integer(1)))
	}), class = c("tmapLayer", "list"))
}
