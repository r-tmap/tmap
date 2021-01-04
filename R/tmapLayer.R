tmapLayer = function(tml, dt) {
	structure(within(tml, {
		aes.mapping = if (!length(aes.mapping)) {
			list()
		} else {
			lapply(aes.mapping, function(a) {
				value = get(a )
				# dim: 0 if constant is used for an aesthetic, 1 if a data variable is mapped, and k if k data variables are mapped to one aesthetic
				dim = if (is.null(value)) 0L else if (value %in% dtcols) length(value) else 0L
				setup = get(paste(a, "setup", sep = "."))
				structure(list(var = a, value = value, dim = dim, setup = setup), class = c("tmapAes", "list"))
			})
		}
		aes.trans = if (!length(aes.trans)) {
			list()
		} else {
			lapply(aes.trans, function(a) {
				value = get(a)
				# dim: 0 if constant is used for an aesthetic, 1 if a data variable is mapped, and k if k data variables are mapped to one aesthetic
				dim = if (is.null(value)) 0L else if (value %in% dtcols) length(value) else 0L
				setup = get(paste(a, "setup", sep = "."))
				structure(list(var = a, value = value, dim = dim, setup = setup), class = c("tmapAes", "list"))
			})
		}
		rm(list = ls()[substr(ls(), 1, 4) != "aes."])
	}), class = "tmapLayer")	
}
