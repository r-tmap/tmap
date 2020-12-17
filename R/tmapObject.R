#' @param tmel tm_element_list
tmapObject = function(tmel) {
	# find shapes and layers
	is_tms = sapply(tmel, inherits, "tm_shape")
	is_tml = sapply(tmel, inherits, "tm_layer")
	ids = cumsum(is_tms)
	ids[!is_tml & !is_tms] = 0
	
	# create groups, for each group: tms (tmap shape) and tmls (tmap layers)
	tmel_spl = split(tmel, f = ids)
	tmo = lapply(tmel_spl, function(tmg) {
		is_tms = sapply(tmg, inherits, "tm_shape")
		is_tml = sapply(tmg, inherits, "tm_layer")
		structure(list(tms = tmg[[1]], tmls = tmg[is_tml]), class = "tmapGroup")
	})
	
	# get the crs of the main shape
	crs = get_main_crs(tmo)
	
	# reproject other shapes if needed
	tmo = structure(lapply(tmo, function(tmg) {
		tmg$tms$crs = crs
		tmg$tms = do.call(tmapShape, tmg$tms)
		
		dtcols = tmg$tms$dtcols
		
		tmapLayer()
		
		tml = tmg$tmls[[1]]
		
		within(tml, {
			aes.mapping = if (!length(aes.mapping)) {
				list()
			} else {
				lapply(aes.mapping, function(a) {
					name = get(a)
					# dim: 0 if constant is used for an aesthetic, 1 if a data variable is mapped, and k if k data variables are mapped to one aesthetic
					dim = if (name %in% dtcols) length(name) else 0L
					setup = get(paste(a, "setup", sep = "."))
				})
			}
			aes.trans = if (!length(aes.trans)) {
				list()
			} else {
				lapply(aes.trans, function(a) {
					name = get(a)
					# dim: 0 if constant is used for an aesthetic, 1 if a data variable is mapped, and k if k data variables are mapped to one aesthetic
					dim = if (name %in% dtcols) length(name) else 0L
					setup = get(paste(a, "setup", sep = "."))
				})
			}
			rm(list = ls()[substr(ls(), 1, 4) != "aes."])
		})
		
		
		
		
		aes.mapping
		
	}), class = "tmapObject")
	
	# create tmapLayer objects
}


# helper function to find the main crs: 
# - it checks which tm_shape is "main" (the first that is set to is.main, and if none, the first)
# - for the main tm_shape, it gets the crs attribute, and if not specified, it extracts the crs from the shape object
get_main_crs = function(tmo) {
	is_main = vapply(tmo, function(tmg) {
		identical(tmg$tms$is.main, TRUE)
	}, FUN.VALUE = logical(1))
	
	main_id = if (any(is_main)) which(is_main)[1L] else 1L
	
	tms_main = tmo[[main_id]]$tms
	
	crs_main = tms_main$crs
	if (is.null(crs_main)) crs_main = sf::st_crs(tms_main$shp)
	crs_main
}
