check_fix = function(sfc, shp_name, reproj, messages) {
	s2 = sf::sf_use_s2()

	isLarge = (inherits(sfc, c("sfc_MULTIPOLYGON", "sfc_MULTILINESTRING", "sfc_GEOMETRYCOLLECTION")) && length(sfc) >= 5000)
	if (isLarge && messages) message("Checking shape object \"", shp_name, " \". It is large, so it may take a while")

	if (!all(st_is_valid(sfc))) {
		checkAgain = FALSE
		tryCatch({
			if (messages) message("The shape object \"", shp_name, "\" is invalid", ifelse(reproj, " (after reprojection). ", ". "), "Trying to fix it...")
			sfc = sf::st_make_valid(sfc)
		}, error = function(e) {
			if (messages) message("Unsuccesful attempt with sf::st_make_valid")
			checkAgain = TRUE
		})
		if (checkAgain || !all(st_is_valid(sfc))) {
			suppressMessages(sf::sf_use_s2(!s2))
			.TMAP$set_s2 = s2
			tryCatch({
				sfc = sf::st_make_valid(sfc)
			}, error = function(e) {
				suppressMessages(sf::sf_use_s2(s2))
				stop("Unable to make ", shp_name, " valid", add, call. = FALSE)
			})
			if (messages) message("Shape ", shp_name, " has been fixed with s2 = ", !s2, ". If the map doesn't look correct, please run sf::sf_use_s2(", !s2, ") before running the tmap code again.")

		}
	}
	sfc
}
