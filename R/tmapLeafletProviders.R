tmapLeafletProviders = function(credits) {
	if (credits) {
		x = leaflet::providers.details
		lapply(x, function(xi) {
			xi$options$attribution
		})
	} else {
		leaflet::providers
	}
}
