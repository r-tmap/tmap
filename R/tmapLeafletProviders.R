tmapLeafletProviders = function(credits) {
	if (credits) {
		x = leaflet::providers.details
		c(lapply(x, function(xi) {
			xi$options$attribution
		}), lapply(extra_providers, "[[", "credits"))
	} else {
		c(leaflet::providers, as.list(stats::setNames(names(extra_providers), names(extra_providers))))
	}
}
