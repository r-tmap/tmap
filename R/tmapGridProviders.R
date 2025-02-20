tmapGridProviders = function(credits) {
	x = maptiles::get_providers()
	if (credits) {
		lapply(x, "[[", "cit")
	} else {
		lapply(x, "[[", "src")
	}
}
