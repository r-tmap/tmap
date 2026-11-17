tmapGridProviders = function(credits) {
	x = maptiles::get_providers()
	if (credits) {
		c(lapply(x, "[[", "cit"), lapply(extra_providers, "[[", "credits"))
	} else {
		c(lapply(x, "[[", "src"), as.list(stats::setNames(names(extra_providers), names(extra_providers))))
	}
}
