tmapGpar = function(fill = NULL,
					col = NULL,
					shape = NULL,
					size = NULL,
					fill_alpha = NULL,
					col_alpha = NULL,
					#pattern = NULL,
					lty = NULL,
					lwd = NULL,
					linejoin = NULL,
					lineend = NULL,
					...) {
	args = c(as.list(environment()), list(...))
	structure(args, class = "tmapGpar")
}

tmapTpar = function(...) {
	args = c(as.list(environment()), list(...))
	structure(args, class = "tmapTpar")
}
