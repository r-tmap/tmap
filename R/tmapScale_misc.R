tmapScale_returnNA = function(n, legend, value.na, label.na, na.show) {
	if (identical(na.show, FALSE)) {
		legend = list(title = NA, 
					  nitems = 0,
					  labels = NA, 
					  dvalues = NA, 
					  vvalues = NA,
					  vneutral = value.na,
					  na.show = NA,
					  setup = list(show = FALSE))
	} else {
		legend = list(title = legend$title, 
					  nitems = 1,
					  labels = label.na, 
					  dvalues = NA, 
					  vvalues = value.na,
					  vneutral = value.na,
					  na.show = TRUE,
					  setup = legend)
	}
	return(format_aes_results(rep(value.na, n), legend))
}
