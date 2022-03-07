trans_log10 = list(
	fun = log10,
	rev = function(x) 10^x,
	domain = c(1e-100, Inf)
)

trans_identity = list(
	fun = function(x) x,
	rev = function(x) x,
	domain = c(-Inf, Inf)
)
