trans_log = list(
	fun = function(x, base = exp(1)) log(x, base),
	rev = function(x, base = exp(1)) base^x,
	domain = c(1e-100, Inf)
)

# trans_logistic = list(
# 	fun = function(x) {
# 		1/ (1 + exp(-x))
# 	},
# 	rev = function(x) {
# 		log(x/(1 - x))
# 	},
# 	domain = c(-Inf, Inf)
# )


trans_sqrt = list(
	fun = sqrt,
	rev = function(x) x^2,
	domain = c(0, Inf)
)


trans_log1p = list(
	fun = log1p,
	rev = expm1,
	domain = c(0, Inf)
)

trans_pseudo_log = list(
	fun = function(x, base = exp(1), sigma = 1) asinh(x/(2 * sigma))/log(base), 
	rev = function(x, base = exp(1), sigma = 1) 2 * sigma * sinh(x * log(base)), 
	#d_transform = function(x) 1/(sqrt(4 + x^2/pseudo_log_sigma^2) * pseudo_log_sigma * log(pseudo_log_base)) 
	#d_inverse = function(x) 2 * pseudo_log_sigma * cosh(x * log(pseudo_log_base)) * log(pseudo_log_base))
	domain = c(-Inf, Inf)
)

trans_identity = list(
	fun = function(x) x,
	rev = function(x) x,
	domain = c(-Inf, Inf)
)
