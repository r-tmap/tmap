trans_log = list(
	fun = log,
	rev = exp,
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

pseudo_log_sigma = 1
pseudo_log_base = exp(1)

trans_pseudo_log = list(
	fun = function(x) asinh(x/(2 * pseudo_log_sigma))/log(pseudo_log_base), 
	rev = function(x) 2 * pseudo_log_sigma * sinh(x * log(pseudo_log_base)), 
	#d_transform = function(x) 1/(sqrt(4 + x^2/pseudo_log_sigma^2) * pseudo_log_sigma * log(pseudo_log_base)) 
	#d_inverse = function(x) 2 * pseudo_log_sigma * cosh(x * log(pseudo_log_base)) * log(pseudo_log_base))
	domain = c(-Inf, Inf)
)

trans_identity = list(
	fun = function(x) x,
	rev = function(x) x,
	domain = c(-Inf, Inf)
)
