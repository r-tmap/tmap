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

trans_log1p = list(
	fun = log1p,
	rev = expm1,
	domain = c(0, Inf)
)

trans_identity = list(
	fun = function(x) x,
	rev = function(x) x,
	domain = c(-Inf, Inf)
)
