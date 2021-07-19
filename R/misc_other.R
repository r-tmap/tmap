islistof = function(x, class) {
	is.list(x) && all(vapply(x, inherits, logical(1), what = class))
}
