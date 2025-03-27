\dontrun{
tm_shape(metro) +
	tm_symbols(size = tm_vars(4:12), size.free = FALSE, size.legend = tm_legend("Population")) +
	tm_animate_slow()
}
