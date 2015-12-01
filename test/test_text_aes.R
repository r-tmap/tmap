data(Europe)

tm_shape(Europe) +
	tm_fill() +
	tm_text("iso_a3", size="pop_est")