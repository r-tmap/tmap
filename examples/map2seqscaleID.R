breaksList <- list(c(0, 10, 20, 30, 40, 50),
				   c(10, 20, 30, 40, 50),
				   c(-50, -40, -30, -20, -10, 0),
				   c(-50, -40, -30, -20, -10))
lapply(breaksList, FUN=map2divscale)
