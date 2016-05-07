\dontrun{
data(World)
World_dots <- sample_dots(World, vars="pop_est_dens", nrow=200, ncol=400, w=1e6)

tm_shape(World_dots) + tm_dots(size = .02, jitter=.1) + 
	tm_layout("One dot represents one million people", title.position = c("right", "bottom"))
}
