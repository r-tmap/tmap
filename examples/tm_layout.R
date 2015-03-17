data(World)

# General layout settings with adjusted title
tm_shape(World) + tm_fill("pop_est_dens", style="kmeans") + 
	tm_layout("World Population")

# Layout settings for the world map
tm_shape(World) + tm_fill("pop_est_dens", style="kmeans") + 
	tm_layout_World("World Population")

# Take a look at Africa
Africa <- World[World$continent=="Africa", ]
qtm(Africa, fill="pop_est_dens", style="kmeans")
# So with general settings, legend is drawn insde western Africa.

# A custom layout wrapper for Africa
tm_layout_Africa <- function(title=NA,
							 inner.margins = c(.02, .4, .02, .02),
							 draw.frame = FALSE,
							 title.position=c("left", "bottom"), 
							 legend.position = c("left", "bottom"), 
							 bg.color = "lightcyan3", ...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_layout", args)
}

qtm(Africa, fill="pop_est_dens", fill.style="kmeans") + 
	tm_layout_Africa("Population of Africa")
