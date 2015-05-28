data(World)

tm_shape(World) + 
    tm_fill("pop_est_dens", style="kmeans", title="Population density") + 
tm_layout_World("World Population", bg.color="lightblue")

# A custom layout wrapper for Africa
tm_layout_Africa <- function(title=NA,
							 inner.margins = c(.02, .25, .02, .02),
							 draw.frame = FALSE,
							 title.position=c("left", "bottom"), 
							 legend.position = c("left", "bottom"),
							 legend.width = 0.5,
							 bg.color = "lightskyblue2", ...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_layout", args)
}

Africa <- World[World$continent=="Africa", ]

qtm(Africa, fill="pop_est_dens", fill.style="kmeans", fill.title="Population density") + 
    tm_layout_Africa("Africa")
