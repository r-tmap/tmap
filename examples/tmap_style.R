current.style <- tmap_style("classic")

data(World)
qtm(World, fill="life_exp")

tmap_style(current.style)
