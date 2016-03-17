data(World)

current.style <- tmap_style("classic")
qtm(World, fill="life_exp", fill.title="Life expectancy")

tmap_style("cobalt")
qtm(World, fill="life_exp", fill.title="Life expectancy")

# restore current style
tmap_style(current.style)
