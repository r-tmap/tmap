data(World)
w1 <- qtm(World, projection = "+proj=eck4", title="Eckert IV")
w2 <- qtm(World, projection = 3857, title="Mercator")
w3 <- qtm(World, projection = "+proj=gall", title="Gall stereographic")
w4 <- qtm(World, projection = "+proj=robin", title="Robinsin")

current.mode <- tmap_mode("plot")
tmap_arrange(w1, w2, w3, w4, widths = c(.25, .75))
tmap_mode(current.mode)
