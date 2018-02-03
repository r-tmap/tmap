data(World)
w1 <- qtm(World, projection = "eck4", title="Eckert IV")
w2 <- qtm(World, projection = "merc", title="Mercator")
w3 <- qtm(World, projection = "gall", title="Gall stereographic")
w4 <- qtm(World, projection = "robin", title="Robinsin")

current.mode <- tmap_mode("plot")
tmap_arrange(w1, w2, w3, w4)
tmap_mode(current.mode)
