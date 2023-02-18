# available formats
tmap_format()

# create option list to be used as a new format
World_small = tmap_format("World")
World_small$scale = 2

# add format
tmap_format_add(World_small, name = "World_small")

# observe that World_small is successfully added:
tmap_format()

data(World)

#qtm(World, fill="HPI", format="World_small")
