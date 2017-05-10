# save current options
current_options <- tmap_options()

# show current options
tmap_options()

# switch to other view
ttm()

# show current options
tmap_options()

# set style to cobalt
tmap_options(tmap.style = "cobalt")

# show current options
tmap_options()

# set style usign tmap_style
tmap_style("classic")

# show current options
tmap_options()

# set unit to imperial
tmap_options(tmap.unit = "imperial", tmap.limits = c(facets.view = 8, facets.plot = 16))

# show current options
tmap_options()

# restore options
tmap_options(current_options)
