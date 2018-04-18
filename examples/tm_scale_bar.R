current.mode <- tmap_mode("plot")

data(NLD_muni)
qtm(NLD_muni, theme = "NLD") + tm_scale_bar(position=c("left", "bottom"))

# restore current mode
tmap_mode(current.mode)
