current.mode <- tmap_mode("plot")

data(NLD_muni)

qtm(NLD_muni, theme = "NLD") + tm_compass()
qtm(NLD_muni, theme = "NLD") + tm_compass(type="8star")
qtm(NLD_muni, theme = "NLD") + tm_compass(type="8star")
qtm(NLD_muni, theme = "NLD") + tm_compass(type="radar", position=c("left", "top"), show.labels = 3)

# restore current mode
tmap_mode(current.mode)
