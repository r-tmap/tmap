## load shapes
data(NLD_muni)
data(World)

## get bounding box (similar to sp's function bbox)
bb(NLD_muni)

## extent it by factor 1.10
bb(NLD_muni, ext=1.10)

## convert to longlat
bb(NLD_muni, projection="longlat")

## change existing bounding box
bb(NLD_muni, ext=1.5)
bb(NLD_muni, width=2, relative = TRUE)
bb(NLD_muni, xlim=c(.25, .75), ylim=c(.25, .75), relative = TRUE)
bb("Limburg", projection = "rd")

bb_italy <- bb("Italy", projection = "eck4")

tm_shape(World, bbox=bb_italy) + tm_polygons()

# NOTE: this does not work in view mode, since the bounding box should be in coordinates
#       of the plotted projection, which are longitude/latitude coordinates (see below)
# NOTE2: this shortcut is saver to use: tm_shape(World, bbox="Italy") + tm_polygons()

\dontrun{
## create new bounding box
bb_ZL <- bb(cx=190000, cy=330000, width=50000, height=50000)

current.mode <- tmap_mode("plot")
tm_shape(NLD_muni, bbox=bb_ZL) + tm_polygons()

tmap_mode("view")
bb_ZL_longlat <- bb(bb_ZL, current.projection="rd", projection="longlat")
tm_shape(NLD_muni, bbox=bb_ZL_longlat) + tm_polygons()

# restore current mode
tmap_mode(current.mode)
}
