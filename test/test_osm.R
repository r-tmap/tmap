## code taken from R paper http://journal.r-project.org/archive/2013-1/eugster-schlesinger.pdf
library(osmar)
src <- osmsource_api()
bb <- center_bbox(174.76778, -36.85056, 700, 700)
ua <- get_osm(bb, source = src)

## first try:
uas <- as_sp(ua)
tm_shape(uas[[3]]) +
	tm_fill() +
tm_shape(uas[[2]]) +
	tm_lines("type") + 
tm_shape(uas[[1]]) +
	tm_bubbles(size = .1)

## some specific selections (code from R paper)
bg_ids <- find(ua, way(tags(k == "building")))
bg_ids <- find_down(ua, way(bg_ids))
bg <- subset(ua, ids = bg_ids)
bg_poly <- as_sp(bg, "polygons")

hw_ids <- find(ua, way(tags(k == "highway")))
hw_ids <- find_down(ua, way(hw_ids))
hw <- subset(ua, ids = hw_ids)
hw_line <- as_sp(hw, "lines")

bs_ids <- find(ua, node(tags(v %agrep% "busstop")))
bs <- subset(ua, node_ids = bs_ids)
bs_points <- as_sp(bs, "points")

bus_ids <- find(ua, relation(tags(v == "bus")))
bus <- lapply(bus_ids,
			function(i) {
					raw <- get_osm(relation(i), full = TRUE)
					as_sp(raw, "lines")
			})

## bind the list of bus routes
bus <- bus[!sapply(bus, is.null)]
busses <- do.call("sbind", bus)


## plot
tm <- tm_shape(bg_poly) +
	tm_fill() +
tm_shape(hw_line) +
	tm_lines("green3") + 
tm_shape(busses) +
	tm_lines("blue") +
tm_shape(bs_points) +
	tm_bubbles(size = .1)


