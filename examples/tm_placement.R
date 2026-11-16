Africa = World[World$continent == "Africa", ]

# default: labels are wrapped, rotated, or shrunk to fit inside the countries;
# labels that still do not fit are moved outside with leader lines
tm_shape(Africa, crs = "+proj=robin") +
	tm_polygons("HPI", fill.legend = tm_legend_hide()) +
	tm_labels("name", size = 0.6)

# never move or shrink labels, and keep the labels that do not fit
tm_shape(Africa, crs = "+proj=robin") +
	tm_polygons("HPI", fill.legend = tm_legend_hide()) +
	tm_labels("name", size = 0.6,
		options = opt_tm_labels(placement = tm_placement(fallback = c("wrap", "rotate", "overlap"))))

# moved labels in a column, with elbow-shaped gray leader lines
tm_shape(Africa, crs = "+proj=robin") +
	tm_polygons("HPI", fill.legend = tm_legend_hide()) +
	tm_labels("name", size = 0.6,
		options = opt_tm_labels(placement = "callout",
			leader = tm_leader(col = "gray40", type = "elbow"))) +
	tm_layout(inner.margins = c(0.02, 0.1, 0.02, 0.1))
