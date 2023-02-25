library(leaflet)
library(leaflegend)
library(cols4all)
data("quakes")
quakes <- quakes[1:100,]
numPal <- colorNumeric('viridis', quakes$depth)
sizes <- sizeNumeric(quakes$depth, baseSize = 10)
symbols <- Map(
	makeSymbol,
	shape = 'triangle',
	color = numPal(quakes$depth),
	width = sizes,
	height = sizes
)

## size and fill+col
leaflet() %>%
	addTiles() %>%
	addLegendSize(
		values = seq(100, 600, by = 100),
		pal = colorNumeric(palette = cols4all::c4a(.P$met$cat$cross, 6), 
						   domain = seq(100, 600, by = 100), 
						   na.color="red", 
						   alpha = FALSE),
		title = 'Depth',
		shape = c('circle'))


### size and fill
leaflet() %>%
	addTiles() %>%
	addLegendSize(
		values = seq(100, 600, by = 100),
		pal =  colorFactor(palette = cols4all::c4a(.P$met$cat$cross, 6), 
							domain = seq(100, 600, by = 100), 
							na.color="red", 
							alpha = FALSE),
		color = "#0099FF",
		strokeWidth = 6,
		shape = c('circle'))


### size and col
leaflet() %>%
	addTiles() %>%
	addLegendSize(
		values = seq(100, 600, by = 100),
		pal =  colorNumeric(palette = cols4all::c4a(.P$met$cat$cross, 6), 
							domain = seq(100, 600, by = 100), 
							na.color="red", 
							alpha = FALSE),
		fillColor = "#CCCCCC",
		strokeWidth = 6,
		title = 'Depth',
		labelStyle = 'margin: auto;',
		shape = c('circle'),
		orientation = c('vertical', 'horizontal'),
		opacity = .7,
		breaks = 5)

### size and shape
leaflet() %>%
	addTiles() %>%
	addLegendSize(
		values = seq(100, 600, by = 100),
		pal =  colorFactor(palette = cols4all::c4a(.P$met$cat$cross, 6), 
						   domain = seq(100, 600, by = 100), 
						   na.color="red", 
						   alpha = FALSE),
		color = "#0099FF",
		strokeWidth = 6,
		shape = c('circle'))

### fill+col (not separately)
# leaflet() %>%
# 	addTiles() %>%
# 	addLegendFactor(
# 		values = letters[1:6],
# 		pal =  colorFactor(cols4all::c4a(.P$met$cat$cross, 6), levels = letters[1:6]),
# 		title = 'Depth',
# 		labelStyle = 'margin: auto;',
# 		shape = c('circle'),
# 		orientation = c('vertical', 'horizontal'),
# 		opacity = .7)

# col+fill only
leaflet() %>%
	addTiles() %>%
	addLegendSymbol(pal = colorFactor(cols4all::c4a(.P$met$cat$cross, 6), levels = letters[1:6]),
					#color = "blue",
					strokeWidth = 3,
					#fillColor = c("red"),
					values = letters[1:6],
					shape = c('circle', 'circle', 'circle', 'circle', 'circle', 'circle', 'circle'))


# col only
leaflet() %>%
	addTiles() %>%
	addLegendSymbol(pal = colorFactor(cols4all::c4a(.P$met$cat$cross, 6), levels = letters[1:6]),
		#color = "blue",
		strokeWidth = 3,
		fillColor = c("red"),
		values = letters[1:6],
		shape = c('circle', 'circle', 'circle', 'circle', 'circle', 'circle', 'circle'))

# fill only
leaflet() %>%
	addTiles() %>%
	addLegendSymbol(pal = colorFactor(cols4all::c4a(.P$met$cat$cross, 6), levels = letters[1:6]),
					color = "blue",
					strokeWidth = 3,
					#fillColor = c("red"),
					values = letters[1:6],
					shape = c('circle', 'circle', 'circle', 'circle', 'circle', 'circle', 'circle'))


# symbols
leaflet() %>%
	addTiles() %>%
	addLegendSymbol(#pal = colorFactor(cols4all::c4a(.P$met$cat$cross, 6), levels = letters[1:6]),
		color = "blue",
		strokeWidth = 3,
		fillColor = c("red"),
		values = letters[1:6],
		shape = c('circle', 'rect', 'triangle', 'cross', 'plus', 'polygon'))


# symbols and col
leaflet() %>%
	addTiles() %>%
	addLegendSymbol(pal = colorFactor(cols4all::c4a(.P$met$cat$cross, 6), levels = letters[1:6]),
					values = letters[1:6],
					fillColor = "#BBBBBB",
					strokeWidth = 6,
					title = 'Depth',
					labelStyle = 'margin: auto;',
					shape = c('circle', 'rect', 'triangle', 'cross', 'plus', 'polygon'),
					orientation = c('vertical', 'horizontal'),
					opacity = .7)



# symbols and fill
leaflet() %>%
	addTiles() %>%
	addLegendSymbol(pal = colorFactor(cols4all::c4a(.P$met$cat$cross, 6), levels = letters[1:6]),
					values = letters[1:6],
					color = "#BBBBBB",
					strokeWidth = 6,
					title = 'Depth',
					labelStyle = 'margin: auto;',
					shape = c('circle', 'rect', 'triangle', 'cross', 'plus', 'polygon'),
					orientation = c('vertical', 'horizontal'),
					opacity = .7)

## line width
leaflet() %>%
	addTiles() %>%
	addLegendLine(
		values = seq(100, 600, by = 100),
		pal = colorNumeric(palette = cols4all::c4a(.P$met$cat$cross, 6), 
						   domain = seq(100, 600, by = 100), 
						   na.color="red", 
						   alpha = FALSE)
	)





##### testing

tm_shape(World) +
	tm_symbols(size = "pop_est")

# to do: fix empty icons
tm_shape(World) +
	tm_symbols(size = "pop_est", col = "purple", lwd = 2, size.scale = tm_scale_continuous(values.scale = 2))


tm_shape(World) +
	tm_symbols(size = "pop_est", fill = "economy", lwd = 2, size.scale = tm_scale_intervals(values.scale = 2, n = 7, style = "kmeans"), fill.legend = tm_legend_combine("size"))



# to do: use all gradient colors (not just middle colors)
tm_shape(World) +
	tm_polygons(fill = "HPI", fill.scale = tm_scale_continuous())



##


library(leaflet)


symbols <- Map(
	makeSymbol,
	shape = c('triangle', 'plus'),
	color = c("red", "blue"),
	fillColor = c("green", "purple"),
	width = c(3,5),
	height = c(3,5),
	opacity = 1,
	'stroke-width' = 0.1
)


leaflet() %>%
	addTiles() %>%
	addLegendImage(
		images = symbols,
		labels = letters[1:2],
		width = c(40, 80),
		height = c(40, 80),
		title = htmltools::tags$div(
			'Leaf',
			style = 'font-size: 24px; text-align: center; margin-bottom: 5px;'),
		position = 'topright', orientation = 'vertical')


