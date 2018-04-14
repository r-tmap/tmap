data(World)


tm_shape(World) +
	tm_fill("HPI", auto.palette.mapping = TRUE, palette = "RdYlBu")


tm_shape(World) +
	tm_fill("HPI", auto.palette.mapping = FALSE, palette = "RdYlBu")

tm_shape(World) +
	tm_fill("economy", auto.palette.mapping = TRUE)

World$a <- runif(length(World), min = -10, max = 40)
tm_shape(World) +
	tm_fill("a", auto.palette.mapping = FALSE)

tm_shape(World) +
	tm_fill("a", auto.palette.mapping = TRUE)


tm_shape(World) +
	tm_fill("a", auto.palette.mapping = FALSE, contrast = c(.3,.6))


tm_shape(World) +
	tm_fill("a", midpoint = NA, contrast = c(.3,.6))


data(metro)

metro$pop_growth <- ((metro$pop2020 - metro$pop2000) / metro$pop2000) * 100
tm_shape(metro) + 
	tm_dots(col = "pop_growth", size = .5)
