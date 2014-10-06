l <- Line(coords = matrix(c(1,3,5,7,9, 1,1,1,1,1), ncol=2))
L <- Lines(list(l), ID = 1)
pL <- SpatialLines(list(L))
pL <- SpatialLinesDataFrame(pL, data = data.frame(ID=1))

coordinates(pL)

pL2 <- split_lines_equal(pL, dist=1.5, include.last = FALSE)
coordinates(pL2)

pL2 <- split_lines_equal(pL, dist=1.5, include.last = TRUE)
coordinates(pL2)
