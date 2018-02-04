metro$cat <- factor(rep(1:3, length.out=length(metro)), labels=letters[1:3])

tm_shape(metro) + 
	tm_dots(col = "pop1950", size=.1) + tm_facets(by="cat", free.scales = TRUE)
