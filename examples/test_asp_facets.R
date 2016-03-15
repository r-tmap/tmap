tm_shape(NLD_prov) +
	tm_fill("gold2") + tm_borders() +
	tm_facets(by="name", free.coords = TRUE, drop.units=TRUE) +
	tm_layout(outer.margins = 0, asp=0)

tm_shape(NLD_prov) +
	tm_fill("gold2") + tm_borders() +
	tm_layout(outer.margins = 0, asp=0)



tm_shape(NLD_prov) +
	tm_fill("gold2") + tm_borders() +
	tm_facets(by="name", free.coords = TRUE, drop.units=TRUE) + tm_layout(design.mode=T, inner.margins=0, outer.margins = 0, between.margin=0, asp=0)



tm_shape(NLD_prov) +
	tm_fill(c("gold2", "red", "green2", "blue")) + tm_borders() +
	tm_layout(design.mode=F, inner.margins=0, asp=0, panel.show = T)

tm_shape(NLD_prov) +
	tm_fill(c("gold2")) + tm_borders() +
	tm_layout(design.mode=F, inner.margins=0, asp=0, panel.show = T)


tm_shape(NLD_prov) +
	tm_fill(c("gold2", "green")) + tm_borders() +
	tm_layout(design.mode=T, inner.margins=0, between.margin=0, asp=0, panel.show = T)

tm_shape(NLD_prov) +
	tm_fill(c("gold2", "green")) + tm_borders() +
	tm_layout(design.mode=T, inner.margins=0, between.margin=0, outer.margins=0, asp=0, panel.show = F)