determine_asp_ratios <- function(gm, interactive) {
	if (interactive) {
		dw <- 1
		dh <- 1
		tasp <- fasp <- 1
		lasp <- NA
		fpi <- NULL
	} else {
		dw <- convertWidth(unit(1-sum(gm$outer.margins[c(2,4)]),"npc"), "inch", valueOnly=TRUE)
		dh <- convertHeight(unit(1-sum(gm$outer.margins[c(1,3)]),"npc"), "inch", valueOnly=TRUE)
		
		fpi <- preprocess_facet_layout(gm, gm$legend.outside, dh, dw)
		
		# aspect ratio of total device
		tasp <- dw/dh
		
		# aspect ratio per facet
		fasp <- fpi$dsw / fpi$dsh #-  fpi$pSH - fpi$between.margin.in)
		
		# aspect ratio per facet minus extern legend
		#lasp <- fasp * (1-fpi$legmarx) / (1-fpi$legmary-fpi$attrmary-fpi$attrmary)
		# !! extern legend already calculated in dsw and dsh in preprocess_facet_layout (see #287)
		lasp <- fasp
	}
	list(shape.dw = dw,
		 shape.dh = dh,
		 shape.tasp = tasp,
		 shape.lasp = lasp,
		 shape.fpi = fpi)
}
