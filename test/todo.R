# run demo script



cartoMap(shp.wp, col=grey(seq(0.3, 0.8, by=0.05)), plot.borders=FALSE)

plot(shp.gm, col=NA, add = TRUE)



my.plot.Spatial <-
function (x, xlim = NULL, ylim = NULL, asp = NA, axes = FALSE, 
		  bg = par("bg"), ..., setParUsrBB = FALSE) 
{
	bbox <- bbox(x)
	if (is.null(xlim)) 
		xlim <- bbox[1, ]
	if (is.null(ylim)) 
		ylim <- bbox[2, ]
	if (is.na(asp)) 
		asp <- ifelse(is.na(proj4string(x)) || is.projected(x), 
					  1, 1/cos((mean(ylim) * pi)/180))
	frame()
	if (is.R()) {
		#plot.window(xlim = xlim, ylim = ylim, asp = asp, ...)
		if (setParUsrBB) 
			par(usr = c(xlim, ylim))
	}
	else {
		plot.default(x = bbox[1, ], y = bbox[2, ], type = "n", 
					 xlim = xlim, ylim = ylim, asp = asp, ann = FALSE, 
					 axes = FALSE, ...)
		if (setParUsrBB) 
			par(usr = c(xlim, ylim))
	}
	pl_reg <- par("usr")
	rect(xleft = pl_reg[1], ybottom = pl_reg[3], xright = pl_reg[2], 
		 ytop = pl_reg[4], col = bg, border = FALSE)
	if (axes) {
		box()
		isp = is.projected(x)
		if (!is.na(isp) && !isp) {
			degAxis(1, ...)
			degAxis(2, ...)
		}
		else {
			axis(1, ...)
			axis(2, ...)
		}
	}
}


unlockBinding("plot.Spatial", getNamespace("sp"))
assign("plot.Spatial", my.plot.Spatial, getNamespace("sp"))





