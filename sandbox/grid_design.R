library(grid)

sasp = 2

# no panel
o = list(nrows = 1,
		 ncols = 1,
		 outer.margins = c(0.02, 0.02, 0.02, 0.02),
		 inner.margins = NA,
		 meta.margins = c(0, 0, 0, 0.2),
		 between.margins = 0.5,
		 panel.label.height = 1.25,
		 grid.mark.height = 2,
		 xylab.height = 1.25,
		 coords.height = 1.25,
		 xlab.show = FALSE,
		 ylab.show = FALSE,
		 xlab.pos = "bottom",
		 ylab.pos = "right",
		 grid.show = FALSE,
		 grid.label.pos = c("right", "bottom"),
		 panel.type = "none", # "wrap" or "xtab",
		 panel.wrap.pos = "top", # or "left", "right", "bottom"
		 panel.xtab.pos = c("left", "top")
)
# 
# # one
# o = list(nrows = 1,
# 		 ncols = 1,
# 		 outer.margins = c(0.02, 0.02, 0.02, 0.02),
# 		 inner.margins = NA,
# 		 meta.margins = c(0, 0, 0, 0.2),
# 		 between.margins = 0.5,
# 		 panel.label.height = 1.25,
# 		 grid.mark.height = 2,
# 		 xylab.height = 1.25,
# 		 coords.height = 1.25,
# 		 xlab.show = FALSE,
# 		 ylab.show = FALSE,
# 		 xlab.pos = "bottom",
# 		 ylab.pos = "right",
# 		 grid.show = FALSE,
# 		 grid.label.pos = c("right", "bottom"),
# 		 panel.type = "wrap", # or "wrap" or "xtab",
# 		 panel.wrap.pos = "top", # or "left", "right", "bottom"
# 		 panel.xtab.pos = c("left", "top")
# 		 )
# 		 
# # wrap	
o = list(nrows = 3,
		 ncols = 3,
		 outer.margins = c(0.02, 0.02, 0.02, 0.02),
		 inner.margins = NA,
		 meta.margins = c(0, 0, 0, 0.2),
		 between.margins = 0.5,
		 panel.label.height = 1.25,
		 grid.mark.height = 2,
		 xylab.height = 1.25,
		 coords.height = 1.25,
		 xlab.show = FALSE,
		 ylab.show = FALSE,
		 xlab.pos = "bottom",
		 ylab.pos = "right",
		 grid.show = FALSE,
		 grid.label.pos = c("right", "bottom"),
		 panel.type = "wrap", # or "wrap" or "xtab",
		 panel.wrap.pos = "top", # or "left", "right", "bottom"
		 panel.xtab.pos = c("left", "top")
)
# 
# xtab
o = list(nrows = 3,
		 ncols = 3,
		 outer.margins = c(0.02, 0.02, 0.02, 0.02),
		 inner.margins = NA,
		 meta.margins = c(0, 0, 0, 0.2),
		 between.margins = 0.5,
		 panel.label.height = 1.25,
		 grid.mark.height = 2,
		 xylab.height = 1.25,
		 coords.height = 1.25,
		 xlab.show = FALSE,
		 ylab.show = FALSE,
		 xlab.pos = "bottom",
		 ylab.pos = "right",
		 grid.show = FALSE,
		 grid.label.pos = c("right", "bottom"),
		 panel.type = "xtab", # or "wrap" or "xtab",
		 panel.wrap.pos = "top", # or "left", "right", "bottom"
		 panel.xtab.pos = c("left", "top")
)
 



#o$x
#o$nrows = 6
#o$meta.margins = c(0.2,0.1,0,0)
#o$panel.type = "wrap"

o$asp = 0
tmapGridInit2(o)




