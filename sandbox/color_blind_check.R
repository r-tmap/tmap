library(colorblindcheck)
library(tmap) # version 4 needed


#################################################################3
### bivariate palettes

# get all palettes that are currently included in tmap4 (grDevices, pals, and rcartocolor)
n = 7
paldf = tmap:::.tmap_pals

div_paldf = paldf[paldf$type == "div", ]

div_names = div_paldf$fullname
div_pals = lapply(div_names, FUN = tmap:::tmapGetPalette, n = n)



#' Check bivariate palette
#' 
#' Check bivariate palette. It computes three quality indices. \code{inter_wing_dist} minimal distance between from one wing (any color) to the other wing (any color); let a step be the distance from one color to a neighboring color, then: \code{min_step} is the minimal step; \code{step_indicator} represents the homogeneity of the steps, more precisely, it represents how much the smallest or largest step deviates from the mean step: \code{max(abs(step - mean(step))) / mean(step)}. These three quality indices are computed for all three color vision deficiency types: per quality indicator, the worst score is returned.
#' 
#' @param p bivariate color palette with odd number of colors (so with a middle color)
#' @return vector of three quality indices
check_div_pal = function(p) {
	n = length(p)
	if ((n %% 2) != 1) stop("p needs to be odd-numbered")
	nh = floor(n/2)
	
	cvds = c("deu", "pro", "tri")
	
	scores = t(sapply(cvds, function(cvd) {
		m = colorblindcheck::palette_dist(p, cvd = cvd)
		inter_wing_dist = min(m[1:nh, (nh+2):n])
		step_sizes = mapply(function(i,j) m[i,j], 1:(n-1), 2:n)
		min_step_size = min(step_sizes)
		mean_step_size = mean(step_sizes)
		step_indicator = max(abs(step_sizes - mean_step_size)) / mean_step_size
		c(inter_wing_dist = inter_wing_dist, min_step = min_step_size, step_indicator = step_indicator)	
	}))
	c(inter_wing_dist = min(scores[,1]), min_step = min(scores[,2]), step_indicator = max(scores[,3]))
}


div_scores = as.data.frame(t(sapply(div_pals, function(p) {
	check_div_pal(p)
})))

div_paldf = cbind(div_paldf, div_scores)

# calculate normalized scores, and compute sqrt (to penalize low scores)
div_paldf$iwd_norm = sqrt(div_paldf$inter_wing_dist / max(div_paldf$inter_wing_dist))
div_paldf$ms_norm = sqrt(div_paldf$min_step / max(div_paldf$min_step))
div_paldf$si_norm = sqrt(1-(div_paldf$step_indicator / max(div_paldf$step_indicator)))

div_paldf$score_min = pmin(div_paldf$iwd_norm, div_paldf$ms_norm, div_paldf$si_norm)
div_paldf$score_mean = (div_paldf$iwd_norm + div_paldf$ms_norm + div_paldf$si_norm) / 3

div_paldf = div_paldf[order(div_paldf$score_mean, decreasing = TRUE), ]

tmap_show_palettes(full_names = div_paldf$fullname, n = 7)
tmap_show_palettes(full_names = div_paldf$fullname, color_vision_deficiency = "deutan", n = 7)
tmap_show_palettes(full_names = div_paldf$fullname, color_vision_deficiency = "protan", n = 7)
tmap_show_palettes(full_names = div_paldf$fullname, color_vision_deficiency = "tritan", n = 7)




example_biv_choropleth = function(pal, dir = "./biv_plots") {
	rank = which(div_paldf$name == pal)
	
	scores = as.list(div_paldf[rank, c("iwd_norm", "ms_norm", "si_norm", "score_min", "score_mean")])
	text = paste("rank:", sprintf("%02d", rank), "-", paste(as.vector(rbind(paste0(names(scores), ":"), sprintf("%.2f", round(unlist(scores), 2)))), collapse = " "))
	
	
	dir.create(dir, showWarnings = FALSE)
	require(tmap)
	data(World)
	World$HPI2 = World$HPI - 28
	
	
	# tmap_save and tmap_arrange not implemented yet for tmap4: work around
	require(grid)
	png(file.path(dir, paste0(sprintf("%02d", rank), "_", pal, ".png")), width = 1200, height = 600)
	
	
	
	grid.newpage()
	grid.text(text)
	vp = viewport(layout = grid.layout(2, 2))
	pushViewport(vp)
	cvd = c("none", "deutan", "protan", "tritan")
	for (row in 1:2) {
		for (col in 1:2) {
			i = col + (row-1) * 2
			
			tm = tm_shape(World, crs = 8857) + 
				tm_polygons("HPI2", 
							fill.scale = tm_scale_intervals(n=9, values = pal, value.na = "grey100", breaks = seq(-17, 17, length.out = 10), labels = (-4):4),
							fill.legend = tm_legend("")) + 
				tm_layout(color.vision.deficiency.sim = cvd[i], panel.labels = cvd[i])# +
				#tm_title(cvd[i], position = tm_pos_auto_in(), frame = FALSE)
			print(tm, vp = viewport(layout.pos.col = col, layout.pos.row = row))
		}
	}
	upViewport()
	dev.off()
}

for (name in div_paldf$name[1:30]) {
	example_biv_choropleth(name)
}


## find color for missing values
hcldf = as.data.frame(expand.grid(h = seq(0, 350, by = 10), c =seq(30,60,by=10), l = seq(20,90, by = 10)))
hclcols = do.call(hcl, as.list(hcldf))


cols = tmap_get_palette("purplegreen", 9)


x = do.call(abind::abind, c(lapply(c("deu", "pro", "tri"), function(cvd) {
		colorblindcheck::palette_dist(c(cols, hclcols), cvd = cvd)[1:9, 10:(9+nrow(hcldf))]	
	}), list(along = 3)))
m = apply(x, 1:2, min)


m = colorblindcheck::palette_dist(c(cols, hclcols))[1:9, 10:(length(hclcols)+9)]	


colNA = hclcols[which.max(apply(m, MARGIN = 2, min))]

colorspace::specplot(c(cols))
colorspace::specplot(c(cols, colNA))
colorspace::specplot(colorspace::deutan(c(cols, colNA)))

colorspace::specplot(colorspace::tritan(c(cols, colNA)))
colorspace::specplot(colorspace::protan(c(cols, colNA)))

