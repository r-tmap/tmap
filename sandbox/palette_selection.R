
library(colorblindcheck)
library(tmap) # version 4 needed

#' Check diverging palette
#' 
#' Check diverging palette. It computes two quality indices. \code{inter_wing_dist} minimal distance between from one wing (any color) to the other wing (any color); let a step be the distance from one color to a neighboring color, then: \code{min_step} is the minimal step. These two quality indices are computed for all three color vision deficiency types: per quality indicator, the worst score is returned.
#' 
#' @param p diverging palette with odd number of colors (so with a middle color)
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
		#mean_step_size = mean(step_sizes)
		#step_indicator = max(abs(step_sizes - mean_step_size)) / mean_step_size
		c(inter_wing_dist = round(inter_wing_dist), min_step = round(min_step_size))
	}))
	c(inter_wing_dist = min(scores[,1]), min_step = min(scores[,2]))
}


#' Check sequential palette
#' 
#' Check sequential palette. It computes two quality indices. \code{min_step} and \code{max_step} are the minimum and maximum step respectively, where a step is the distance from one color to a neighboring color. \code{min_step} is the leading indicator: the higher, the better the palette. From palettes with equal \code{min_step}, those with the lowest \code{max_step} can be considered as better, because the steps are more uniform. These two quality indices are computed for all three color vision deficiency types: per quality indicator, the worst score is returned. 
#' 
#' and \code{max_step} is low, although the former is much more important.
#' 
#' @param p sequential palette
#' @return vector of three quality indices
check_seq_pal = function(p) {
	n = length(p)

	cvds = c("deu", "pro", "tri")
	
	scores = t(sapply(cvds, function(cvd) {
		m = colorblindcheck::palette_dist(p, cvd = cvd)
		step_sizes = mapply(function(i,j) m[i,j], 1:(n-1), 2:n)
		min_step_size = min(step_sizes)
		max_step_size = max(step_sizes)
		#mean_step_size = mean(step_sizes)
		#step_indicator = max(abs(step_sizes - mean_step_size)) / mean_step_size
		c(min_step = round(min_step_size), max_step = round(max_step_size))
	}))
	c(min_step = min(scores[,1]), max_step = min(scores[,2]))
}

#' Check cyclic palette
#' 
#' Check cyclic palette. Same as \code{check_seq_pal}, but also the difference between the first and last color is considered as step
#' 
#' @param p cyclic palette
#' @return vector of three quality indices
check_cyc_pal = function(p) {
	n = length(p)
	cvds = c("deu", "pro", "tri")
	
	scores = t(sapply(cvds, function(cvd) {
		m = colorblindcheck::palette_dist(c(p, p[1]), cvd = cvd)
		step_sizes = mapply(function(i,j) m[i,j], 1:n, 2:(n+1))
		min_step_size = min(step_sizes)
		max_step_size = max(step_sizes)
		#mean_step_size = mean(step_sizes)
		#step_indicator = max(abs(step_sizes - mean_step_size)) / mean_step_size
		c(min_step = round(min_step_size), max_step = round(max_step_size))
	}))
	c(min_step = min(scores[,1]), max_step = min(scores[,2]))
}

#' Check categorical palette
#' 
#' Check categorical palette. It computes one quality indicator: the \code{min_dist}, the minimal distance between any two colors. This is computed for all three color vision deficiency types: the worst (i.e. lowest) score is returned. 
#' 
#' and \code{max_step} is low, although the former is much more important.
#' 
#' @param p sequential palette
#' @return vector of three quality indices
check_cat_pal = function(p) {
	cvds = c("deu", "pro", "tri")
	
	scores = sapply(cvds, function(cvd) {
		colorblindcheck::palette_dist(p, cvd = cvd)
	})
	c(min_dist = round(min(scores, na.rm = TRUE)))
}




pals = .tmap_pals

cols = list()

for (type in c("seq", "div", "cyc")) {
	cols[[type]] = local({
		nms = pals$fullname[pals$type == type]
		lst = lapply(nms, function(p) tmap_get_palette(p, n = ifelse(type == "cyc", 9, 7), contrast =  c(0,1)))
		names(lst) = nms
		lst
	})
}

for (i in c(6, 8, 10, 12, 20)) {
	cols[[paste0("cat", i)]] = local({
		nms = pals$fullname[pals$type == "cat" & pals$maxn >= i]
		lst = lapply(nms, function(p) tmap_get_palette(p, n = i, contrast =  c(0,1)))
		names(lst) = nms
		lst
	})
}


# check black and whites
# which(sapply(do.call(c, cols), function(p) {
# 	sum(p %in% c("#000000", "#FFFFFF"))
# }) > 0 )



x = mapply(function(col, type) {
	if (substr(type, 1, 3) == "cat") {
		n = as.integer(substr(type, 4, nchar(type)))
		type = substr(type, 1, 3)
	}
	fun = paste0("check_", type, "_pal")
	do.call(rbind, lapply(col, function(cl) {
		do.call(fun, list(p=cl))
	}))
}, cols, names(cols), SIMPLIFY = FALSE)

f = function(x) format(x, width=2)

y = mapply(function(a, b, nm) {
	df = cbind(data.frame(fullname = names(a)), as.data.frame(b))
	if (nm == "div") {
		df = df[order(pmin(df$inter_wing_dist, df$min_step * 2), decreasing = TRUE), ]
		df$q = paste0("iwd:", f(df$inter_wing_dist), "; min_step:", f(df$min_step))
		df$maxn = 9
	} else if (nm %in% c("seq", "cyc")) {
		df = df[order(df$min_step, -df$max_step, decreasing = TRUE), ]
		df$q = paste0("min_step:", f(df$min_step), "; max_step:", f(df$max_step))
		df$maxn = ifelse(nm == "cyc", 9, 7)
	} else {
		df = df[order(df$min_dist, decreasing = TRUE), ]
		df$q = paste0("min_dist:", f(df$min_dist))
		df$maxn = as.integer(substr(nm, 4, nchar(nm)))
	}
	df[, c("fullname", "q", "maxn"), drop = FALSE]
}, cols, x, names(cols), SIMPLIFY = FALSE)

pals2 = do.call(rbind, y)

library(tidyverse)
pals3 = pals2 %>% 
	left_join({pals %>% mutate(maxn = NULL)}, by = "fullname")


tmap_show_palettes(pals = pals3)

