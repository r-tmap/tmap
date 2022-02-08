



















#################################################################3
### bivariate palettes

# get all palettes that are currently included in tmap4 (grDevices, pals, and rcartocolor)
n = 7
paldf = tmap:::.tmap_pals

div_paldf = paldf[paldf$type == "div", ]

div_names = div_paldf$fullname
div_pals = lapply(div_names, FUN = tmap:::tmapGetPalette, n = n)





div_scores = as.data.frame(t(sapply(div_pals, function(p) {
	check_div_pal(p)
})))

div_paldf = cbind(div_paldf, div_scores)


div_paldf_sel = div_paldf[div_paldf$inter_wing_dist > 10 & div_paldf$min_step > 9 & div_paldf$series == "hcl", ]
div_paldf_sel = div_paldf_sel[order(div_paldf_sel$inter_wing_dist, div_paldf_sel$min_step, decreasing = TRUE), ]

div_paldf_sel = div_paldf_sel[div_paldf_sel$label != "Spectral", ]

tmap_show_palettes(full_names = div_paldf_sel$fullname, n = 9)
tmap_show_palettes(full_names = div_paldf_sel$fullname, color_vision_deficiency = "deutan", n = 9)
tmap_show_palettes(full_names = div_paldf_sel$fullname, color_vision_deficiency = "protan", n = 9)
tmap_show_palettes(full_names = div_paldf_sel$fullname, color_vision_deficiency = "tritan", n = 9)







spectra = c(c("Terrain", "Terrain 2", "Viridis", "Plasma", "Inferno", "Rocket", "Turku", "Hawaii", "Batlow"))
#			paldf$label[paldf$series == "ocean" & paldf$type == "seq"])

n = 7

spec_paldf = paldf[paldf$label %in% spectra, ]

spec_names = spec_paldf$fullname
spec_pals = lapply(spec_names, FUN = tmap:::tmapGetPalette, n = n)



spec_scores = as.data.frame(t(sapply(spec_pals, function(p) {
	check_seq_pal(p)
})))

spec_paldf = cbind(spec_paldf, spec_scores)


spec_paldf_sel = spec_paldf#[spec_paldf$inter_wing_dist > 10 & spec_paldf$min_step > 9 & spec_paldf$series == "hcl", ]
spec_paldf_sel = spec_paldf_sel[order(spec_paldf_sel$min_step, -spec_paldf_sel$max_step, decreasing = TRUE), ]

tmap_show_palettes(full_names = spec_paldf_sel$fullname, n = 9)
tmap_show_palettes(full_names = spec_paldf_sel$fullname, color_vision_deficiency = "deutan", n = 9)
tmap_show_palettes(full_names = spec_paldf_sel$fullname, color_vision_deficiency = "protan", n = 9)
tmap_show_palettes(full_names = spec_paldf_sel$fullname, color_vision_deficiency = "tritan", n = 9)


### seq

seq_paldf = paldf[!(paldf$label %in% spectra) & paldf$type == "seq" & paldf$series == "hcl", ]


seq_names = seq_paldf$fullname
seq_pals = lapply(seq_names, FUN = tmap:::tmapGetPalette, n = n)



seq_scores = as.data.frame(t(sapply(seq_pals, function(p) {
	check_seq_pal(p)
})))

seq_paldf = cbind(seq_paldf, seq_scores)


seq_paldf_sel = seq_paldf[seq_paldf$min_step > 6, ]
seq_paldf_sel = seq_paldf_sel[order(seq_paldf_sel$min_step, -seq_paldf_sel$max_step, decreasing = TRUE), ]

tmap_show_palettes(full_names = seq_paldf_sel$fullname, n = 7)
tmap_show_palettes(full_names = seq_paldf_sel$fullname, color_vision_deficiency = "deutan", n = 7)
tmap_show_palettes(full_names = seq_paldf_sel$fullname, color_vision_deficiency = "protan", n = 7)
tmap_show_palettes(full_names = seq_paldf_sel$fullname, color_vision_deficiency = "tritan", n = 7)

### cyclic
# cycl = c("kovesi.rainbow_bgyr_35_85_c72", "kovesi.rainbow_bgyr_35_85_c73", "kovesi.rainbow_bgyrm_35_85_c69", "kovesi.rainbow_bgyrm_35_85_c71", paldf$name[paldf$type=="cyc"])
# 
# cycl_fn = paldf$fullname[match(cycl, paldf$name)]
# 
# tmap_show_palettes(full_names = cycl_fn, n = 7)
# tmap_show_palettes(full_names = cycl_fn, color_vision_deficiency = "deutan", n = 7)
# tmap_show_palettes(full_names = cycl_fn, color_vision_deficiency = "protan", n = 7)
# tmap_show_palettes(full_names = cycl_fn, color_vision_deficiency = "tritan", n = 7)




rm(.tmap_pals)
.tmap_pals$min_step = ""
.tmap_pals$inter_wing_dist = ""
.tmap_pals$min_step[match(div_paldf$fullname, .tmap_pals$fullname)] = div_paldf$min_step
.tmap_pals$inter_wing_dist[match(div_paldf$fullname, .tmap_pals$fullname)] = div_paldf$inter_wing_dist
.tmap_pals$include = ""

