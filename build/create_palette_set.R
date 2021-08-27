library(pals)
library(rcartocolor)
library(grid)

tmap_pal_name_compress = function(x) tolower(gsub("[-, \\,, (, ), \\ ]",  "", x))


hcl = lapply(c("qualitative", "sequential", "diverging", "divergingx"), hcl.pals)
# merge diverging and divergingx
hcl[[3]] = c(hcl[[3]], hcl[[4]])
hcl[[4]] = NULL

hcl_labels = unlist(hcl)
hcl_names = tmap_pal_name_compress(hcl_labels)
hcl_n = rep(Inf, length(hcl_labels))
hcl_type = unlist(mapply(rep, c("cat", "seq", "div"), sapply(hcl, length), USE.NAMES = FALSE, SIMPLIFY = TRUE))
hcl_series = rep("hcl", length(hcl_labels))
hcl_package = rep("grDevices", length(hcl_labels))
hcl_fun = rep("hcl.colors", length(hcl_labels))
hcl_fun_type_arg = rep("palette", length(hcl_labels))

palette_hex = grDevices:::.palette_colors_hex
palette_labels = names(palette_hex)
palette_names = tmap_pal_name_compress(palette_labels)
palette_n = unname(sapply(palette_hex, length, USE.NAMES = FALSE))
palette_type = rep("cat", length(palette_hex))
palette_series = rep("palette", length(palette_hex))
palette_package = rep("grDevices", length(palette_hex))
palette_fun = rep("palette.colors", length(palette_hex))
palette_fun_type_arg = rep("palette", length(palette_hex))

pals_syspals = pals:::syspals
pals_syspals = pals_syspals[names(pals_syspals) %in% ls(asNamespace("pals"))]

pals_functions = ls(asNamespace("pals")) 

pals_names = names(pals_syspals)
pals_labels = pals_names
pals_n = unname(sapply(pals_names, function(b) {
	length(pals_syspals[[b]])
}))
pals_series = ifelse(substr(pals_names, 1, 6) == "brewer", "brewer",
					 ifelse(substr(pals_names, 1, 5) == "ocean", "ocean",
					 	   ifelse(substr(pals_names, 1, 6) == "kovesi", "kovesi",
					 	   	   ifelse(pals_names %in% c("magma", "inferno", "plasma", "viridis"), "viridis", "misc"))))
pals_type = ifelse(pals_names %in% c("alphabet", "alphabet2", "cols25", "glasbey", "kelly", "polychrome", "stepped", "stepped2", "stepped3", "okabe", "tableau20", "tol", "tol.groundcover", "watlington", 
									 "brewer.set1", "brewer.set2", "brewer.set3", "brewer.pastel1",
									 "brewer.pastel2", "brewer.dark2", "brewer.paired"), "cat",
				   ifelse(pals_names %in% c("coolwarm", "cubehelix", "brewer.brbg", "brewer.piyg", "brewer.prgn", "brewer.puor", "brewer.rdbu", "brewer.rdgy", "brewer.rdylbu", "brewer.rdylgn", "brewer.spectral", "ocean.balance", "ocean.delta", "ocean.curl", pals_names[substr(pals_names, 1, 16) == "kovesi.diverging"]), "div", ifelse(substr(pals_names, 1, 13) == "kovesi.cyclic", "cyc", "seq")))

pals_labels[pals_series == "brewer"] = substr(pals_labels[pals_series == "brewer"], 8, nchar(pals_labels[pals_series == "brewer"]))
pals_labels[pals_series == "ocean"] = substr(pals_labels[pals_series == "ocean"], 7, nchar(pals_labels[pals_series == "ocean"]))
pals_labels[pals_series == "kovesi" & pals_type == "cyc"] = substr(pals_labels[pals_series == "kovesi" & pals_type == "cyc"], 15, nchar(pals_labels[pals_series == "kovesi" & pals_type == "cyc"]))
pals_labels[pals_series == "kovesi" & pals_type == "div"] = substr(pals_labels[pals_series == "kovesi" & pals_type == "div"], 18, nchar(pals_labels[pals_series == "kovesi" & pals_type == "div"]))
pals_labels[pals_series == "kovesi" & pals_type == "seq"] = substr(pals_labels[pals_series == "kovesi" & pals_type == "seq"], 8, nchar(pals_labels[pals_series == "kovesi" & pals_type == "seq"]))


pals_n[pals_type != "cat"] = Inf


pals_package = rep("pals", length(pals_names))
pals_fun = pals_names
pals_fun_type_arg = rep("", length(pals_names))

biv_names = c(
	"arc.bluepink",
	"brewer.qualbin",
	"brewer.divbin",
	"brewer.divseq",
	"brewer.qualseq",
	"brewer.divdiv",
	"brewer.seqseq1",
	"brewer.seqseq2",
	"census.blueyellow",
	"tolochko.redblue",
	"stevens.pinkgreen",
	"stevens.bluered",
	"stevens.pinkblue",
	"stevens.greenblue",
	"stevens.purplegold")
biv_n = c(16, 6, 6, rep(9, 12))
biv_labels = biv_names
biv_series = rep("bivariate", length(biv_names))
biv_package  = rep("pals", length(biv_names))
biv_fun = biv_names
biv_fun_type_arg = rep("", length(biv_names))
biv_type = rep("biv", length(biv_names))


carto_names = metacartocolors$Name
carto_labels = carto_names
carto_n = metacartocolors$Max_n
carto_series = rep("carto", length(carto_names))
carto_package = rep("rcartocolor", length(carto_names))
carto_fun = rep("carto_pal", length(carto_names))
carto_fun_type_arg = rep("name", length(carto_names))
carto_type = ifelse(metacartocolors$Type == "diverging", "div", ifelse(metacartocolors$Type == "qualitative", "cat", "seq"))


gn = function(x, name) get(paste(x, name, sep = "_"))

.tmap_pals = do.call(rbind, lapply(c("hcl", "palette", "pals", "biv", "carto"), function(x) {
	data.frame(name = gn(x, "names"),
			   label = gn(x, "labels"),
			   package = gn(x, "package"),
			   type = gn(x, "type"),
			   series = gn(x, "series"),
			   maxn = gn(x, "n"),
			   fun = gn(x, "fun"),
			   fun_type_args = gn(x, "fun_type_arg"))
}))


.tmap_pals$fullname = paste(.tmap_pals$series, .tmap_pals$name, sep = "__")


usethis::use_data(.tmap_pals, internal = TRUE, overwrite = TRUE)



tmap_pals = local({
	fnm = split(.tmap_pals$label, list(.tmap_pals$package, .tmap_pals$series)) 
	nm = split(.tmap_pals$name, list(.tmap_pals$package, .tmap_pals$series)) 
	
	fnm = fnm[vapply(fnm, FUN = function(x) length(x) > 0, FUN.VALUE = logical(1))]
	nm = nm[vapply(nm, FUN = function(x) length(x) > 0, FUN.VALUE = logical(1))]
	
	mapply(function(fnmi, nmi) {
		names(fnmi) = nmi
		as.list(fnmi)
	}, fnm, nm)
	
})

usethis::use_data(tmap_pals, internal = FALSE)


