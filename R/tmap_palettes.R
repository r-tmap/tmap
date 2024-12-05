#rm = rownames(tmaptools::tmap.pal.info)
#dput(sapply(rm, cols4all:::c4a_name_convert))

pals_v3 = c(BrBG = "brewer.br_bg", PiYG = "brewer.pi_yg", PRGn = "brewer.prgn",
			PuOr = "brewer.pu_or", RdBu = "brewer.rd_bu", RdGy = "brewer.rd_gy",
			RdYlBu = "brewer.rd_yl_bu", RdYlGn = "brewer.rd_yl_gn", Spectral = "brewer.spectral",
			Accent = "brewer.accent", Dark2 = "brewer.dark2", Paired = "brewer.paired",
			Pastel1 = "brewer.pastel1", Pastel2 = "brewer.pastel2", Set1 = "brewer.set1",
			Set2 = "brewer.set2", Set3 = "brewer.set3", Blues = "brewer.blues",
			BuGn = "brewer.bu_gn", BuPu = "brewer.bu_pu", GnBu = "brewer.gn_bu",
			Greens = "brewer.greens", Greys = "brewer.greys", Oranges = "brewer.oranges",
			OrRd = "brewer.or_rd", PuBu = "brewer.pu_bu", PuBuGn = "brewer.pu_bu_gn",
			PuRd = "brewer.pu_rd", Purples = "brewer.purples", RdPu = "brewer.rd_pu",
			Reds = "brewer.reds", YlGn = "brewer.yl_gn", YlGnBu = "brewer.yl_gn_bu",
			YlOrBr = "brewer.yl_or_br", YlOrRd = "brewer.yl_or_rd")


getPal = function(name, n = NA, rep = TRUE, range = NA, reversed = FALSE) {
	if (name %in% c("cat", "seq", "div")) {
		name = cols4all::c4a_options("defaults")$defaults[[name]]
	}

	cols4all::c4a(name, n = n, nm_invalid = {if (rep) "repeat" else "interpolate"}, range = range, reverse = reversed)
}

getPalNA = function(name) {
	if (name %in% c("cat", "seq", "div")) {
		name = cols4all::c4a_options("defaults")$defaults[[name]]
	}
	cols4all::c4a_na(name)
}

getPalBiv = function(name, m = NA, n = NA, rep = TRUE) {
	cols4all::c4a(name, m = m, n = n, nm_invalid = {if (rep) "repeat" else "interpolate"})
}

getPalMeta = function(name, no.match = "null") {
	if (!is.character(name)) return(NULL)
	if (name %in% c("cat", "seq", "div")) {
		name = cols4all::c4a_options("defaults")$defaults[[name]]
	}
	if (name %in% names(pals_v3)) {
		oldname = name
		name = unname(pals_v3[oldname])
		info = cols4all::c4a_info(name, verbose = FALSE)
		message_c4a(oldname, info, fullname = info$series == "brewer")
	} else {
		info = cols4all::c4a_info(name, no.match = no.match, verbose = FALSE)
	}
	info
}
