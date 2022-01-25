step4_plot_collect_legends = function(tmx) {
	# collect legends
	dt_template = data.table::data.table(by1__ = integer(0), by2__ =  integer(0), by3__ =  integer(0), legend = list())
	legs = data.table::rbindlist(c(list(dt_template), lapply(tmx, function(tmxi) {
		data.table::rbindlist(lapply(tmxi$layers, function(tml) {
			
			legs_cached = get("legs", .TMAP)
			
			legs = c(tml$trans_legend, tml$mapping_legend)
			
			legs = lapply(legs, function(l) {
				l[, legend:=list(legs_cached[l$legnr])]
				l
			})
			
			legs2 = lapply(legs, function(legs_aes) {
				legs_aes$vneutral = unlist(lapply(legs_aes$legend, function(l) l$vneutral), use.names = FALSE)
				legs_aes
			})
			
			# find shared legends
			
			clones = vapply(legs2, function(l) {
				a = l$legend[[1]]$aes
				if (!is.null(a)) a else ""
			}, FUN.VALUE = character(1))
			
			
			legs2b = mapply(function(l, lnm) {
				w = which(clones == lnm)
				if (length(w) > 0L) {
					legsclone = lapply(legs2[w], function(li) {
						li$legend
					})
					num_legends = vapply(legsclone, length, numeric(1))
					if (any(num_legends != length(l$legend))) warning("legends could not be shared; the aesthetics need the same .free specification", call. = FALSE)
					
					
					l$legend = do.call(mapply, args = c(list(FUN = function(l, ...) {
						clns = list(...)
						k = length(l$vvalues)
						l$clones = lapply(clns, function(cl) {
							vv = cl$vvalues
							if (k != length(vv)) stop("legends could not be shared; the number of legend items is different", call. = FALSE)
							vv
						})
						names(l$clones) = names(clones[w])
						l
					}, SIMPLIFY = FALSE), c(list(l$legend), legsclone)))
				}
				l
			}, legs2, names(legs2), SIMPLIFY = FALSE)
			
			
			copy_neutral = (length(legs) > 1)
			
			legs3 = mapply(function(legs_aes, legnm, i) {
				bvars = names(legs_aes)[substr(names(legs_aes), 1, 2) == "by"]
				
				if (!is.null(legs_aes)) {
					for (k in 1:nrow(legs_aes)) {
						if (length(legs_aes$legend[[k]]) == 1 || !legs_aes$legend[[k]]$show) {
							legs_aes$legend[[k]] = list(NULL)
							next
						}
						
						bval = legs_aes[k, bvars, with = FALSE]
						gp = tml$gp
						
						# fill gp values with (scaled) aes data
						gpaid = which(paste0("__", legnm) == unlist(gp, use.names = FALSE))
						for (j in gpaid) gp[[j]] = legs_aes$legend[[k]]$vvalues
						
						# will gp with neutral values for other graphical variables
						if (copy_neutral) {
							nvalues = mapply(function(lclone, lname) {
								bvars2 = names(lclone)[substr(names(lclone), 1, 2) == "by"]
								
								bvars_int = intersect(bvars, bvars2)
								if (!length(bvars_int)) {
									lclone$vneutral[1]
								} else  {
									lclone[bval, on = bvars_int]$vneutral
								}
							}, legs2[-i], names(legs2[-i]), SIMPLIFY = FALSE)
							
							
							for (j in 1L:length(nvalues)) {
								gpid = which(paste0("__", names(nvalues)[j]) == sapply(gp, "[[", 1))
								for (jj in gpid) gp[[jj]] = nvalues[[j]]	
							}
							
						}
						
						
						leleg = legs_aes$legend[[k]]
						
						# get gp values from shared legends (slave)
						if ("clones" %in% names(leleg)) {
							clist = leleg$clones
							for (j in 1L:length(clist)) {
								cnm = names(clist)[j]
								gp[[cnm]] = clist[[j]]
							}
						}
						
						leleg$gp = gp
						leleg$vneutral = NULL
						leleg$vvalues = NULL
						
						if (length(leleg) == 1) {
							legs_aes$legend[[k]] = list(leleg)
						} else {
							legs_aes$legend[[k]] = leleg
						}
					}
				}
				legs_aes
			}, legs2b, names(legs2b), 1:length(legs2b), SIMPLIFY = FALSE)
			
			data.table::rbindlist(legs3, fill = TRUE)
		}), fill = TRUE)
	})), fill = TRUE)
	
	# remove empty legends
	legs = legs[vapply(legs$legend, length, FUN.VALUE = integer(1)) > 1, ][, vneutral := NULL]
	
	set(legs, j= 5:ncol(legs), value = NULL)
	
	legs
}
