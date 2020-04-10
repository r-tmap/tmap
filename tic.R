do_package_checks(error_on = "warning")

get_stage("install") %>%
	add_code_step(install.packages("lwgeom", configure.args="--without-liblwgeom"))

do_pkgdown(document = FALSE)
