add_package_checks(warnings_are_errors = FALSE)

get_stage("install") %>%
	add_step(step_install_cran("lwgeom", configure.args="--without-liblwgeom")) %>% 
	add_step(step_install_github("mtennekes/tmaptools"))

###
# deploy pkgdowm site
###
if (Sys.getenv("id_rsa") != "") {
	
	get_stage("before_deploy") %>%
		add_step(step_setup_ssh())
	
	get_stage("deploy") %>%
		add_step(step_build_pkgdown())
}
