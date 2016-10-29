library(devtools)
ck <- function(run_dont_test=FALSE) {
	unlink("output")
	dir.create("output", showWarnings = FALSE)
	check("pkg", check_dir="output", run_dont_test = run_dont_test)
}
