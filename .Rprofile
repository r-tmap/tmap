library(devtools)
ck <- function() {
	unlink("output")
	dir.create("output", showWarnings = FALSE)
	check("pkg", cleanup = FALSE, check_dir="output")
}
