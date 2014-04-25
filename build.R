require(devtools)

## Create palletes, vignette and documentation...
# setwd("../build")
# source("roxygen.R")
# setwd("../pkg")


ck <- check()

check_doc()

++++++## check
if (ck) {
	unlink( '../output', TRUE)
	dir.create("../output", showWarnings=FALSE)
	build(path= "../output")
}
