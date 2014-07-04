require(devtools)

## Create palletes, vignette and documentation...
setwd("../build")
source("roxygen.R")
setwd("../pkg")


check_doc()

ck <- check()

++++++## check
if (ck) {
	unlink( '../output', TRUE)
	dir.create("../output", showWarnings=FALSE)
	build(path= "../output")
}
