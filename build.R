require(devtools)

check_doc()

check()

unlink( '../output', TRUE)
dir.create("../output", showWarnings=FALSE)
build(path= "../output")
