## create documentation
require(roxygen2)
options(error=traceback)


unlink( '../pkg/man', TRUE)

setwd('../pkg')

roxygenize( '.'
			, roxygen.dir='.'
			, copy.package=FALSE
			, unlink.target=TRUE
)
setwd('../build')