# Initialize the package

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.checkr <- list(
    checkr.logger = function(x) invisible() # do nothing
  )
  toset <- !(names(op.checkr) %in% names(op))
  if(any(toset)) options(op.checkr[toset])

  invisible()
}
