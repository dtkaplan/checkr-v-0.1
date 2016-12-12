#' glue to hold the test statements together
#'
#' These operate on the output of a locator test.
#' \code{then} restricts the subsequent tests to the lines after the one identified
#' \code{previously} restricts the subsequent tests to the lines before the one identified
#' @param capture A set of expressions of the sort returned by capture.code()

#' @export
then <- function(capture) {
  still_valid <- capture$valid_lines > capture$line
  capture$valid_lines <- capture$valid_lines[still_valid]
  capture$line <- NA

  capture
}

#' @export
previously <- function(capture) {
  still_valid <- capture$valid_lines < capture$line
  capture$valid_lines <- capture$valid_lines[still_valid]
  capture$line <- NA

  capture
}

