#' glue to hold the test statements together
#'
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

#' @export
either <- function(test_1, test_2) {
  f <- function(capture) {
    one <- test_1(capture)
    if (one$passed) return(one)

    two <- test_2(capture)
    if (two$passed) return(two)

    # neither was found
    capture$passed <- FALSE
    capture$line <- NA
    capture$message <- paste("Either", one$message, "or", two$message)

    capture
  }

  f
}
