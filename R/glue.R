#' glue to hold the test statements together
#'
#' @param EX A set of expressions of the sort returned by capture.code()

#' @export
before <- function(EX) {
  still_valid <- EX$code$valid_lines < EX$line
  EX$code$valid_lines <- EX$code$valid_lines[still_valid]
  EX
}

#' @export
after <- function(EX) {
  still_valid <- EX$code$valid_lines > EX$line
  EX$code$valid_lines <- EX$code$valid_lines[still_valid]
  EX
}

#' @export
either <- function(test_1, test_2) {
  f <- function(code) {
    one <- test_1(code)
    if (one$found) return(one)

    two <- test_2(code)
    if (two$found) return(two)

    # neither was found
    return(list(found = FALSE, line = NA, code = code,
                message = paste("Either", one$message, "or", two$message)))
  }

  f
}
