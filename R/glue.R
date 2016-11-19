#' glue to hold the test statements together
#'
#' @param EX A set of expressions of the sort returned by capture.code()

#' @export
then <- function(EX) {
  still_valid <- EX$valid_lines > EX$line
  EX$valid_lines <- EX$valid_lines[still_valid]
  EX
}

#' @export
next_expression <- function(EX) {
  get_expression(EX, line = 2)
}

#' @export
get_expression <- function(EX, line = 1) {
  if (line < 1 || line > length(EX$valid_lines))
    stop("Invalid line number.")
  EX$valid_lines <- EX$valid_lines[line]
  EX
}

#' @export
either <- function(test_1, test_2) {
  f <- function(code) {
    one <- test_1(code)
    if (one$passed) return(one)

    two <- test_2(code)
    if (two$passed) return(two)

    # neither was found
    return(list(passed = FALSE, line = NA, code = code,
                message = paste("Either", one$message, "or", two$message)))
  }

  f
}
