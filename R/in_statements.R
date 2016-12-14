#' Look for matches to the contents of the statements themselves

#' @param pattern character string containing the pattern to search for in the statements
#' @param message character string giving the failure message
#' @param regex if \code{TRUE} treat the pattern as a regex
#' @param mistake if \code{TRUE}, the test fails if the pattern is located
#' @export
in_statements <- function(pattern, message = NULL, regex = FALSE, mistake = FALSE ) {
  f <- function(capture) {
    if (!capture$passed) return(capture) # short circuit the test if a previous test failed

    capture$created_by <- sprintf("in_statements(%s)", pattern)
    for (k in capture$valid_lines) {
      if (grepl(pattern, capture$statements[[k]], fixed = !regex)) {
        # we found a match!
        capture$line <- k
        if (mistake) break
        else return(capture)
      }
    }

    # if we got here, the test failed
    capture$passed <- FALSE
    capture$message <- message

    capture
  }
  f
}

