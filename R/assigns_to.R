#' Locator test for assignment to a specific name or regex
#'
#' This locator test looks for an assignment to the named variable or,
#' if \code{regex} is given, to a name matching the regex.
#' @return a function that can be used in a locator test sequence. That function
#' takes a capture as input and returns a capture as output
#'
#' @details You can specify both \code{name} and \code{regex}. If both are specified, the
#' failure message will refer to \code{name} (if \code{hint = TRUE}).
#'
#' @param name a character string or unquoted bare name that should be the name assigned to the result of a code line.
#' @param message character string with the failure message
#' @param hint if \code{TRUE}, failure message should show the name to the student
#' @param regex a regex that defines the match
#' @export
assigns_to <- function(name= NULL, message = NULL, hint = FALSE, regex = NULL) {
  name <- as.character(substitute(name))
  specified_name <-
    if (is.null(regex)) name
    else if (is.null(name))
      sprintf("matching the regex %s", regex)
    else name
  f <- function(capture) {
    # short circuit if a failed test is input
    if (!capture$passed) return(capture)
    result <- FALSE
    found_some_assignment <- FALSE
    for (k in capture$valid_lines) {
      obj_names <- ls(capture$names[[k]], all.names = TRUE)
      if (length(obj_names) > 0) found_some_assignment <- TRUE
      if (is.null(regex)) {
        if (name %in% obj_names) {
          result <- TRUE
          line <- k
          break # found it!
        }
      } else {
        if(any(grepl(regex, obj_names))) {
          result <- TRUE
          line <- k
          break # found it!
        }
      }
    }

    if (result) {
      capture$passed <- TRUE
      capture$line <- line
    } else {
      capture$passed <- FALSE
      capture$line <- NA
      if (is.null(message)) {
        capture$message <- if (found_some_assignment) {
          ifelse(hint,
                 sprintf("didn't see the name %s among the assignments", specified_name),
                 "didn't see the assignment the problem asked you to make"
          )
        } else {
          ifelse(hint,
                 sprintf("didn't see name %s being assigned to a result", specified_name),
                 "didn't see any assignment of a name to a result")
        }
      } else {
        capture$message <- message
      }

    }


    capture
  }
  f
}
