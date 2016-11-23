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
either <- function(test_1, test_2, ...) {
  f <- function(capture) {
    one <- test_1(capture)
    if (one$passed) return(one)

    two <- test_2(capture)
    if (two$passed) return(two)

    more_tests <- list(...)
    more_results <- list()
    more_messages <- character(length(more_tests))
    for (k in seq_along(more_tests)) {
      more_results[[k]] <- more_tests[[k]](capture)
      more_messages[[k]] <- more_results[[k]]$message
      if (more_results[[k]]$passed) return(more_results[[k]])
    }

    # none was found
    capture$passed <- FALSE
    capture$line <- NA
    capture$message <- paste("either", one$message, "or", two$message)
    if (length(more_results) > 0 ) {
      more_message <- paste(more_messages, collapse = ' or ')
      capture$message <- paste(capture$message, "or", more_message)
    }
    capture
  }

  f
}
