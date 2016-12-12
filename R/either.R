#' Combine results of locator tests
#'
#' \code{either()} works on multiple tests. If any of the tests pass, the whole set of tests
#' passes.
#'
#' @param test_1 a locator test
#' @param test_2 another locator test
#' @param ... optionally, more locator tests
#'
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
