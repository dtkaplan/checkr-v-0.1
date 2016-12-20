#' Combine results of locator tests
#'
#' These functions combine multiple tests into a single test.
#'
#'
#' @details \code{any_test(t1, t2, ...)} If any of the tests pass, the whole set of tests
#' passes.
#'
#' \code{all_tests(t1, t2, ...)} All of the given tests must pass for the test to pass.
#'
#' \code{branch_test(condition, yes, no)}
#'
#'
#'
#' @param test_1 a locator test
#' @param test_2 another locator test
#' @param condition a test to determine which of \code{yes} or \code{no}
#' @param yes the test to run if \code{condition} passes
#' @param no the test to run if \code{condition} fails
#' @param ... optionally, more locator tests
#'
#' @rdname compose_tests
#' @export
any_test <- function(test_1, test_2, ...) {
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

#' @rdname compose_tests
#' @aliases all_tests
#' @export
all_tests <- function(...) {
  tests <- list(...)
  f <- function(capture) {
    for (k in seq_along(tests)) {
      capture$created_by <- sprintf("all_tests() test %s", k)
      res <- tests[[k]](capture)
      if ( ! res$passed) {
        capture$passed <- FALSE
        break
      }
    }

    # we must have passed all the tests
    capture
  }
  f
}


#' @rdname compose_tests
#' @aliases branch_test
#' @export
branch_test <- function(condition, yes, no) {
  if (missing(yes)) stop("give a test to conduct if the condition passes")
  if (missing(no)) stop("give a test to conduct if the condition fails")
  f <- function(capture) {
    res <- condition(capture)
    if (res$passed) return(yes(capture))
    else return(no(capture))
    res
  }
  f
}

