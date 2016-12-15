#' Locator functions
#'
#' \code{final_} finds the last command line in the code
#' No arguments need be given. Last is last!
#'
#' \code{in_statements} looks for character matches to the code
#'
#' \code{in_values} looks for a match to the values produced by the code
#'
#' \code{similar_names} takes a character string containing an R expression.
#' It looks for the statement in the submitted code that has a set of names
#' that most closely matches those in the character string. It never fails, always
#' returning a capture object pointing to some statement. However, the heuristic
#' used for comparison is just a heuristic!
#'
#' @param x the target that you are looking for in the values.
#' Specify the match in terms of a comparison function
#' @param pattern character string containing the pattern to search for in the statements
#' @param message character string giving the failure message
#' @param regex if \code{TRUE} treat the pattern as a regex
#' @param mistake if \code{TRUE}, the test fails if the pattern is located
#' @param ... placeholder for future possible arguments
#'
#' @seealso other locator functions \code{\link{assigns_to}}, \code{\link{fcall}}
#' @rdname locator_functions
#' @export
final_ <- function(...) {
  simple <- function(capture) {
    capture$created_by <- "last result from code"
    capture$line <- max(capture$valid_lines)

    capture
  }
  dots <- lazyeval::lazy_dots(...)
  if (length(dots) == 0) return(simple)
}

#' @rdname locator_functions
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

#' @rdname locator_functions
#' @export
similar_names <- function(x, message = NULL) {
  if (is.character(x)) {
    x <- try(parse(text = x))
  }
  the.names <- all.names(x)
  f <- function(capture){
    if (!capture$passed) return(capture) # short circuit the test if a previous test failed

    best_n <- -Inf
    best_k <- max(capture$valid_lines) # return something!
    for (k in capture$valid_lines) {
      expression.names <- all.names(capture$expressions[[k]])
      ns <- sum(the.names %in% expression.names)
      if (ns > best_n) {
        best_n <- ns
        best_k <- k
      }
    }
    capture$line <- best_k

    capture
  }
  f
}


#' @rdname locator_functions
#' @export
#'
in_values <- function(x, message = NULL) {
  # test will be a comparison test
  if (is.function(x)) {
    test <- x
  } else {
    # simple comparison to the value
    # construct a function that tests for this
    test <- function(val) {
      res <- all.equal(val, x)
      if (isTRUE(res)) return("")
      else return(sprintf("no value created matching %s", x))
    }
  }
  f <- function(capture) {
    if (!capture$passed) return(capture) # short circuit the test if a previous test failed

    for (k in capture$valid_lines) {
      res <- test(capture$returns[[k]])
      if (nchar(res) == 0) { # we found a match
        capture$line <- k
        return(capture)
      }
    }

    # if we get here, there was no match
    capture$passed <- FALSE
    capture$message <- res
    capture$line <- NA

    capture
  }
  f
}

