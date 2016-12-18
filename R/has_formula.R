#' Look for a formula in a command line and apply a test to the formula
#'
#' @param test a test like \code{match_formula()}. Default: Does the formula exist?
#' @param message character string message to give on failure
#' @seealso \code{\link{check_value}}, \code{\link{check_argument}}
#'
#'
#' @export
has_formula <- function(test = NULL, message = "couldn't find a formula") {
  if (is.null(test)) test <-
      function(x) ifelse(inherits(x, "formula"), "", message)
  f <- function(capture) {
    if ( ! capture$passed) return(capture) # short circuit the test
    capture$created_by <- "a statement with a formula"
    for (j in capture$valid_lines) {
      formulas <- formulas_in_expression(capture$expressions[j])
      if (length(formulas) > 0) {
        for (k in 1:length(formulas)) { # there might be more than one
          # any one that passes is good enough
          passed <- test(formulas[[k]])
          if (nchar(passed) == 0) { # test passed
            capture$passed <- TRUE
            capture$line <- j
            return(capture)
          }
        }
      }
    }
    # if we got here, the test failed
    capture$passed <- FALSE
    capture$message <- message

    capture
  }
  f
}

# return a list of the formulas in an expression
formulas_in_expression <- function(expression) {
  by_args <- get_functions_in_line(expression, 1)
  if (! "~" %in% by_args$fun_names) return(NULL) # nothing found

  get_formulas(by_args$args) # return the list of formulas.
}

get_formulas <- function(args) {
  count <- 0
  res <- list()
  for (k in 1:length(args)) {
    if (args[[k]][[1]] == as.name("~")) {
      count <- count + 1
      res <- c(res, eval(as.call(args[[k]])) )
    }
  }

  res
}

