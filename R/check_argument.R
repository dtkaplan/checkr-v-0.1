#' Apply a test to the value of an argument
#'
#' When previous locator tests have identified a matching line, this function can be used
#' to check an argument to a function in that line.
#'
#' @param arg_spec character string describing what we want to match, e.g. \code{"sin(grab_this)"}. There must always be a
#' \code{grab_this} in \code{arg_spec}.
#' @param test is a function that takes the concordance produced by <arg_spec>
#' and returns a message saying what's wrong. If that message is "", the test passes.
#' Works on a single line, that identified by the previous locator test.
#' @return a capture object
#' #' @seealso \code{\link{check_value}}

#' @export
check_argument <- function(arg_spec, test) {
  f <- function(capture) {
    expanded <- as.list(parse(text = arg_spec)[[1]])
    message <- sprintf("couldn't find match to %s", arg_spec)
    the_fun <- expanded[[1]]
    if ( ! capture$passed) return(capture) # short circuit the test
    if ( ! capture$line %in% capture$valid_lines)
      stop("Test designer should specify a previous test that finds the line to examine.")
    all_calls <- get_functions_in_line(capture$expressions, line = capture$line)
    inds = which(all_calls$fun_names == the_fun)
    for (j in inds) {
      # call_to_check <- as.call(parse(text = as.character(all_calls$args[[j]])))
      call_to_check <- as.call(all_calls$args[[j]])
      result <- corresponding_arguments(call_to_check, expanded)
      for (i in seq_along(result$grabbed)) {
        message <- test(eval(result$grabbed[[i]], envir = capture$names[[capture$line]]))
        if(message == "") { # it passed, so no need to check others
          capture$passed <- TRUE
          capture$message <- message
          return(capture)
        }
      }
    }
    # None of the matches passed the test
    capture$passed <- FALSE
    capture$line <- NA
    call_text <- gsub("grab_this", "_____", as.character(arg_spec))
    capture$message <- paste("in function call",
                             sprintf("%s", call_text),
                             "the argument",
                             message)

    capture
  }
  f
}
