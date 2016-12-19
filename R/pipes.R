#' Locator and check functions relating to pipes
#'
#' @param pattern a character string or complete expression starting the pipe.
#' @param message character string failure message
#' @rdname pipe_tests
#' @export
find_pipe_start <- function(pattern = NULL, message = "no %>% pipe found.") {
  pattern <- as.character(substitute(pattern))
  f <- function(capture){
    # short circuit the test
    if (! capture$passed) return(capture)
    capture$created_by <- sprintf("%s at start of pipe ", pattern)
    for (k in capture$valid_lines) {
      if ("..tmp1.." %in% ls(capture$names[[k]], all.names = TRUE)) {
        # make sure there is one starting space at the start and $ at end
        # to ensure the match is not of a substring.
        pattern <- paste0(" ", gsub("^ +", "", pattern))
        if ((nchar(pattern)==0) || any(grepl(pattern, capture$statements[[k]], fixed = TRUE))) {
          capture$passed <- TRUE
          capture$line <- k
          return(capture)
        }
      }
    }
    # if we got here, the test failed
    capture$passed <- FALSE

    capture
  }
  f
}

#' @rdname pipe_tests
#' @export

check_pipe_input <- function(test, message = "") {
  pipe_input_output(test = test, message = message, offset = 1)
}

#' @rdname pipe_tests
#' @export
check_pipe_output <- function(test, message = "") {
  pipe_input_output(test = test, message = message, offset = 0)
}

# helper function
pipe_input_output <- function(test, message = "", offset = 1) {
  # offset = 1 means look to the previous line, that is, the input to this step
  # offset = 0 means look to the current line, that is, the output from this step
  if (! is.function(test)) stop("Must provide function for argument test.")
  in_or_out <- ifelse(offset == 1, "input", "output")
  f <- function(capture) {
    if (! capture$passed) return(capture) # short circuit on failed input
    capture$created_by <- sprintf("grabbing %s to a pipeline stage.", in_or_out)
    line_names <- ls(capture$names[[capture$line - offset]], all.names = TRUE)
    inds <- grep("^\\.\\.tmp[0-9]*\\.\\.$", line_names)
    if (length(inds) > 0) {
      input_name <- line_names[inds[1]]
      res <- test(get(input_name, capture$names[[capture$line]]))
      if (nchar(res)) { # non-null message from test
        capture$passed <- FALSE
        capture$message <- ifelse(nchar(message), message, res)
        return(capture)
      } else {  # test was passed
        capture$passed <- TRUE
        return(capture)
      }
    } else {
      capture$passed <- FALSE
      capture$message <- sprintf("failed to find %s to %s", in_or_out, capture$statements[[capture$line]])
    }

    capture
  }
  f
}
