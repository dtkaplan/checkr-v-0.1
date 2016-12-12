#' Find matching lines based on function calls.
#'
#' A functional form for testing functions and arguments
#' with this, you can set up the test beforehand and pass the code to it later.

#' @param fun_spec character string describing the function to look for
#' and whatever args are to be matched, e.g. "2 + 2".
#' Use \code{whatever} for any arguments for which
#' a place is needed but for which you don't care what the value is.
#' "success" means that the pattern was found.
#' @param message a character string message to be returned on failure. If
#' not provided, a suitable message will be constructed based on \code{fun_spec}.
#' @param mistake if \code{TRUE}, then finding a matching line causes a failure. This is
#' useful for testing for common mistakes.
#' @param hint if \code{TRUE}, signals downstream functions to provide a more diagnostic
#' failure message. (Not used at present.)
#'
#' @return a function that takes a capture object as input and returns
#' a capture argument noting the line that matches (if any).

#' @export
fcall <- function(fun_spec, message = NULL, mistake = FALSE, hint = FALSE) {
  fun_spec <- gsub("_{3,10}", "whatever", fun_spec) # accept ___ as equivalent to <whatever>
  if (is.null(message)) {
    message <-
      if (mistake) sprintf("should not be calling %s", fun_spec)
    else sprintf("couldn't find match to %s", fun_spec)
  }
  reference_call <- (parse(text = fun_spec))[[1]]
  the_fun <- reference_call[[1]]
  f <- function(capture) {
    capture$created_by <- sprintf("a statement like '%s'", fun_spec)
    if ( ! capture$passed) return(capture) # short circuit test
    failed <- FALSE
    for (j in capture$valid_lines) {
      all_calls <- get_functions_in_line(capture$expressions, line = j)
      inds = which(all_calls$fun_names == the_fun)
      for (i in inds) {
        call_to_check <- try(as.call(all_calls$args[[i]]), silent = TRUE)
        if (inherits(call_to_check, "try-error"))
          call_to_check <- as.call(parse(text = as.character(all_calls$args[[i]])))
        result <- corresponding_arguments(call_to_check, reference_call)
        if (length(result$missing) == 0 && length(result$mismatch) == 0) {
          # no problems found
          if (mistake) {
            # but since <mistake> is true, we should fail the test having
            # found the sought-after pattern
            capture$passed <- FALSE
            capture$message <- message
            capture$line <- j
          } else {
            capture$passed <- TRUE
            capture$message <- ""
            capture$line <- j
          }
          return(capture)
        } else if ( ! mistake) {
          failed <- TRUE
        }
      }
    }

    # done with all the lines
    # if we got here, none of the tests passed
    if (mistake) return(capture)
    else {
      capture$passed <- FALSE
      capture$message <- message
    }

    capture
  }
  f
}

# Helper functions to look for matching function-call patterns.

# Get a list of the functions and the arguments on
# a specified line of the expressions

get_functions_in_line <- function(expressions, line) {
  EX <- expressions[[line]]
  # find all names that aren't variables. Keep duplicates.
  N <- all.names(EX)
  keep <- ! N %in% all.vars(EX)
  res <- list(fun_names = N[keep])

  res$args <- walk_tree(EX, res$fun_names)

  res
}


walk_tree <- function(EX, fun_names) {
  # get the function and arguments at the highest level
  if (length(EX) == 1) {
    if (class(EX) == "call") EX <- EX[[1]]
    if (any(EX == fun_names)) res <- as.character(EX)
    else res <- NULL
  } else if (any(EX[[1]] == fun_names)) {
    res <- list(as.list(EX))
    if (length(EX) > 1) res <- c(res, walk_tree(EX[[2]], fun_names))
    if (length(EX) == 3) res <- c(res, walk_tree(EX[[3]], fun_names))
  } else {
    res <- NULL
  }
  res
}
