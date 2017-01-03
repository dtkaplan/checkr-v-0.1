#' Pre-evaluation checking of code
#'
#' These functions support checking of user code before it is evaluated. This is
#' useful when the built in parsing error messages from R are not gentle enough for
#' beginning users.
#'
#' \code{check_function_calls()} checks whether functions are called with opening and closing parentheses.
#' You need to provide a list of the functions it should look for (since no parens are needed for
#' object names).
#'
#' \code{check_assignment_names()} checks whether assignments are made to a legal name (or other
#' syntactically legal construction).
#'
#' @return a character string. All pre-check functions return a character string (for now).
#' If the string has length 0, the check passes. Non-zero length strings should be
#' diagnostic messages or hints for users.
#'
#' @rdname pre-evaluation
#' @param user_code a character string containing the user's code
#' @param fnames a character string vector of function names to check
#' @export
check_function_calls <- function(user_code, fnames) {
  message <- ""
  for (fun in fnames) {
    no_start_paren <- paste0(fun, " ?[^\\(]")
    start_paren <- paste0(fun, " ?\\(")
    close_paren <- paste0(fun, " ?\\(.*\\)")
    if (grepl(no_start_paren, user_code)) {
      message <-
        sprintf("Function %s (like all functions) must be followed by parentheses, e.g.  `%s(arguments)`.", fun, fun)
      return(message)
    }
    lines <- grep(start_paren, user_code)
    # there was an opening paren. Was there a closing one?
    closed_lines <- grep(close_paren, user_code[lines])
    if (length(closed_lines) == 0 ) {
      message <- sprintf("What about the closing parenthesis for `%s( )`?", fun)
    }
  }
  return(message)

}

#' @rdname pre-evaluation
#' @export
check_assignment_names <- function(user_code) {
  message <- ""
  user_code <- unlist(strsplit(user_code, "[\n;]"))
  lines_with_assignment <- grep("^.*<-", user_code)
  if (length(lines_with_assignment) > 0) {
    names <- gsub("^(.*)<-.*$", "\\1", user_code[lines_with_assignment])
    for (k in seq_along(names)) {
      res <- try(parse(text = names[k]), silent = TRUE)
      if (inherits(res, "try-error")) {
        message <- sprintf("`%s` is an illegal name for an object.", names[k])
        return(message)
      }
    }
  }

  message
}


