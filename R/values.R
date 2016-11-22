#' Checking values of objects created or displayed by the code
#'
#' These functions capture the object itself. If there is no such object, a character
#' string with the message is returned. That message will have a 'no_match' attribute
#'
#' If the test passes, the value is returned in the "value" attribute of the capture object
#'
# #' @param capture the code object
# #' @param what the object name, class(es), function/argument string, or text to match
# #' @param strict whether the matching name or text can be approximate
# #' (e.g. capitalization, dots, underscores, ...)
#
# # The user interface: create an object, run the test assigning the result
# # to a named object, and, if passed, you can access the value by attr(obj, "value")
# #' @export
# val_by_name <- function(what = what, message = "Give a real message", strict = TRUE ) {
#   val_by_factory(what = what, message = message, strict = strict, get_mode = get_val_by_name)
# }
#
# #' @export
# val_by_class <- function(what = what, message = "Give a real message", strict = TRUE ) {
#   val_by_factory(what = what, message = message, strict = strict, get_mode = get_val_by_class)
# }
#
# #' @export
# val_by_text_match <- function(what = what, message = "Give a real message", strict = TRUE ) {
#   val_by_factory(what = what, message = message, strict = strict, get_mode = get_val_by_text_match)
# }
#
# #' @export
# val_by_argument <- function(what = what, message = "Give a real message", strict = TRUE ) {
#   val_by_factory(what = what, message = message, strict = strict, get_mode = get_val_by_argument)
# }
#
# val_by_factory <- function(what, message, strict, get_mode) {
#   f <- function(capture) {
#     # val_by_name
#     res <- get_mode(capture, what = what, strict = strict, message = message)
#     if (is_match(res)) { # Null message, so found the sought-after item
#       attr(capture, "value") <- res
#     } else {
#       capture$passed <- FALSE
#       capture$message <- message
#     }
#
#     capture
#   }
#
#   f # function to return
# }

#' @param capture a capture object
#' @param test the test to run to evaluate the captured value
#' @param message the message to give if the test fails
#' @export
check_value <- function(test, message = NULL, mistake = FALSE) {
  test_text <- deparse(substitute(test))
  if (is.null(message))
    message <- ifelse(mistake,
      sprintf("test '%s' shouldn't pass", text_text),
      sprintf("test '%s' failed", test_text))
  success_message <- ! mistake # if the mistaken test is passed, the test fails
  f <- function(capture) {
    if ( ! capture$pass) return(capture) # non-passing input given, so don't do test

    if (is.na(capture$line) || (! capture$line %in% capture$valid_lines)) {
      stop("Using check_value() without a valid capture$line. Give a preliminary test to identify the line who's value is sought.")
    }
    value <- capture$returns[[capture$line]]
    result <- test(value)
    if ((mistake && result) || !result) {
      # either the mistaken pattern was found, so the test should fail
      # or the pattern was not found when it should have been (mistake == TRUE) and
      # so the test failed
      capture$passed = FALSE
      capture$message = paste(message, sprintf("for value of line '%s'", capture$statements[capture$line]))
    }

    capture
  }
  f
}
# check_value <- function(capture, test, message = "Give a helpful message!") {
#   if ( ! capture$pass) return(capture) # non-passing input given, so don't do test
#
#   val <- attr(capture, "value")
#   if (is.null(val)) {
#     capture$message <- "No value captured"
#     capture$pass <- FALSE
#   } else {
#     passing <- test(val)
#     if ( ! passing) {
#       capture$pass <- FALSE
#       capture$message <- message
#     }
#   }
#
#   capture
# }

# #' @export
# get_value <- function(capture) {
#   capture$R[[capture$line]]
# }

#' @export
# check_numeric_result <- function(value, message, ...) {
#   f <- function(capture) {
#     if ( ! capture$passed) return(capture)
#     for (k in capture$valid_lines) {
#       res <- check_number(value, ...)
#       if (res == "") {
#         capture$line <- k
#         return(capture)
#       }
#     }
#     capture$passed <- FALSE
#     return(capture)
#   }
# }


# get_val_by_name <- function(capture, what, strict = TRUE, message = "Give a real message") {
#   res <- no_match(message) # prepare not to find it
#   for (k in capture$valid_lines) {
#     nms <- ls(capture$names[[k]])
#     match_ind <- get_match_ind(what, nms, strict = strict)
#     if (length(match_ind) > 0) {
#       # multiple matches? Get just the first in this environment
#       # There can't be multiple matches in an environment if strict = TRUE
#       val <- get(nms[match_ind[1]], capture$names[[k]])
#       return(val) # value of the object that matched
#     }
#   }
#   return(res) # no match
# }


# THIS NEEDS TO BE FOLDED INTO in_names()

# but doesn't need to be exported
get_match_ind <- function(what, nms, strict = TRUE) {
  if (strict) {
    return(which(nms == what))
  } else {
    # change everything to lower case
    # remove dots and underscores from names
    what <- tolower(gsub("[\\.|_]*", "", what))
    nms <- tolower(gsub("[\\.|_]*", "", nms))
    return(get_match_ind(what, nms, strict = TRUE))
  }
}


# get_val_by_class <- function(capture, what, strict = TRUE, message = "Give a real message") {
#
#   for (k in capture$valid_lines) {
#     nms <- ls(capture$names[[k]])
#     for (j in seq_along(nms)) {
#       val <- get(nms[j], capture$names[[k]])
#       if (inherits(val, what)) return(val)
#     }
#   }
#   return(no_match(message)) # no match found
# }



get_val_by_argument <- function(capture, what, strict = TRUE, message = "Give a real message") {
  # get the object produced by the first line of the command that matches
stop("Not yet implemented")
}


# get_val_by_text_match <- function(capture, what, strict = TRUE, message = "Give a real message") {
#   # get the object produced by the first line of the command that matches
#   stop("Not yet implemented")
# }
#
# # create a no-match object
# no_match <- function(message) {
#   res <- message
#   attr(res, "no_match") <- TRUE
#
#   res
# }
#
# # test for a no-match object, returning
# # the message string if it's no match
# # or "" if there is a match.
# is_match <- function(res) {
#   is.null(attr(res, "no_match"))
# }


