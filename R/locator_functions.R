#' Locator functions
#'
#' \code{find_statement} looks for character matches to the code
#'
#' \code{find_value} looks for a match to the values produced by the code
#'
#' \code{has_names} looks for variables and functions in the expression that match all of those
#' given in \code{pattern}. The \code{pattern} should be an unquoted expression. A good
#' trick if you just want a set of names, e.g. \code{fred}, \code{betty}, \code{sarah}, is to
#' set pattern to a kind of function call: \code{fred(betty, sarah)}.
#'
#' \code{has_constants} looks for numbers and character strings in an expression. (These are different from function names
#' and object names.)
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
#' @seealso other locator functions \code{\link{find_assignment}}, \code{\link{find_call}}
#' @examples
#' U <- capture.code("ifelse(sin(37 + 14) > .5, 'yes', 'no way')")
#' test_1 <- has_constants(37, 14)
#'
#' @rdname locator_functions
#' @export
has_constants <- function(..., message=NULL, mistake = FALSE) {
  constants <- list(...)
  types <- unlist(lapply(constants, FUN = class))
  numerical_values <-
      unlist(as.numeric(constants[types %in% c("integer", "numeric")]))
  string_values <-
      unlist(constants[types %in% c("character")])

  f <- function(capture){
    if (!capture$passed) return(capture) # short circuit the test if a previous test failed

    capture$created_by <- sprintf("looking for constants %s",
                                  paste0("'", unlist(constants), "'", collapse = ", "))
    for (k in capture$valid_lines) {
      token_df <- sourcetools::tokenize_string(capture$statements[[k]])
      strings <- token_df$value[token_df$type == 'string']
      strings <- gsub("^[\"']", "", strings) # kill leading quote
      strings <- gsub("[\"']$", "", strings) # kill trailing quote
      numbers <- as.numeric(token_df$value[token_df$type == 'number'])
      nmatch <- numerical_values %in% numbers
      smatch <- string_values %in% strings
      if (length(nmatch) == 0 && length(smatch) == 0) next
      if (all(nmatch) && all(smatch)) {
        # we found a match
        capture$line <- k
        if (mistake) break
        else return(capture)
      }
    }
    # if we got here, the test failed or the mistake was found
    capture$passed <- FALSE
    capture$message <- message

    capture
  }
  f
}

#' @rdname locator_functions
#' @export
has_names <- function(pattern, message = NULL, mistake = FALSE) {
  EX <- substitute(pattern)
  look_for_names <- all.names(EX)
  f <- function(capture) {
    if (!capture$passed) return(capture) # short circuit the test if a previous test failed

    capture$created_by <- sprintf("looking for names %s",
                                  paste0("'", look_for_names, "'", collapse = ", "))
    for (k in capture$valid_lines) {
      line_names <- all.names(capture$expressions[[k]])
      if (all(look_for_names %in% line_names)) {
        # we found a match
        capture$line <- k
        if (mistake) break
        else return(capture)
      }
    }
    # if we got here, the test failed or the mistake was found
    capture$passed <- FALSE
    capture$message <- message

    capture
  }
  f
}


#' @rdname locator_functions
#' @export
find_statement <- function(pattern, message = NULL, regex = FALSE, mistake = FALSE ) {
  f <- function(capture) {
    if (!capture$passed) return(capture) # short circuit the test if a previous test failed

    capture$created_by <- sprintf("find_statement(%s)", pattern)
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
find_value <- function(x, message = NULL) {
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

