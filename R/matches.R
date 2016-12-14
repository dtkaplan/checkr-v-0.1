#' Comparison functions
#'
#' There are two steps in using a comparison function. First, call the \code{match_} function itself, setting the
#' argument \code{x} to be the reference value to which you will want the comparison to be made. This
#' will return a function that will actually carry out the comparison. Second, use that returned function
#' as the argument to a locator function like \code{check_value()} or \code{check_argument} or \code{in_values()}.
#'
#'
#' @return a function which, when applied to the object to be tested, carries out the matching
#' test and returns an failure message. (\code{""} means the test passed.)
#' @param x the reference value for the object to be tested
#' @param hint if \code{TRUE}, attempt to give a more diagnostic explanation
#' of mistakes
#' @param pm specify the matching range with a plus-or-minus amount
#' @param tol specify the matching range by the ratio of the discrepancy to x
#' @param range specify the matching range by two numbers
#' @param same_order if \code{TRUE}, insist that the vector elements be in the same order as in the soln.
#' @param names_contain if \code{TRUE} (default) allow the data frame being checked to contain
#' additional variable names than those in \code{x}.
#' @param names_match if \code{TRUE}, all the names in the data frame being checked must be
#' in the reference x. default: \code{FALSE}
#' @param nrow if \code{TRUE}, the number of rows must be the same
#' @param classes if \code{TRUE} the matching data frame variables must be of the same class

#' @rdname matches

#' @export
match_number <- function(x, tol = NULL, pm = 1e-8, range = NULL, hint = FALSE) {
  if(hint) {
    message <- sprintf("should be %s", x)
    if ( ! is.null(range) && length(range) == 2)
      message <- sprintf("should be in range %s to %s", range[1], range[2])
    if ( ! is.null(tol))
      message <- sprintf("should be %s plus or minus %s", x, tol)
  } else {
    message <- sprintf("gives wrong numerical value")
  }

  f <- function(val) {
    if (x == 0) return(ifelse(val == 0, "", message))
    if (length(range) == 2) return(ifelse(val >= min(range) & val <= max(range), "", message))
    if ( ! is.null(tol)) return(ifelse(abs(val / x - 1) < tol, "", message))
    if ( ! is.null(pm)) return(ifelse(abs(x - val) < pm, "", message))
  }
  f
}


#' @rdname matches
#' @export
#'
match_names <- function(x, hint = FALSE) {
  message <- if(hint) sprintf("should have names %s", capture.output(names(x)))
             else "doesn't match required names"
  f <- function(val) {
    result <- all(names(val) %in% names(x)) && all(names(x) %in% names(val))

    if (result) return("")
    else return(message)
  }
  f
}

#' @rdname matches
#' @export
match_class <- function(x, hint = FALSE) {
  message <- if(hint) {
    sprintf("should have class %s", x)
  } else {
    "has wrong class."
  }

  f <- function(val) {
    ifelse(inherits(val, x), "", message)
  }
  f
}


#' @rdname matches
#' @export
match_data_frame <- function(x, names_contain = TRUE, names_match = FALSE,
                             nrow = FALSE, classes = FALSE, hint = FALSE) {
  connector <- function(m) ifelse(nchar(m) > 0, "and ", "")
  f <- function(val) {
    message <- ""
    if (names_contain) {
      nm <- names(x)
      missing <- nm[ ! nm %in% names(val)]
      if (length(missing) > 0) {
        message <- paste0(connector(message), "missing variables",
                          ifelse(hint, " ", ""),
                          ifelse(hint, paste0("'", missing, "'", collapse = ", " ), ""))
      }
    }
    if (names_match) {
      this_test <- length(setdiff(names(val), names(x))) == 0
      if ( ! this_test) {
        nm <- names(val)
        extras <- nm[ ! nm %in% names(x)]
        message <- paste0(connector(message),
                          "extra variables ",
                          ifelse(hint, paste0("'", extras, "'", collapse = ", "), ""))
      }
    }
    if (nrow) {
      this_test <- nrow(x) == nrow(val)
      if ( ! this_test) {
        message <- paste0(message, connector(message),
                          ifelse(hint, sprintf("has %d rows but needs %d rows", nrow(val), nrow(x)),
                                 "has wrong number of rows"))
      }
    }
    if (classes) {
      bad_set <- NULL
      for (nm in names(x)) {
        if(class(val[[nm]]) != class(x[[nm]])) bad_set <- c(bad_set, nm)
      }
      if (length(bad_set) > 0) {
        to_add <- if(hint) {
          paste("variable",
                paste0("'", bad_set, "'", " should be class ",
                       lapply(x[bad_set], FUN = class), collapse = ", "))
        } else {
          paste0("has wrong class for ", paste0("'", bad_set, "'", collapse = ", "))
        }
        message <- paste0(message, connector(message), to_add)
      }
    }
    return(paste0(ifelse(nchar(message) > 0, "data frame ", ""),
                  message, ifelse(nchar(message) > 0,  ".", "")))
  }
  f
}


#' @rdname matches
#' @export
match_formula <- function(x) {
  f <- function(student) {
    res <- f_same_response(student, x)
    if (nchar(res) != 0) return(res)
    res <- f_same_explanatory(student, x)
    if (nchar(res) != 0) return(res)

    return("")
  }
  f
}


#' @rdname matches
#' @export
match_vector <- function(x, same_order = TRUE, hint = FALSE) {
  f <- function(val) {
    if ( ! is.vector(val)) {
      return(paste("should be a vector", ifelse(hint, capture.output(x), "")))
    }
    if (same_order) {
      if (length(val) == length(x) && all(x == val)) return("")

      if (hint) return(sprintf("doesn't match %s", capture.output(x)))
      else return("isn't an exact match")

    }
    missing <- val[ ! val %in% x]
    excess <- x[ ! x %in% val]

    if (length(val) != length(x)) {
      if (hint) {
        return(sprintf("vector should have length %s", nrow(x)))
      } else {
        return(sprintf("vector has wrong length"))
      }
    } else {
      if (length(excess) > 0) res <- paste("has excess elements",
                                           ifelse(hint, capture.output(excess), "" ))
      if (missing(excess) > 0) res <- paste(res, ifelse(nchar(res) > 0, "and", ""),
                                            "is missing elements",
                                            ifelse(hint, capture.output(excess), "" ))
    }
    return(res)

  }
  f
}


# has all the components of the reference formula (and perhaps some others)
is_in_formula <- function(x){
  f <- function(student) {
    res <- ""
    if (length(x) > 2) res <- f_same_response(student, x)
    if (nchar(res) != 0) return(res)

    res <- f_same_explanatory(student, x)
    missing <- attr(res, "missing")
    if (length(missing) > 0)
      return(paste("formula missing", paste0("'", missing, ", ", collapse = "")))
    else return("")
  }
  f
}

