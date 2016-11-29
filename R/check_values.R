#' Check values of objects
#'

# pm is in absolute numbers
# tol is how far the ratio is from 1
# range is a numerical range

#' @export
match_number <- function(x, tol = NULL, pm = 1e-8, range = NULL, diag = FALSE) {
  message <- sprintf("should be %s", x)
  if(diag) {
    if ( ! is.null(range) && length(range) == 2)
      message <- sprintf("should be in range %s to %s", range[1], range[2])
    if ( ! is.null(tol))
      message <- sprintf("should be %s plus or minus %s", x, tol)
  } else {
    sprintf("has wrong numerical value")
  }

  f <- function(val) {
    if (x == 0) return(ifelse(val == 0, "", message))
    if (length(range) == 2) return(ifelse(val >= min(range) & val <= max(range), "", message))
    if ( ! is.null(tol)) return(ifelse(abs(val / x - 1) < tol, "", message))
    if ( ! is.null(pm)) return(ifelse(abs(x - val) < pm, "", message))
  }
  f
}

#' @export

match_vector <- function(x, same_order = TRUE, diag = FALSE) {
  f <- function(val) {
    if ( ! is.vector(val)) {
      return(paste("should be a vector", ifelse(diag, capture.output(x), "")))
    }
    if (same_order) {
      if (length(val) == length(x) && all(x == val)) return("")

      if (diag) return(sprintf("doesn't match %s", capture.output(x)))
      else return("isn't an exact match")

    }
    missing <- val[ ! val %in% x]
    excess <- x[ ! x %in% val]

    if (length(val) != length(x)) {
      if (diag) {
        return(sprintf("vector should have length %s", nrow(x)))
      } else {
        return(sprintf("vector has wrong length"))
      }
    } else {
      if (length(excess) > 0) res <- paste("has excess elements",
                                           ifelse(diag, capture.output(excess), "" ))
      if (missing(excess) > 0) res <- paste(res, ifelse(nchar(res) > 0, "and", ""),
                                     "is missing elements",
                                     ifelse(diag, capture.output(excess), "" ))
    }
    return(res)

  }
  f
}

# @param x a list or vector with names
#' @export
#'
match_names <- function(x, diag = FALSE) {
  message <- if(diag) sprintf("should have names %s", capture.output(names(x)))
             else "doesn't match required names"
  f <- function(val) {
    result <- all(names(val) %in% names(x)) && all(names(x) %in% names(val))

    if (result) return("")
    else return(message)
  }
  f
}

#' @export
match_class <- function(x, diag = FALSE) {
  message <- if(diag) {
    sprintf("should have class %s", x)
  } else {
    "has wrong class."
  }

  f <- function(val) {
    ifelse(inherits(val, x), "", message)
  }
  f
}

#' @export
match_formula <- function(answer) {
  f <- function(student) {
    res <- f_same_response(student, answer)
    if (nchar(res) != 0) return(res)
    res <- f_same_explanatory(student, answer)
    if (nchar(res) != 0) return(res)

    return("")
  }
  f
}

#' @export
is_in_formula <- function(student, answer) {
  res <- ""
  if (length(answer) > 2) res <- f_same_response(student, answer)
  if (nchar(res) != 0) return(res)

  res <- f_same_explanatory(student, answer)
  missing <- attr(res, "missing")
  if (length(missing) > 0)
    return(paste("formula missing", paste0("'", missing, ", ", collapse = "")))
  else return("")
}

# @param names_contain if \code{TRUE} (default), all the names in df must be in the
# data frame being checked
# @parm names_match if \code{TRUE}, all the names in the data frame being checked must be
# in the reference df. default: \code{FALSE}
# @param nrow if \code{TRUE}, the number of rows must be the same
# @param classes if \code{TRUE} the matching variables must be of the same class
# @param diag if \code{TRUE} give a more diagnostic message about mis-matches.
#' @export
match_data_frame <- function(df, names_contain = TRUE, names_match = FALSE,
                             nrow = FALSE, classes = FALSE, diag = FALSE) {
  connector <- function(m) ifelse(nchar(m) > 0, "and ", "")
  f <- function(val) {
    message <- ""
    if (names_contain) {
      nm <- names(df)
      missing <- nm[ ! nm %in% names(val)]
      if (length(missing) > 0) {
        message <- paste0(connector(message), "missing variables",
                          ifelse(diag, " ", ""),
                         ifelse(diag, paste0("'", missing, "'", collapse = ", " ), ""))
      }
    }
    if (names_match) {
      this_test <- length(setdiff(names(val), names(df))) == 0
      if ( ! this_test) {
        nm <- names(val)
        extras <- nm[ ! nm %in% names(df)]
        message <- paste0(connector(message),
                         "extra variables ",
                         ifelse(diag, paste0("'", extras, "'", collapse = ", "), ""))
      }
    }
    if (nrow) {
      this_test <- nrow(df) == nrow(val)
      if ( ! this_test) {
        message <- paste0(message, connector(message),
                         ifelse(diag, sprintf("has %d rows but needs %d rows", nrow(val), nrow(df)),
                                "has wrong number of rows"))
      }
    }
    if (classes) {
      bad_set <- NULL
      for (nm in names(df)) {
        if(class(val[[nm]]) != class(df[[nm]])) bad_set <- c(bad_set, nm)
      }
      if (length(bad_set) > 0) {
        to_add <- if(diag) {
          paste("variable",
            paste0("'", bad_set, "'", " should be class ",
                   lapply(df[bad_set], FUN = class), collapse = ", "))
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
