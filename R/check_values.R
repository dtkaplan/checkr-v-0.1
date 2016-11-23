#' Check values of objects
#'

# pm is in absolute numbers
# tol is how far the ratio is from 1
# range is a numerical range

#' @export
check_number <- function(x, tol = NULL, pm = 1e-8, range = NULL, diag = FALSE) {
  message <- if(diag) {
    sprintf("should be %s", x)
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
check_class <- function(x, diag = FALSE) {
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
check_data_frame <- function(df, names_contain = TRUE, names_match = FALSE,
                             nrow = FALSE, classes = FALSE, diag = FALSE) {
  connector <- function(m) ifelse(nchar(m) > 0, "and ", "")
  f <- function(val) {
    message <- ""
    if (names_contain) {
      nm <- names(df)
      missing <- nm[ ! nm %in% names(val)]
      if (length(missing) > 0) {
        message <- paste0(connector(message), "missing variables",
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
    return(paste0(ifelse(nchar(message) > 0, "Data frame ", ""),
                  message, ifelse(nchar(message) > 0,  ".", "")))
  }
  f
}
