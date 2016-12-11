#' Compare pairs of values
#'
#' Or, more precisely, create functions that will compare the values
#' of identically named objects in two environments or lists. Typically,
#' one of the environments will be from the submitted answers, the other will
#' be from the official solution.
#'
#' @return a list of with class \code{"test_result"}. See \code{new_test_result()}.
#'
#' @details \code{same_()} is a general template for constructing comparison functions
#'
#' \code{same_num()} compares numerical vectors, providing a tolerance for numerical
#' disagreement, \code{same_vec()} compares vectors for exact equality.
#'
#' @param compare_fun the function to compare two objects, e.g. compare_numbers, compare_classes
#' @param name the name of the object. Should be an unquoted name (e.g. \code{b}),
#' not a character string (e.g., not \code{"b"}). Expressions
#' are also allowed, e.g. \code{sqrt(b)}
#' @param hint Set to \code{TRUE} to provide more diagnostic messages to the student.
#' @param ... additional parameters to describe the match
#' @export
same_ <- function(compare_fun, objname, hint = FALSE, ...) {
  if (!is.function(compare_fun)) stop("arg 'compare_fun' must be a function.")
  # name <- substitute(name) # for dealing with expressions
  Ldots <- lazyeval::lazy_dots(...)
  f <- function(submitted, reference) {
    res <- in_both(objname, submitted, reference)
    if (!res$passed) return(res)

    S <- try(eval(objname, envir = submitted), silent = TRUE)
    if (inherits(S, "try-error")) S <- "nothing for you"
    R <- eval(objname, envir = reference)
    if (length(S) == 1 && S == "no such command found") message <- "no corresponding command found"
    else message <- compare_fun(S, R, hint = hint, ...)
    if (nchar(message)) {
      res$message <- message
      res$passed <- FALSE
    }
    res
  }
}

#' @export
same_vec <- function(name, hint = FALSE, ...) {
  same_(compare_vectors, substitute(name), hint = hint, ...)
}


#' @export
same_num <- function(name, hint = FALSE, ...) {
  name <- substitute(name)
  Ldots <- lazyeval::lazy_dots(...)
  f <- function(submitted, reference) {
    res <- in_both(name, submitted, reference)
    if (!res$passed) return(res)

    S <- try(eval(name, envir = submitted), silent = TRUE)
    if (inherits(S, "try-error")) S <- "nothing for you"
    R <- eval(name, envir = reference)
    if (length(S) == 1 && S == "no such command found") message <- "no corresponding command found"
    else message <- compare_numbers(S, R, hint = hint, ...)
    if (nchar(message)) {
      res$message <- message
      res$passed <- FALSE
    }
    res
  }
  f
}


# internal comparison functions

compare_classes <- function(S, R, hint = FALSE, all = TRUE) {
  cs <- class(S)
  cr <- class(R)
  passed <-
    if (all) {
      length(setdiff(cs, cr)) == 0
    } else {
      any(cs %in% cr)
    }
  message <-
    if (passed) {
      ""
    } else if (hint) {
      paste('should have class', paste0("'", cr, "'", collapse = ", "))
    } else {
      "has wrong class"
    }

  message
}

compare_length <- function(S, R, hint = FALSE) {

  message <-
    if (length(S) == length(R)) ""
    else if (hint) {
      sprintf("should have length %d", length(R))
    } else {
      "has wrong length"
    }

  message
}

# exact equality. See compare_numbers() for numerical comparison

compare_vectors <- function(S, R, same_order = TRUE, hint = FALSE) {
  if ( ! is.vector(S)) {
    return(paste("should be a vector", ifelse(hint, capture.output(R), "")))
  }
  message <- compare_length(S, R, hint = hint)
  if (nchar(message)) return(message)

  if (same_order) {
    message <-
      if (all(S == R)) ""
    else if (hint) sprintf("doesn't match %s", capture.output(R))
    else "isn't an exact match"
  } else {
    missing <- S[ ! S %in% R]
    excess <- R[ ! R %in% S]

    if (length(excess) > 0)
      message <- paste("has excess elements",
                       ifelse(hint, capture.output(excess), "" ))
    if (missing(excess) > 0)
      message <- paste(message, ifelse(nchar(res) > 0, "and", ""),
                       "is missing elements",
                       ifelse(hint, capture.output(missing), "" ))
  }

  message
}

compare_numbers <- function(S, R, tol = NULL, pm = 1e-8, hint = FALSE) {
  message <- compare_length(S, R, hint = hint)
  if (nchar(message)) return(message)
  message <-
    if (hint) {
      if ( ! is.null(tol)) sprintf("should be %s", paste0(R, collapse = " "))
      else sprintf(
        ifelse(length(R) > 1,
               "should be [%s] plus or minus %s",
               "should be %s plus or minus %s"),
        paste0(R, collapse = " "), pm)
    } else {
      "gives wrong numerical value"
    }

  if ( ! is.null(tol)) {
    # deal with division by zero
    is_same <- ifelse(R == 0, abs(S) < tol, abs(S / R - 1) < tol)
    if (!all(same)) return(message)
  } else if (!all(abs(S - R) < pm)) {
    return(message)
  }

  "" # if we got here, we passed the test
}

# helper functions to create a test_result object

new_test_result <- function() {
  res <- list(passed = TRUE,   # was the test passed
              stop = FALSE,    # signal to stop if this test was not passed
              message = "",    # diagnostic message to student
              suffice = FALSE, # signal to stop if this test passes
              has_value = FALSE, # is there an object stored here?
              value = NULL)    # the object being stored
  class(res) <- "test_result"

  res
}

# check if it's a test_result object
#' @export
is.test_result <- function(obj) inherits(obj, "test_result")

in_both <- function(name, sub, ref) {
  res <- new_test_result()
  in_sub <- all(all.vars(name) %in% names(sub))
  in_ref <- all(all.vars(name) %in% names(ref))
  if (in_sub && in_ref) return(res)  # success!

  # This message will refer to the object created in match_values(), so
  # probably not useful to the student, but can help the person writing the statements.
  m1 <- ifelse(in_sub, "", sprintf("can't find object '%s' in submitted code", all.vars(name)))
  m2 <- ifelse(in_ref, "", sprintf("no object '%s' in solution code", all.vars(name)))
  link <- ifelse(nchar(m1) && nchar(m2), " and ", "")
  res$message <- paste0(m1, link, m2)

  res # return the test object
}


# modifiers on pass objects
#' @export
needs <- function(pass) {
  if (!pass$pass) pass$stop <- TRUE
  pass
}
#' @export
suffices <- function(pass) {
  pass$suffices <- TRUE
}

