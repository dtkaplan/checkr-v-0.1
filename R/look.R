#' Look for the line, if any, containing the sought-for object.
#'
#' @title look for matches in the code
#'
#' @description Just the first such line is reported.
#'
#' @param where character string specifying whether to look at names of objects
#' assigned names, objects
#' produced by the commands (and potentially not named),
#' the statements themselves. NOT SURE WHAT ROLE commands will play.  Those are the
#' expressions as expressions.
#' @param what character string, regex, class name, number range, or name assigned
#' that we're looking for
#' @param mode what kind of thing to search for.
#' @param mistake to indicate that, to pass,
#' the pattern should not be found. Can be used to check for common
#' mistakes.
#'
#' @return A list with 3 elements: \code{passed} indicating TRUE/FALSE whether the
#' test was passed; \code{line} the line number where the item was found; \code{code}
#' the same as the input.
#' @export
find_content <- function(where = c("returns", "names", "statements", "commands"),
                         what, mode = c("class", "number", "match", "regex"),
                         message = "Test writer: give a meaningful fail message",
                         mistake = FALSE, ...) {
  # search among the results for a match
  where <- match.arg(where)
  mode <- match.arg(mode)
  # If looking for an exact match, parse the "what" in order
  # to be able to compare it to the captured code, which will have been parsed
  if (mode == "match") {
    tmp <- try(as.character(parse(text = what)), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      tmp <- what
      stop(sprintf("String '%s' for matching is not parsable", what))
    } else {
      what <- tmp
    }

  }
  pre_process <- ifelse(where == "names",
                        ls,
                        ifelse(where %in% c("match", "regex"),
                               capture.output,
                               I)
  )
  if (mode == "number" && ! (length(what) == 2 || is.numeric(what)) )
    stop("Specify a range of 2 numbers.")
  success <- !mistake
  success_message <- ifelse(mistake, message, "")
  fail_message <- ifelse(mistake, "", message)
  f <- function(capture) {
    capture$created_by <-
      if (where == "names") sprintf("that creates an object named '%s'.", what)
      else if (where == "returns") sprintf("that returns an object like '%s'.", what)
      else if (where %in% c("statements", "commands")) sprintf("that contains a command like '%s'.", what)
      else "this is a meaningless created_by message. Shouldn't be happening."

    if ( ! capture$pass) return(capture) # short circuit if non-passing input
    success_flag <- FALSE
    found_in_line <- NA
    search_in <- capture[[where]]
    for (k in capture$valid_lines) {
      content <- pre_process(search_in[[k]])
      if (mode == "class") {
        if (inherits(content, what)) {
          success_flag <- TRUE; found_in_line <- k; break
        }
      } else if (mode %in% c("match", "regex") ) {
        if (grepl(what, content, fixed = mode == "match")){
          success_flag <- TRUE; found_in_line <- k; break
        }
      } else if (mode == "number") {
        if (is.numeric(content) &&
            content >= min(what) &&
            content <= max(what)) {
          success_flag <- TRUE; found_in_line <- k; break
        }
      }
    }
    capture$line <- found_in_line
    if (success_flag) {
      capture$passed = success
      capture$message = success_message
    } else {
      capture$passed = ! success
      capture$message = fail_message
    }

    capture
  }

  f # return the function created
}

# These are factories for test functions. Test functions take either a result or the
# output of capture.output() as inputs. Test functions always return a result.
# a result has fields <passed>, <message>, <code>, <lines>. The output of capture.code

in_factory <- function(where) {
  f <- function(what, message = NULL, regex = NULL, number = NULL, class = NULL, mistake = FALSE, ...) {
    mode = "match"
    if (!is.null(regex)) {
      mode <- "regex"
      what <- regex
    }
    if (!is.null(number)) {
      mode <- "number"
      what <- number
    }
    if (!is.null(class)) {
      mode <- "class"
      what <- class
    }
    if (is.null(message)) {
      message <-
        sprintf("couldn't find match to %s'%s'",
                ifelse(mode == "number", "number ",
                       ifelse(mode == "class", "class ",
                       ifelse(mode == "regex", "regex ", ""))),
                as.character(what))
    }

    find_content(where = where, mode = mode, what = what,
                 message = message, regex = regex, number = number, class = class, ...)
  }
  f
}
#' @export
in_names <- in_factory("names")


# If the argument to <in_statements()> is a fixed string, it must be parsable.
# Why? because comparison will be done to the parsed version of the lines.
#' @export
in_statements <- in_factory("statements")
#' @export
in_values <- in_factory("returns")
#' @export
final_ <- function(...) {
  simple <- function(capture) {
    capture$created_by <- "last result from code"
    capture$line <- max(capture$valid_lines)

    capture
  }
  dots <- lazyeval::lazy_dots(...)
  if (length(dots) == 0) return(simple)
}

# search for a value in the solution code. Then, when called again
# on the student code, look for a value similar to this one. So long as there is some
# value in the student code, no matter how different from the solution code's value,
# a value will be returned.
# @param what either an object name or an identifying character string from the code generating
# the object
#' @export
closest_to <- function(what) {
  what <- substitute(what)
  if (is.character(what)) {
    # Look for a literal string in the captured code. <what> must be parsable so that
    # it can be matched to the captured code, which has been parsed.
    tmp <- try(as.character(parse(text = what)), silent = TRUE)
    if (inherits(tmp, "try-error"))
      stop(sprintf("Argument to closest_to(), '%s', is not a complete parsable statement.", what))
  }
  solution_value = NULL # used when student code is evaluated
  f <- function(capture) {

    if (is.null(solution_value)) {
      # perform the match and extract the value
      capture <-
        if(is.name(what)) {
          in_names(as.character(what))(capture)
        } else {
          in_statements(as.character(what))(capture)
        }
      solution_value <<- capture$returns[[capture$line]] # store the result
    } else {
      # find the closest matching in the capture presented
      capture$line <- find_the_closest_one(capture, solution_value)
    }
    capture
  }
  f
}

# find the closest returns in a capture to the given value
find_the_closest_one <- function(capture, val) {
  if ((!"returns" %in% names(capture)) || (length(capture$returns) == 0))
      return(NULL)
  best_score <- -Inf
  best_i <- 1
  for (k in seq_along(capture$returns)) {
    score <- 0
    X <- capture$returns[[k]]
    # class
    if (inherits(X, class(val))) score <- score + 5
    if (is.list(X) == is.list(val) ||
        is.vector(X) == is.vector(val) ||
        is.matrix(X) == is.matrix(val)) {
      score <- score + 2
      if (class(X[[1]]) == class(val[[1]])) score <- score + 2
      if (isTRUE(all.equal(X[[1]], val[[1]]))) score <- score + 3
    }
    # named elements
    obj_names <- names(val)
    if (length(obj_names) > 0) {
      score <- score + sum(names(X) %in% obj_names)
    }
    # same length result
    obj_length <- length(val)
    diff_lengths <- obj_length - length(X)
    if (diff_lengths == 0) score <- score + 3

    if (score > best_score) {
      best_score <- score
      best_i <- k
    }
  }

  best_i
}

#'
# a functional form for testing functions and arguments
# with this, you can set up the test beforehand and pass the code to it later.

# look like, e.g. "2 + 2". Use NULL for any arguments for which
# a place is needed but for which you don't care what the value is.
# "success" means that the pattern was found
# @param fun_spec character string describing the function to look for
# and whatever args are to be matched.
#' @export
fcall <- function(fun_spec, message = NULL, mistake = FALSE, diag = FALSE) {
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
